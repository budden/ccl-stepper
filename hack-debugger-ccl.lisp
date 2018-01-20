;;; -*- coding: utf-8; -*-

(eval-when (:execute)
  (error "Use compile-load sequence to run the concept"))

; enable next line to get verbose printing and more exports
(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :ncsdbg *features*))

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage :native-code-stepper
    (:use :cl :ccl)
    (:shadowing-import-from :cl-user :step-into)
    (:export
     #:! ; step given function with args

     #:test-explicit-stepping-entry
     #:test-break
     #:test-trace-break

     #:*tracing-enabled*
     #:*stepping-enabled*
     #:stepize-fn
     #:find-source-just-at-select-frame-time
     #:dont-debug-to-listener
     #:skip-if-not-stepping

     #:step-continue
     #:step-over
     #:step-out
     #:step-into 
     )
    ))


(in-package :native-code-stepper)

(import '(step-continue step-over step-out step-into) :cl-user)

;; It is essential. In low debug, there are almost no calls
(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(optimize (space 0) (speed 0) (debug 3) (compilation-speed 0))))


(defvar *tracing-enabled* t "If true, step points are printed")
(defparameter *stepping-enabled* nil ; FIXME this is defparameter as we unable to reset it when stepping out
  ; of lowest steppable frame. So if we switched from execution to stepping we normally dont' switch back until 
  ; user issues "step-continue" command at some point.
  ; Possible solution is to put advice on invoke-debugger
  "Enable/disable stepping. If this is true and *step-into-flag* is t, then step points break execution.")

;; push '(*stepping-enabled*) and '(*tracing-enabled*) to process-initial-bindings
  
(defvar *in-run-steppoint* nil "Bound to t in a call to (break) made inside run-steppoint")
(defvar *stepped-source-is-shown-already-in-the-debugger* nil
  "When debugger is opened and dbg::debugger-select-frame is called for the first time, we try to find and show stepped source")

(defvar *stepper-call-to* nil)

(defvar *step-into-flag* nil "If it is set after break, first step point inside call is fired.")
(defvar *step-over-flag* nil "If it is set after brek, next step point in a caller is fired")

(defvar *in-stepper-trap-frame-break* nil "When stepping is enabled, we think that all traps are parts of stepper, so we bind the variable to know if we are in the trap. Note that traps set by user will be ignored one step-continue (:sc) command is issued")

(defvar *trace-break-function* nil "Bound in the scope of our advice to compiler::trace-break")

; FIXME this is a trash! Replace with a weak hash-table where weak key is a stepped function object and value is nil
(defvar *active-steppoints* nil "list of created breakpoints")


;; CONCEPTS
(defparameter *concepts*
  "Stepizable is a function which we can instrument for stepping
   Steppable is a function such that we can step in. 

   CONS is neither steppable nor stepizable
   FUNCALL is steppable, but not stepizable 
   SOME-USER-FUN is both steppable and stepizable
   PRINT might form another class: it is steppable, but stepping is disabled in recursive calls, otherwise we die immediately. We still have no concept for that, so print is neither steppable nor stepizable (and we can't step inside printing)")

(defparameter *non-steppable-calls* `(
                                      invoke-debugger
                                      run-steppoint
                                      stop-stepping
                                      ;dbg::get-call-frame
                                      ;dbg::debug1
                                      ;conditions::in-break
                                      ;runtime:bad-args-or-stack ; useless
                                      break ; bad things would happen
                                      backtrace-as-list
                                      activate-stepping-and-do-first-step
                                      swank/backend:activate-stepping
                                      swank/backend:sldb-step-into
                                      CCL::RUN-PROCESS-INITIAL-FORM
                                      ccl::%kernel-restart
                                      )
  "Functions call to which we don't touch while making function steppable. Difference with *non-stepizable-fns* is a bit fuzzy...")

;; Functions which we never instrument with step point wrappers
(defparameter *stepizibility-per-symbol* 
  `(
    (invoke-debugger nil)
    (ccl::cheap-eval nil)
    (funcall nil)
    (ccl::call-check-regs nil)
    (,(intern "EVAL-FOR-EMACS-RT" :swank) nil)
    (ccl::cbreak-loop nil)
    (CLCO::|(CLCO::DEF-PATCHED-SWANK-FUN SWANK-REPL::TRACK-PACKAGE)| nil)
    (CLCO::|(CLCO::DEF-PATCHED-SWANK-FUN SWANK-REPL::REPL-EVAL)| nil)
    (CCL::BREAK-LOOP-HANDLE-ERROR nil)
    (CCL::%ERROR nil)
    )
  "Stepizibility per symbol takes priority over that of package")

(defparameter *stepizibility-per-package*
  `((,(find-package :swank) nil)
    (,(find-package :swank/backend) nil)
    (,(find-package :swank/ccl) nil)
    (,(find-package :swank-repl) nil)
    (,(find-package :clco) :ask-user)
    (,(find-package :oduvanchik) :ask-user)
    (,(find-package :native-code-stepper) nil)
    (,(find-package :ccl) :ask-user)
    )
  "Order is important, because a symbol can be in many packages. 
Packages are asked from top to bottom, the first one mentioned yields an answer for the symbol")
    
(defun setf-*stepping-enabled* (value)
  #+ncsdbg (format t "~%setting *stepping-enabled* to ~A~%" value)
  (setf *stepping-enabled* value))

; information of a steppoint for a step-point
(defstruct steppoint-info fn offsets old-called kind)

; See 10.5.3 Allocation of interned symbols and packages - uninterned symbols 
; move too quickly in memory to operate on their addresses safely.
; FIXME try to get rid of this, try to make uninterned symbols with allocation-in-gen-num
(defpackage :bstp (:nicknames :steppoint-symbols-temporary-package) (:use))

(defvar *my-gensym-counter* 0 "Ensures that all steppoint symbols are distinct")



;;----------------------------------------------------------------------------------------------
;;---  CAN WE STEP THIS? -----------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------

(defun call-allowed-to-stepize-p (call-into)
  "Should we instrument the call to this function from other function?"
  (typecase call-into
    ((satisfies steppoint-symbol-p)
     nil)
    (symbol
     (cond
      ((member call-into *non-steppable-calls*)
       nil)
      (t
       (let ((entry
              (or
               (assoc call-into *stepizibility-per-symbol*)
               (assoc (symbol-package call-into) *stepizibility-per-package*))))
         (or
          (not entry)
          (second entry))))))
    #| (function ; FIXME
        (not (member (coerce call-into 'function) *non-steppable-calls* :key (lambda (x) (coerce x 'function))))) |#
    (t ; other constants, e.g. numbers. 
     nil)))

(defun make-long-living-symbol (name)
  (progn 
    (let ((symbol (intern (format nil "~A ~A" name (incf *my-gensym-counter*))
                          :steppoint-symbols-temporary-package)))
      symbol)))

(defun steppoint-symbol-p (x)
  "steppoint symbol names a function call to which is substituted instead of original function call in a code"
  (and (symbolp x)
       (typep (get x 'steppoint-info) 'steppoint-info)))

(defun stepize-stack ()
  #|(let ((commands-left-to-stepize 2))
    (dolist (entry (ccl::backtrace-as-list :count 250))
      (let* ((fn-name-on-stack (first entry))
             (allowed-to-stepize-p (call-allowed-to-stepize-p fn-name-on-stack))
             (stepized-already
              (function-is-stepized-p fn-name-on-stack))
             (stepize-p
              (ecase stepized-already
                ((t)
                 (incf commands-left-to-stepize -1)
                 nil)
                (:empty
                 (incf commands-left-to-stepize -1)
                 nil)
                ((nil)
                 (case allowed-to-stepize-p
                   ((nil) nil)
                   (:ask-user
                    (when (y-or-n-p "Stepize ~S?" fn-name-on-stack)
                      (incf commands-left-to-stepize -1)))
                   (t
                    t))))))
        (when stepize-p
          (stepize-fn fn-name-on-stack)))
      (when (eql commands-left-to-stepize 0)
        (return))))|#)
  
;;----------------------------------------------------------------------------------------------
;;---- STUDY OF STACK FRAMES AND OPERATIONS ON THEM ------------------------------------------------------------
;;----------------------------------------------------------------------------------------------
#|

FIXME - take from SLIME

 (defun frm-get-some-frame ()
  (assert dbg::*debugger-stack*)
  (slot-value DBG::*debugger-stack* 'DBG::current-frame)
  )


 (defun frm-top-frame (&optional initial-frame)
  "Top of debugger stack"
  (setf initial-frame (or initial-frame (frm-get-some-frame)))
  (do ((this initial-frame r)
       (r initial-frame (slot-value r 'dbg::prev)))
      ((null r) this)))

 (defun frm-never-stop-complex-name-p (name)
  "Some forms are parts of stepper. Never stop on them, 
  never show their source"
  (and
   (consp name)
   (or
    (eq (third name) 'stepize-fn-for-one-called)
    ;(equalp name '(subfunction 1 compiler::get-encapsulator))
    (some 'symbol-of-package-not-for-stepping-p
          (cdr name))
    ;(member 'DBG::dbg-trap-frame-break name)
    (and
     (consp (third name))
     (frm-never-stop-complex-name-p (third name))
     ;(member 'DBG::dbg-trap-frame-break (third name))
    ))))

 (defun frm-stepizible-frame-p (frame)
  (when (slot-exists-p frame 'dbg::function-name)
    (let ((fn-name (slot-value frame 'dbg::function-name)))
      (stepizible-function-name-p fn-name)
      )))

 (defun frm-find-topmost-stepizible-frame (down-from-frame)
  "Find potentially steppable fn on debugger stack below from given frame"
  ;(let ((top (frm-top-frame down-from-frame)))
  (do ((frame down-from-frame (slot-value frame 'dbg::%next)))
      ((null frame) nil)
    (when (frm-stepizible-frame-p frame)
      (return-from frm-find-topmost-stepizible-frame frame))
    ))

 (defun frm-stepize-stepizible-frame (frame)
  (stepize-fn (slot-value frame 'DBG::function-name)))


 (defun frm-find-supposed-stepped-frame (any-frame-in-stack)
  "Find a frame we are likely to step. Down-from should be top of the stack"
  (let ((our-frame
         (frm-find-topmost-stepizible-frame (frm-top-frame any-frame-in-stack))
              ; this is currently stepped frame as *in-run-steppoint* is t, hence (run-steppoint)->(break) is on the stack
         ))
    (when (and our-frame *in-stepper-trap-frame-break*)
      (setf our-frame
            (frm-find-topmost-stepizible-frame our-frame))) ; do it twice: frame exited is hidden
    our-frame
    ))

|#

 

;;----------------------------------------------------------------------------------------------
;;------------------------- STEPPOINTS MACHINERY -----------------------------------------------
;;----------------------------------------------------------------------------------------------

#+ncsdbg
(defun temp-steppoint (&rest ignore)
  "When substitution of calls with lambda is too complex, substitue it with temp-steppoint instead"
  (declare (ignore ignore))
  (print "I'm temp-steppoint"))


(defun function-designator-p (x)
  (typecase x
    (function t)
    (symbol (fboundp x))
    (t nil)))

(defvar *functions-known-to-have-no-stepizable-points*
  (make-hash-table :test 'eq :weak :key))

(defun function-is-stepized-p (function-or-name)
  "Are there step point in function now?"
  (let* ((fn (coerce function-or-name 'function))
         (known-to-have-no-stepizable-points
          (gethash fn *functions-known-to-have-no-stepizable-points*))
         (actually-stepized-p 
          (ccl::lfunloop for reference in fn
                         when (steppoint-symbol-p reference)
                         return t)))
    (cond
     (actually-stepized-p t)
     (known-to-have-no-stepizable-points :empty)
     (t nil))))

(defun function-constants (fn)
  (ccl::lfunloop for reference in (coerce fn 'function)
                 collect reference))


(defun stepize-fn (function-or-name)
  "Find all steppable points from compiled function and set steppoints where possible. Might insert no step points actually, if all calls are not call-allowed-to-stepize-p"
  #+ncsdbg (format t "~%stepizing ~S~%" function-or-name)
  (let ((steppoint-symbol-p-v (steppoint-symbol-p function-or-name))
        (function-is-stepized-p-v
         (function-is-stepized-p function-or-name)))
    (cond
     (steppoint-symbol-p-v
      (error "attemp to stepize steppoint-symbol"))
     ((eq function-is-stepized-p-v :empty)
      (swank/ccl::loud-message "Function ~S to stepize has no steppable points"
                    function-or-name))
     ((eq function-is-stepized-p-v t)
      ; steppoints are set already - do nothing
      )
     (t
      (gc)
      (ccl::without-gcing
       (let* ((fn (coerce function-or-name 'function))
             (indices
              (ccl::lfunloop
               for reference in fn for i from 0
               do (when (typep reference
                               '(cons
                                 (eql ccl::indices-of-function-references-in-constant-array)))
                    (return (cdr reference)))))
             (no-of-steppoints 0))
        (warn "Indices = ~S, constants = ~S" indices (function-constants fn))
        (cond
         (indices
          (ccl::lfunloop for reference in fn for i from ccl::+function-immediate-constants-are-counted-from+
                         do (when (find i indices)
                              (assert
                               (function-designator-p reference) ()
                               "Something wrong - attempting to stepise a non-function-designator")
                              (when (call-allowed-to-stepize-p reference)
                                #+ncsdbg (format t "~&stepizing ~S -> ~S~%" fn reference)
                                (stepize-fn-for-one-called fn reference i)
                                (incf no-of-steppoints)))))
         (t
          (warn "Unable to stepize ~S - it has no steppable points" function-or-name)))))))))
  
(defun stepize-fn-for-one-called (fn reference i)
  "fn is a function, reference is a function-designator-p . Set steppoints for one reference. 
  Breakpoint is indeed a closure and a change in a function references (immediates).
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (unless (steppoint-symbol-p reference)
    (let*
        ((steppoint-symbol
          (cond
           ((and (symbolp reference)
                 (symbol-package reference))
            (make-long-living-symbol
             (concatenate
              'string
              "stepper-call-"
              (package-name (symbol-package reference))
              "::"
              (symbol-name reference))))
           (t
            (break "Untested branch for ~S" reference)
            (make-long-living-symbol "stepper-direct-call") ; we can include printable representation of function here, but we need to strip away address of it
            )))
         (steppoint ; lambda call to which is places instead of original call
          (lambda (&rest args)
            (run-steppoint fn reference args))))
      (setf (get steppoint-symbol 'steppoint-info)
            (make-steppoint-info :fn fn :old-called reference))
      (setf (symbol-function steppoint-symbol) steppoint)
      (set-steppoints-for-one-called-in-an-fn fn i steppoint-symbol)
      (push steppoint-symbol *active-steppoints*)
      (values reference steppoint))))

(defun set-steppoints-for-one-called-in-an-fn (fn i steppoint-symbol)
  "There is a ready steppoint. Change the code of fn to call it."
  (ccl::set-nth-immediate fn i steppoint-symbol))

(defmacro with-stepper-restarts (&body body)
  `(let ( ; bindings for break only
         (*step-into-flag* nil) 
         #|
         (DBG::*hidden-symbols*
          (append '(break run-steppoint invoke-debugger stepize-fn-for-one-called) DBG::*hidden-symbols*)) |#
         (*stepper-call-to* call-to)
         (*in-run-steppoint* t)
         (*stepped-source-is-shown-already-in-the-debugger* nil))
     (restart-case
         (let (#+SWANK (swank::*sldb-quit-restart* (find-restart 'step-continue)))
           ,@body
           )
       (step-continue ()  :report "Continue")
       ; (step-out )
       ; (step-next )
       (step-into ()
                  :report "Step into"
                  (setf *step-into-flag* t)
                  (stepize-stack)
                  )) ; result does not matter
     ))
     
(defun run-steppoint (call-from call-to call-args)
  "Run through steppoint. Break if appropriate. Debugger functions will do the rest"
  (when *tracing-enabled*
    (format t "~&native stepper break, from ~S~
               ~% into ~S, args=~S~%" call-from call-to call-args))
  (cond
   (*stepping-enabled*
    (let ( ; bindings for both break and apply
          (*step-over-flag* nil)
          ; (*stepping-enabled* nil) ; so that :c is a continue
          )
      (with-stepper-restarts
       (break "Step: before call from ~S~
               ~%  to ~S with args=~S~%"
              call-from call-to call-args))
      ; step-* functions are called from break that may stepize other functions 
      ; and/or set *step-into-flag*
      (setf-*stepping-enabled* *step-into-flag*))  
    
      (let ((result (multiple-value-list (apply call-to call-args))))
        (with-stepper-restarts
         (break "Step after: call from ~S~
                 ~%  to ~S returned (values~{ ~S~})"
                call-from call-to result))
        (values-list result)))
   (t
    (apply call-to call-args))))

;;-------------------------------------------------------
;;-------------------------------- INTERFACE ------------
;;-------------------------------------------------------
(defun ! (function &rest args)
  "Step function with args"
  (stepize-fn function)
  (let ((*stepping-enabled* t))
    (apply function args)))

(defun fact (n)
  (if (<= n 1) 1
      (* n (fact (- n 1)))))

(defun stop-stepping ()
  (setf *stepping-enabled* nil))

(defun t2 ()
  (stepize-fn 'ccl::compile-named-function)
  (let ((*stepping-enabled* t))
    (eval '(compile (defun f () (print 'list))))))


(defun activate-stepping-and-do-first-step (frame)
  (declare (ignore frame))
  (assert (not *stepping-enabled*))
  (setf *stepping-enabled* t)
  ;; (pprint (ccl::backtrace-as-list :count 50))
  ;; FIXME - see FIXME near other use of stepize-stack
  (stepize-stack)
  (cond
   ((find-restart 'continue)
    (invoke-restart 'continue))
   (t
    (swank/ccl::loud-message "No continue restart - unable to do the first step. Please invoke an appropriate restart by hand"))))

