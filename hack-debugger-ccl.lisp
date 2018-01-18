;;; -*- coding: utf-8; -*-

(eval-when (:execute)
  (error "Use compile-load sequence to run the concept"))

; enable next line to get verbose printing and more exports
(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :ncsdbg *features*))

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage :native-code-stepper
    (:use :cl :ccl)
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


(proclaim '(optimize (space 0) (speed 0) (debug 3) (compilation-speed 0)))


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
(defparameter *non-steppable-calls* '(
                                      invoke-debugger
                                      ;dbg::get-call-frame
                                      ;dbg::debug1
                                      ;conditions::in-break
                                      ;runtime:bad-args-or-stack ; useless
                                      break ; bad things would happen
                                      ) 
  "Functions call to which we don't touch while making function steppable")

(defvar *non-stepizable-fns*
  '(run-steppoint ! stop-stepping)
  "Functions we never trying to stepize in additional to functions in protected packages")

(defparameter *packages-of-non-stepizable-functions*
  nil
  "Calls to functions in that packages can be step points, but we never step into those functions.
  Take them into our variable so that don't lose in a situation where *packages-for-warn-on-redefinition* is bound to nil"
  )


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

(defun call-steppable-p (call-into call-kind)
  "Can we stop at this call"
  (when (= call-kind 0)
       ; don't attempt to step direct calls as they
       ; use stack in some other way.
    (return-from call-steppable-p
      (cond
       ((search "subfunction of lambda-fact" (format nil "~A" call-into)) t)
       ((search "sub-with-no-args" (format nil "~A" call-into)) t)
       (t nil))))
  (typecase call-into
    (symbol
     (and 
      (not (member call-into *non-steppable-calls*))
      (not (= call-kind 0))))
    (function 
     (not (member (coerce call-into 'function) *non-steppable-calls* :key (lambda (x) (coerce x 'function)))))
    (t ; other constants, e.g. numbers. 
     nil)))
    

(defun package-is-not-for-stepping-p (p)
  "Accepts nil, package, or package name"
  (etypecase p
    (package
     (package-is-not-for-stepping-p (package-name p)))
    (null
     nil)
    ((or symbol string)
     (member p *packages-of-non-stepizable-functions* :test 'string=))
    ))


(defun symbol-of-package-not-for-stepping-p (s)
  "S can be anything, including non-symbol, in this case we return nil"
  (and (symbolp s)
       (package-is-not-for-stepping-p (symbol-package s))))

(defun stepizible-function-name-p (fn-name)
  "Can we step into a function?"
  (cond
   ((member fn-name *non-steppable-calls*) nil)
   ((member fn-name *non-stepizable-fns*) nil)
   ((frm-never-stop-complex-name-p fn-name) nil)
   ((symbol-of-package-not-for-stepping-p fn-name) nil)
   (t t)))


(defun frm-never-stop-complex-name-p (name)
  "Some forms are parts of stepper. Never stop on them, 
  never show their source"
  (and
   (consp name)
   (or
    (eq (third name) 'stepize-fn-for-one-called)
    (some 'symbol-of-package-not-for-stepping-p
          (cdr name))
    (and
     (consp (third name))
     (frm-never-stop-complex-name-p (third name))
    ))))


(defun make-long-living-symbol (name)
  (progn 
    (let ((symbol (intern (format nil "~A ~A" name (incf *my-gensym-counter*))
                          :steppoint-symbols-temporary-package)))
      symbol)))


(defun steppoint-symbol-p (x)
  "steppoint symbol names a function call to which is substituted instead of original function call in a code"
  (and (symbolp x)
       (typep (get x 'steppoint-info) 'steppoint-info)))

  
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

(defun function-has-step-points-p (function-or-name)
  "Are there step point in function now?"
  (let* ((fn (coerce function-or-name 'function)))
    (ccl::lfunloop for reference in fn
                   when (steppoint-symbol-p reference)
                   return t)
    nil))


(defun function-constants (fn)
  (ccl::lfunloop for reference in (coerce fn 'function)
                 collect reference))


(defun stepize-fn (function-or-name)
  "Find all steppable points from compiled function and set steppoints where possible"
  #+ncsdbg (format t "~%stepizing ~S~%" function-or-name)
  (cond
   ((steppoint-symbol-p function-or-name)
    (error "attemp to step steppoint-symbol"))
   ((function-has-step-points-p function-or-name)
    ; steppoints are set already - do nothing
    )
   (t
    (let* ((fn (coerce function-or-name 'function))
           (indices
            (ccl::lfunloop
             for reference in fn for i from 0
             do (when (typep reference
                             '(cons
                               (eql ccl::indices-of-function-references-in-constant-array)))
                  (return (cdr reference))))))
      (warn "Indices = ~S, constants = ~S" indices (function-constants fn))
      (cond
       (indices
        (ccl::lfunloop for reference in fn for i from ccl::+function-immediate-constants-are-counted-from+
                       do (when (find i indices)
                            (assert
                             (function-designator-p reference) ()
                             "Something wrong - attempting to stepise a non-function-designator")
                            #+ncsdbg (format t "~&stepizing ~S -> ~S~%" fn reference)
                            (stepize-fn-for-one-called fn reference i))))
       (t
        (warn "Unable to stepize ~S - it has no steppable points" function-or-name)))))))

(defun stepize-fn-for-one-called (fn reference i)
  "fn is a function, reference is a function-designator-p . Set steppoints for one reference. 
  Breakpoint is indeed a closure and a change in a function references (immediates).
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (unless (steppoint-symbol-p reference)
    (let*
        ((steppoint-symbol
          (cond
           ((symbolp reference)
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
      (let ( ; bindings for break only
            (*step-into-flag* nil) 
            #|
            (DBG::*hidden-symbols*
             (append '(break run-steppoint invoke-debugger stepize-fn-for-one-called) DBG::*hidden-symbols*)) |#
            (*stepper-call-to* call-to)
            (*in-run-steppoint* t)
            (*stepped-source-is-shown-already-in-the-debugger* nil))
        (restart-case
            (let (#+SWANK (swank::*sldb-quit-restart* (find-restart 'step-continue)))
              (break "Step: before call from ~S~
                      ~%  to ~S with args=~S~%"
                     call-from call-to call-args))
          (step-continue ()  :report "Continue")
          ; (step-out )
          ; (step-next )
          (step-into ()
                     :report "Step into"
                     (setf *step-into-flag* t)
                     )) ; result does not matter
        
        ; step-* functions are called from break that may stepize other functions 
        ; and/or set *step-into-flag*
        (setf-*stepping-enabled* *step-into-flag*))
      (apply call-to call-args)))
   (t
    (apply call-to call-args)
    )))

    


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

