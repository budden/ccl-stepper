;;; -*- coding: utf-8; -*-

(eval-when (:execute)
  (error "Use compile-load sequence to run the concept"))

; enable next line to get verbose printing and more exports
(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :ncsdbg *features*))

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage :ncse ; native-code-stepper-example
    (:use)
    (:export #:fact)))

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage :native-code-stepper
    (:use :cl :ccl)
    #+sbcl (:shadowing-import-from :cl-user :step-into)
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


(defparameter *tracing-enabled* nil "If true, step points are printed")

(defparameter *stepping-enabled* nil 
  "Enable/disable stepping. If this is true and *step-into-flag* is t, then step points break execution.")

;; push '(*stepping-enabled*) and '(*tracing-enabled*) to process-initial-bindings
  
(defvar *stepped-source-is-shown-already-in-the-debugger* nil
  "When debugger is opened and dbg::debugger-select-frame is called for the first time, we try to find and show stepped source")

(defvar *step-out-flag* nil)

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
                                      ;; this call crashed
                                      CCL::X862-MAKE-STACK

                                      ;; suspicios 
                                      ;; print  ; keep it for debug
                                      format
                                      finish-output
                                      princ
                                      prin1
                                      terpri
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

(defvar *function-stepizibility-info-cache*
  ; key - function object
  ; value:
   ; has-no-steppable-points ;; means «has no steppable points»
   ; is-a-steppoint
   ; stepized-already
   ; forbidden
  (make-hash-table :test 'eq :weak :key))


;;----------------------------------------------------------------------------------------------
;;---  CAN WE STEP THIS? -----------------------------------------------------------------------
;;----------------------------------------------------------------------------------------------

(defun call-allowed-to-stepize-p (call-into)
  "Should we instrument the call to this function from other function?. This function should not cons"
  (typecase call-into
    ((satisfies steppoint-symbol-p)
     nil)
    (symbol
     (let ((fn (fboundp call-into)))
       (cond
        ((null fn) nil)
        (t
         (let ((status (gethash fn *function-stepizibility-info-cache*)))
           (ecase status
             (:forbidden nil) ; на самом деле есть два запрета
           ; - запрет степизировать вызов и запрет степизировать саму функцию!
             ((:has-no-steppable-points :is-a-steppoint :stepized-already)
              t)
             ((nil)
              (cond
               ((null (symbol-package call-into))
                (cache-stepizibility-info call-into :forbidden)
                nil) ; some setters fall here
               ((member call-into *non-steppable-calls*)
                (cache-stepizibility-info call-into :forbidden)
                nil)
               (t
                (call-allowed-to-stepize-by-function-name-p call-into fn))))))))))
    #| (function ; FIXME
        (not (member (coerce call-into 'function) *non-steppable-calls* :key (lambda (x) (coerce x 'function))))) |#
    (t ; other constants, e.g. numbers. 
       nil)))

(defun call-allowed-to-stepize-by-function-name-p (call-into call-into-as-fn)
  (let ((entry
         (or
          (assoc call-into *stepizibility-per-symbol*)
          (assoc (symbol-package call-into) *stepizibility-per-package*))))
    (cond
     ((or
       (not entry)
       (second entry)) t)
     (t
      (cache-stepizibility-info call-into-as-fn :forbidden)
      nil))))

(defun make-long-living-symbol (name)
  (progn 
    (let ((symbol (intern (format nil "~A ~A" name (incf *my-gensym-counter*))
                          :steppoint-symbols-temporary-package)))
      symbol)))

(defun steppoint-symbol-p (x)
  "steppoint symbol names a function call to which is substituted instead of original function call in a code"
  (and (symbolp x)
       (typep (get x 'steppoint-info) 'steppoint-info)))

;; We had loud-message in swank, but it's gone somehow
(defun loud-message (&rest args)
  (apply 'warn args))

(defun stepize-stack ()
  "Подготавливает к шаганию эту функцию и до 250 тех, кто её вызвал"
  (let ((commands-left-to-stepize 1))
    (dolist (entry (ccl::backtrace-as-list :count 250))
      (let* ((fn-name-on-stack (first entry))
             (fn (coerce fn-name-on-stack 'function))
             (allowed-to-stepize-p (call-allowed-to-stepize-p fn-name-on-stack))
             (stepized-already
              (eq
               (gethash fn *function-stepizibility-info-cache*)
               :stepized-already))
             (stepize-p
              (ecase stepized-already
                ((t)
                 (incf commands-left-to-stepize -1)
                 nil)
                (:has-no-steppable-points
                 (incf commands-left-to-stepize -1)
                 (loud-message "Nothing to stepize in ~S" entry)
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
        (return)))))
  
 

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

(defun function-constants (fn)
  (ccl::lfunloop for reference in (coerce fn 'function)
                 collect reference))


(defmacro carefully (&body body)
  "It was intended to run in an exclusive mode, but 
there's no exclusive mode"
  `(progn ,@body))

(defun stepize-fn (function-or-name)
  "Find all steppable points from compiled function and set steppoints where possible. Might insert no step points actually, if all calls are not call-allowed-to-stepize-p"
  #+ncsdbg (format t "~%stepizing «~S»~%" function-or-name)
  (let* ((function (coerce function-or-name 'function))
         (info (gethash function
                        *function-stepizibility-info-cache*)))
    (ecase info
      (:has-no-steppable-points
       #+ncsdbg (format t "~&«~S» has no steppoints - no work~%" function))
      (:is-a-steppoint
       (error "attempt to stepize steppoint-symbol"))
      (:stepized-already
       #+ncsdbg (format t "~&«~S» stepized already - skpping~%" function))
      (:forbidden
       #+ncsdbg (format t "~&«~S» is forbidden to stepize - skpping~%" function))
      ((nil)
       (stepize-fn-inner function-or-name)
      ))))

(defun cache-stepizibility-info (function-or-name info)
  (setf (gethash function-or-name *function-stepizibility-info-cache*)
        info))

(defun stepize-fn-inner (function-or-name)
  (let* ((start ccl::+function-immediate-constants-are-counted-from+)
         (fn (coerce function-or-name 'function))
         (no-of-steppoints 0))
    (multiple-value-bind
        (indices last-index)
        (carefully 
         (ccl::lfunloop
          for reference in fn for i from start
          do (when (typep reference
                          '(cons
                            (eql ccl::indices-of-function-references-in-constant-array)))
               (return (values (cdr reference) i)))))
      (warn "Indices = ~S, constants = ~S" indices (function-constants fn))
      (cond
       (indices
        (let* ((references
                (make-array (+ last-index 1) :initial-element nil))
               (non-references
                (make-array (+ last-index 1) :initial-element nil))) ; may be longer than needed
          (carefully
           (ccl::lfunloop for reference in fn
                          for our-ref-array-index from 0
                          for immediate-num from start
                          do (cond
                              ((member immediate-num indices)
                               (setf (aref references our-ref-array-index) reference))
                              )))
          (pprint references)
          (pprint non-references)
          ;(warn "references = ~S" references)
          (loop
            for reference across references
            for our-ref-array-index from 0
            for immediate-num from start
            do (when (member immediate-num indices)
                 (unless (function-designator-p reference)
                   (error "Something wrong - attempting to stepise a non-function-designator ~S" reference))
                 (when (call-allowed-to-stepize-p reference)
                   (progn
                     #+ncsdbg (format t "~&stepizing call from ~S to ~S~%" fn reference)
                     (stepize-fn-for-one-called fn reference
                                                (- immediate-num start))
                     (incf no-of-steppoints)))))
          (cache-stepizibility-info fn :stepized-already)
          ))
       (t
        (cache-stepizibility-info fn :has-no-steppable-points)
        #+ncsdbg (format t "~&~S has no steppable points~%" fn))))))
  
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
      (cache-stepizibility-info fn :is-a-steppoint)
      (set-steppoints-for-one-called-in-an-fn fn i steppoint-symbol)
      (push steppoint-symbol *active-steppoints*)
      (values reference steppoint))))

(defun set-steppoints-for-one-called-in-an-fn (fn i steppoint-symbol)
  "There is a ready steppoint. Change the code of fn to call it."
  (print "=============================")
  (print i)
  (print steppoint-symbol)
  (let ((old (carefully
              (ccl::nth-immediate fn (+ i ccl::+function-immediate-constants-are-counted-from+)))))
    (print old))
  (carefully
   (ccl::set-nth-immediate fn (+ i ccl::+function-immediate-constants-are-counted-from+) steppoint-symbol))
  (print "after")
  (print steppoint-symbol)
  ;(print (ccl::nth-immediate fn 44))
  (print (ccl::nth-immediate fn (+ i ccl::+function-immediate-constants-are-counted-from+))))


(defvar *последнее-повеление* :undefined) 

(defmacro with-stepper-restarts ((перед-вызовом-ли) &body body)
  "Ссылается на лок.переменные из run-steppoint"
  `(let ((*stepped-source-is-shown-already-in-the-debugger* nil))
     (setf-*stepping-enabled* nil)
     (assert (not (eq *последнее-повеление* :undefined)) () "В треде при пошаговой отладке должно быть задано *последнее-повеление*")
     ;; если не выбрано шагать, то считаем, что надо продолжать
     (setq *последнее-повеление* 'step-continue)
     (stepize-stack)
     (restart-case
         (let (#+SWANK (swank::*sldb-quit-restart* (find-restart 'step-continue)))
           ,@body
           )
       (step-next
        ()
        :report
        (lambda (поток)
          (format поток "~S"
                  (if ,перед-вызовом-ли 
                      "Переступи и покажи итог"
                      "Переступи (= шагни)")))
        (setq *последнее-повеление* 'step-next))
       (step-into 
        () :report (lambda (поток)
                     (format поток "~S"
                             (if ,перед-вызовом-ли 
                                 "Зайди"
                                 "Зайди (= шагни)")))
        (setq *последнее-повеление* 'step-into))
       (step-continue 
        () :report "Беги"
        (setq *последнее-повеление* 'step-continue)
        )) ; result does not matter
     ))

(defvar *можно-выйти-наверх* nil)

(defun run-steppoint (call-from call-to call-args)
  "Есть точка вызова, за которую мы уже зацепились. Пройти её - отладка может быть включена или выключена"
  (when *tracing-enabled*
    (format t "~&native stepper break, from ~S~
               ~% into ~S, args=~S~%" call-from call-to call-args))
  (let (result-values-list)
    (unless (eq *последнее-повеление* 'step-continue)
      (with-stepper-restarts
       (t)
       (break "Step: before call from ~S~
               ~%  to ~S with args=~S~%"
              call-from call-to call-args)))
    (ecase *последнее-повеление*
      ((step-into step-continue)
       (stepize-fn call-to) ; надо, чтобы после кеширования это происходило ОЧЕНЬ быстро!
       (apply call-to call-args))
      (step-next
       (setq *последнее-повеление* 'step-continue )
       (setq result-values-list (multiple-value-list (apply call-to call-args)))
       ; *последнее-повеление* могло поменяться внутри функции
       ; мы игнорируем step-continue, т.е. заказ на итог - это как бы 
       ; точка останова
       (with-stepper-restarts
        (nil)
        (break "Итог: вызов ~S вернул ~{~S~%~}" call-to result-values-list))
       (values-list result-values-list)))))

;;-------------------------------------------------------
;;-------------------------------- INTERFACE ------------
;;-------------------------------------------------------
(defun ! (function &rest args)
  "Step function with args"
  (stepize-fn function)
  (setf *последнее-повеление* 'step-into)
  (apply function args))

(defun ncse:fact (n)
  (if (<= n 1) 1
      (* n (ncse:fact (- n 1)))))

(defun stop-stepping ()
  (setf *последнее-повеление* 'step-continue))

(defun activate-stepping-and-do-first-step (frame)
  (declare (ignore frame))
  (assert (not *stepping-enabled*))
  (setf *последнее-повеление* 'step-into)
  ;; (pprint (ccl::backtrace-as-list :count 50))
  ;; FIXME - see FIXME near other use of stepize-stack
  (stepize-stack)
  (cond
   ((find-restart 'continue)
    (invoke-restart 'continue))
   (t
    (loud-message "No continue restart - unable to do the first step. Please invoke an appropriate restart by hand"))))

