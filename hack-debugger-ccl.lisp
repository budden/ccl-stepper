;;; -*- coding: utf-8; -*-

(eval-when (:execute)
  (error "Use compile-load sequence to run the concept"))

; enable next line to get verbose printing and more exports
(eval-when (:compile-toplevel :load-toplevel)
  (pushnew :ncsdbg *features*))

(eval-when (:compile-toplevel :load-toplevel)
  (defpackage :native-code-stepper
    (:use :ccl)
    (:shadowing-import-from #:system #:with-fast-lock)
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
     )
    ))



(in-package :native-code-stepper)


(proclaim '(optimize (space 0) (speed 0) (debug 3) (fixnum-safety 3)))


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
  (copy-list *packages-for-warn-on-redefinition*)
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
    ((or symbol string)
     (member p *packages-of-non-stepizable-functions* :test 'string=))
    (null
     nil)))


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





(defun make-long-living-symbol (name)
  "See 10.5.3 Allocation of interned symbols and packages"
  ; FIXME make this synchronized
  (progn ; allocation-in-gen-num *symbol-alloc-gen-num*
    (let ((symbol (intern (format nil "~A~A" name (incf *my-gensym-counter*))
                          :steppoint-symbols-temporary-package)))
      ; (unintern symbol :steppoint-symbols-temporary-package)
      symbol)))





(defun steppoint-symbol-p (x)
  "steppoint symbol names a function call to which is substituted instead of original function call in a code"
  (and (symbolp x)
       (typep (get x 'steppoint-info) 'steppoint-info)))

  

;;----------------------------------------------------------------------------------------------
;; RESEARCH TOOLS ----------------------------------------------------------------
;;----------------------------------------------------------------------------------------------

#+ncsdbg
(defun read-byte-from-hex-string (s)
  "For debugging"
  (parse-integer s :radix #10r16))


#+ncsdbg
(defun extract-n-bytes (addr n) 
  "For debugging. Extracts n bytes and returns a list of them"
  (let (result)
    (dotimes (i n)
      (push (peek-byte addr) result)
      (incf i)
      (incf addr))
    (nreverse result)))

#+ncsdbg
(defun n-to-hex (x)
  "Превращает в строку"
  (format nil "~X" x))


#+ncsdbg
(defun n-to-32-bytes (x)
  "Дано на входе 32-разрядное число, разбиваем на байты"
  (do ((quotient nil)(remainder nil)(result nil)(i 0 (+ i 1)))
      ((= i +number-of-bytes-in-word+) result)
    (multiple-value-setq (quotient remainder) (floor x #x100))
    (push remainder result)
    ;(format t "~%~X" quotient)
    (setf x quotient)
    ))

#+ncsdbg
(defun poke-opcode (function-object offset opcode-list)
  (let* ((dest-address (+ (extract-address-from-function function-object) offset))
         )
    (dolist (byte opcode-list)
      (poke dest-address byte) 
      (incf dest-address))
    ;(format t "~%dest address is ~X~%poke opcode ~S" dest-address opcode-list)
    ))

#+ncsdbg
(defun 32-bytes-to-n (x)
  "Дан список байт, построить число из них (в обратном порядке)"
  (assert (typep x '(or (cons integer) null)))
  (let* ((len (length x))
         (result 0))
    (dotimes (i len)
      (setf result (+ (* result #x100) (elt x (- len i 1)))))
    result))

#+ncsdbg
(defun maybe-twos-complement (x)
  "Вроде не нужна?"
  (cond
   ((>= x 0) x)
   (t 
    (int32-to-integer (int32-1+ (int32-lognot (integer-to-int32 (- x)))))
   )))

#+ncsdbg
(defun poke-int3 (function offset &optional (count-of-nops 0))
  (assert (functionp function))
  (let ((offs (+ (extract-address-from-function function) offset)))
    (poke offs #xCC) ; int3 int 3
    (dotimes (i count-of-nops)
      (incf offs)
      (poke offs #x90 ; nop
            ))))

;----------------------------------------------------------------------------------------------
;--- ADDRESSES AND FUNCTIONS MANIPULATION -----------------------------------------------------
;----------------------------------------------------------------------------------------------
    
(defparameter +number-of-bytes-in-word+ 4 "Magic constant")
(defparameter +length-of-indirect-call-command+ 6 "Magic constant")
(defparameter +direct-call+ 1)
(defparameter +indirect-call+ 2)
(defparameter +hackish-symbol-value-offset+ 4)
(defparameter +magic-reference-table-offset+ 12)
(defparameter +magic-word-size+ 4)
(defparameter +hackish-address-shift+ 5)

(defun poke (addr byte)
  "Write byte at addr"
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)) byte))

(defun poke-unsigned (addr integer)
  (setf (fli:dereference (fli:make-pointer :address addr :type :unsigned)) integer))

(defun peek-unsigned (addr)
  (fli:dereference (fli:make-pointer :address addr :type :unsigned)))

(defun peek-byte (addr)
  "Read byte from addr"
  (fli:dereference (fli:make-pointer :address addr :type :unsigned-byte)))


(defun extract-address-from-function (function)
  "Возвращает адрес по объекту функции. Lame: extracts address from printed representation"
  (assert (functionp function))
  ; (object-address function) is not that precise
  (let* ((text (format nil "~A" function))
         (length (length text))
         (offset (- length 9))
         (hex-address (subseq text offset (+ offset 8)))
         (*read-base* 16)
         (address (read-from-string hex-address)))
    address))

(defun extract-code-size-from-function (function)
  "Returns a code from a function (LAME: with the help of inspector)"
  (assert (functionp function))
  (let* ((code (first (nth-value 1 (get-inspector-values function nil))))
         (printed-code (prin1-to-string code))
         (end-pos (position #\) printed-code :from-end t))
         (beg-pos (position #\( printed-code :from-end t))
         (length-as-string (subseq printed-code (1+ beg-pos) end-pos))
         (*read-base* #10r10)
         (length (read-from-string length-as-string)))
    length))


(defun calc-call-offset (dst-address address-to-call)
  "Offset for direct call"
  (- address-to-call dst-address +hackish-address-shift+))


(defun object-to-entry-in-reference-table (obj call-kind)
  "We have an object. Lets' calc a value for it that fits into reference table"
  (ecase call-kind
    (1 (+ (object-address obj)
          +hackish-symbol-value-offset+ ; magic
          (- call-kind) ; magic
          ))
    (0 (extract-address-from-function obj))))

(defun do-gc ()
  (gc-generation t))

(defun replace-fn-reference (fn new-fn call-kind old-fn)
  "Changes fn's reference from old-fn to new-fn in an fn's reference table. Call-kind must be of old-fn"
  (allocation-in-gen-num 2
    (let* ((code-size (extract-code-size-from-function fn))
           (references (extract-function-references fn))
           (pos (position old-fn references))
           (gc-done (do-gc))
           (fn-address (extract-address-from-function fn))
           (table-address (+ fn-address code-size +magic-reference-table-offset+))
           (new-value (object-to-entry-in-reference-table new-fn call-kind))
           (check-old-value (object-to-entry-in-reference-table old-fn call-kind))
           entry-address
           real-old-value)
      (declare (ignore gc-done))
      (unless pos
        (return-from replace-fn-reference nil) ; changed already or something's wrong
        )
      (setf entry-address (+ table-address (* pos +magic-word-size+)))
      (setf real-old-value (peek-unsigned entry-address))
      (assert (= real-old-value check-old-value) () "Didn't find correct old value in reference table")
      (poke-unsigned entry-address new-value)
      (do-gc)
      #+ncsdbg (format t "~%Old entry in a reference table: ~X" real-old-value)
      #+ncsdbg (format t "~%code-size=~D,pos=~S,table-address=~X" code-size pos table-address)
      )))  

(defun extract-function-references (function-object)
  "Functions we refer to" ; FIXME simpler way with less consing is in stepize-fn:
  ; (cdr (system::function-constants function-object))
  (assert (functionp function-object))
  (let* ((constants (SYSTEM::function-constants function-object))
         (limit (- (length constants) 1))
         (result nil))
    (dotimes (i limit)
      (push (elt constants (+ i 1)) result))
    (nreverse result)))


(defun get-offsets-of-name-in-callable-constants (fn callable-constants a-name)
  "callable-constants as returned by SYSTEM::compute-callable-constants. Returns list of offsets of a-name"
  (let (result
        a-call-kind
        (first-time-p t))
    (dolist (rec callable-constants)
      (destructuring-bind (offset name call-kind) rec
        (when (eq a-name name)
          (when first-time-p
            (setf a-call-kind call-kind)
            (setf first-time-p nil))
          (unless (eql call-kind a-call-kind)
            (warn "Both direct and indirect calls of ~S from ~S, unable to stepize that calls" a-name fn)
            (return-from get-offsets-of-name-in-callable-constants nil))
          (push offset result))))
    (values (nreverse result) a-call-kind)))


;;----------------------------------------------------------------------------------------------
;;---- STUDY OF STACK FRAMES AND OPERATIONS ON THEM ------------------------------------------------------------
;;----------------------------------------------------------------------------------------------

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

 

;;----------------------------------------------------------------------------------------------
;;------------------------- STEPPOINTS MACHINERY -----------------------------------------------
;;----------------------------------------------------------------------------------------------

#+ncsdbg
(defun temp-steppoint (&rest ignore)
  "When substitution of calls with lambda is too complex, substitue it with temp-steppoint instead"
  (declare (ignore ignore))
  (print "I'm temp-steppoint"))

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
    (let* ((fn (coerce function-or-name 'function)))
      (ccl::lfunloop for v in fn for i from 0 


#|      
           (constants (SYSTEM::compute-callable-constants fn))
           (names-only (cdr (SYSTEM::function-constants fn))) ; names without duplicates
           )
      #+ncsdbg (format t "~%stepizing ~S~%constants=~S~%" fn constants)
      (dolist (call-into names-only)
        (multiple-value-bind (offsets call-kind)
            (get-offsets-of-name-in-callable-constants fn constants call-into)
          (cond
           ((null offsets)
            ; do nothing
            )
           ((call-steppable-p call-into call-kind)
            (stepize-fn-for-one-called fn offsets call-into call-kind))
           (t
            ; do nothing
            ))))))))  |#

(defun run-steppoint (call-from call-to call-args)
  "Run through steppoint. Break if appropriate. Debugger functions will do the rest"
  (when *tracing-enabled*
    (format t "~%native stepper break, from ~S into ~S, args=~S" call-from call-to call-args))
  (cond
   ((and *stepping-enabled*)
    (let ( ; bindings for both break and apply
          (*step-over-flag* nil)
          ; (*stepping-enabled* nil) ; so that :c is a continue
          ) 
      (let ( ; bindings for break only
            (*step-into-flag* nil)
            (DBG::*hidden-symbols*
             (append '(break run-steppoint invoke-debugger stepize-fn-for-one-called) DBG::*hidden-symbols*))
            (DBG::*default-debugger-commands*
             (append 
              '(
                (:sc step-continue "continue")
                ; (:si step-into "step into") is added permanently
                ; (:so step-over "step over") is added permanently
                )
              DBG::*default-debugger-commands*))
          ;(DBG:*print-invisible-frames* nil)
            (*stepper-call-to* call-to)
            (*in-run-steppoint* t)
            (*stepped-source-is-shown-already-in-the-debugger* nil))
        (break "Stepper break before call from ~S into ~S with args=~S~%Current source should be highlighted in the editor.~%type :si (F4) for step in, :so (F8) for step over, :sc (F9) for continue" call-from call-to call-args)
        ; step-* functions are called from break that may stepize other functions 
        ; and/or set *step-into-flag*
        (setf-*stepping-enabled* *step-into-flag*)
        )
      (multiple-value-prog1
          (apply call-to call-args)
        (when *step-over-flag* (setf-*stepping-enabled* t)))))
   (t
    (apply call-to call-args)
    )))


(defun stepize-fn-for-one-called (fn offsets old-called kind)
  "Set steppoints for one pair fn,old-called, where fn is caller, old-called is callee.
  Breakpoint is indeed a closure and a change in a function code and constants. 
  Only function calls can be used as a breakpoint locations, otherwise debugger
  is unable to find source location so the entire mechanism becomes useless"
  (allocation-in-gen-num 2
    (let*
        ((steppoint-symbol
          (cond
           ((symbolp old-called)
            (make-long-living-symbol (concatenate 'string
                                                  "stepper-call-"
                                                  (package-name (symbol-package old-called))
                                                  "::"
                                                  (symbol-name old-called))))
           (t              
            (make-long-living-symbol "stepper-direct-call") ; we can include printable representation of function here, but we need to strip away address of it
            )))
         (steppoint ; lambda call to which is places instead of original call
          (lambda (&rest args)
            (run-steppoint fn old-called args))))
      (setf (get steppoint-symbol 'steppoint-info)
            (make-steppoint-info :fn fn :offsets offsets :old-called old-called :kind kind))
      (setf (symbol-function steppoint-symbol) steppoint)
      (set-steppoints-for-one-called-in-an-fn fn offsets steppoint-symbol kind old-called)
      (push steppoint-symbol *active-steppoints*)
      (values old-called steppoint))))


(defun set-steppoints-for-one-called-in-an-fn (fn offsets steppoint-symbol call-kind old-called)
  "There are ready steppoints. Change the code of fn to call them. Offsets is a list of offsets as returned by SYSTEM::compute-callable-constants"
  (allocation-in-gen-num 2
    (
     #+ncsdbg progn
              #-ncsdbg with-other-threads-disabled ; FIXME disable other threads appropriately
      (let* ((fn (coerce fn 'function))
             (steppoint-fn (symbol-function steppoint-symbol))
             fn-address
             new-opcode
             replace-to-address
             )
        (assert (functionp fn))
        (assert (or (functionp old-called) (and (symbolp old-called)
                                                (fboundp old-called))))
        ; replace reference 
        (ecase call-kind
          (1 ; call [symbol]
                   ; если funcall (car list) превращается в call [адрес (car list)], мы сломаемся. FIXME проверить
           (assert (symbolp old-called) () "Can't handle other indirect calls than from a symbol")
           (do-gc)
           (replace-fn-reference fn steppoint-symbol 1 old-called)
           )
          (0
           (do-gc)
           (replace-fn-reference fn steppoint-fn 0 old-called)
           ))
        ; replace opcodes
        (dolist (offset offsets)
          (setf new-opcode
                (ecase call-kind
                  (1 ; call [symbol]
                   ; If (car list) is converted to call [address (car list)], we will fail
                   (assert (symbolp old-called) () "Can't handle other indirect calls than from a symbol")
                   (do-gc)
                   (replace-fn-reference fn steppoint-symbol 1 old-called)
                   (do-gc)
                   (setf fn-address (extract-address-from-function fn))
                   (setf replace-to-address (+ (object-address steppoint-symbol) +hackish-symbol-value-offset+))
                   replace-to-address
                   )
                  (0
                   (do-gc)
                   (replace-fn-reference fn steppoint-fn 0 old-called)
                   (do-gc)
                   (setf fn-address (extract-address-from-function fn))
                   (setf replace-to-address (extract-address-from-function steppoint-fn))
                   (calc-call-offset (+ (extract-address-from-function fn) offset -1) replace-to-address)
                   )))
          (poke-unsigned (+ fn-address offset) new-opcode)
          #+ncsdbg (format t "~%fn-address=~X, replace-to-address=~X"
                           fn-address replace-to-address)
          (assert (= (extract-address-from-function fn) fn-address)
              () "your image is likely smashed (1)")
          (ecase call-kind
            (1
             (assert (= (+ (object-address steppoint-symbol) +hackish-symbol-value-offset+) replace-to-address)
                 () "your image is likely smashed (2)"))
            (0
             (assert (= (extract-address-from-function steppoint-fn) replace-to-address)
                 () "your image is likely smashed (3)")))
          )
        old-called
        ))))


       
(defun stepize-and-step-frame (frame)
  "Stepizes a fn of frame and steps in the frame without stepping into call"
  ; entering step mode in a selected frame
  (assert (frm-stepizible-frame-p frame))
  (stepize-fn (slot-value frame 'DBG::function-name))
  (install-break-on-leaving-a-frame frame)
  (cond
   (*in-run-steppoint*
    (setf *step-over-flag* t)
    (setf *step-into-flag* nil)
    )
   (t
    (setf-*stepping-enabled* t)
    )
   )
  (dbg::dbg-continue))


;;-------------------------------------------------------
;;-------- DEALING WITH VARIOUS KINDS OF BREAKS  --------
;;-------------------------------------------------------
(defadvice (DBG::dbg-trap-frame-break skip-if-not-stepping :around) (values function-name)
  "Stepper can set up traps on return to support stepping out of the function. Handle this"
  (format t "~%Entering advice for DBG::dbg-trap-frame-break with ~S~%" function-name)
  (cond
   ((not *stepping-enabled*)
    (format t "~%Ignoring trap on exit of ~S~%" function-name)
    nil ; swallow trap silently. This intereferes normal operation of traps - they
        ; will not work anymore unless in stepper.
    )
   (t
    (setf-*stepping-enabled* nil)
    (setf *step-over-flag* nil)
    (setf *step-into-flag* nil) 
    (let ((*in-stepper-trap-frame-break* t)
          (*stepped-source-is-shown-already-in-the-debugger* nil))
      (format t "~%Calling next-advice for DBG::dbg-trap-frame-break with ~S~%" function-name)
      (call-next-advice values function-name)))))


(defadvice (compiler::trace-break trace-break-stepper-advice :around) (function args where)
  "Support switching from trace with break into stepping"
  (let ((*trace-break-function* function))
    (call-next-advice function args where)))




;;-------------------------------------------------------
;;-------- TUNE THE IDE  --------------------------------
;;-------------------------------------------------------

(defadvice (lispworks-tools::debugger-select-frame find-source-just-at-select-frame-time :before) (frame self &optional update-backtrace)
  (declare (ignore update-backtrace))
  (unless *stepped-source-is-shown-already-in-the-debugger*
    (when t ; *stepping-enabled* 
      (when (or *in-run-steppoint* *in-stepper-trap-frame-break*)
        (setf frame
              (or frame (frm-get-some-frame)))
        (let ((stepped-code-frame
               (frm-find-supposed-stepped-frame frame))
              )
          (when stepped-code-frame
            (setf *stepped-source-is-shown-already-in-the-debugger* t)
            (format t "~%let's watch at ~S~%" stepped-code-frame)
            (ignore-errors
              (system::dbg-edit-frame-internal stepped-code-frame self))
            ))))))
        
(defadvice (dbg::debug-to-listener-p dont-debug-to-listener :around) (condition)
  "Always open GUI debugger in stepping mode" 
  (if (or *stepping-enabled* *in-stepper-trap-frame-break*)
      nil (call-next-advice condition)))



;;-------------------------------------------------------
;;--- IDE COMMANDS FOR STEPPER --------------------------
;;-------------------------------------------------------
(defun pane-from-stream (stream)
  (typecase stream
    (EDITOR::rubber-stream
     (EDITOR::rubber-stream-text-pane stream))
    (t
     nil)))


(defun display-stepper-message (format-string &rest format-args)
  (apply 'CAPI:display-message-for-pane (pane-from-stream *standard-output*)
         format-string format-args))


(defun step-continue (&rest ignore)
  "Continue execution w/o stepping"
  (declare (ignore ignore))
  (setf-*stepping-enabled* nil)
  ;(setf *in-run-steppoint* nil) действительно это надо было?
  (dbg::dbg-continue))


(defun function-has-step-points-p (function-or-name)
  "Are there step point in function now?"
  (let* ((fn (coerce function-or-name 'function))
         (constants (SYSTEM::compute-callable-constants fn)))
    (dolist (rec constants)
      (destructuring-bind (offset call-into call-kind) rec
        (declare (ignore offset call-kind))
        (when (steppoint-symbol-p call-into)
          (return-from function-has-step-points-p t))))
    nil))


(defun install-break-on-leaving-a-frame (frame)
  "Ensure break when a given frame is being exited"
  (let ((next-stepizible-frame
         (frm-find-topmost-stepizible-frame frame)))
    ;(when next-stepizible-frame
    ;  ; this handle only normal exit so is insufficient
    ;  (dbg-stepize-stepizible-frame next-stepizible-frame))
    (declare (ignore next-stepizible-frame))
    (format t "~%Setting trap on exit of ~S~%" frame)
    (DBG::in-dbg-trap-on-exit frame)
    ))


(defun step-over (&rest ignore)
  "Step current function not entering the calls"
  (declare (ignore ignore))
  (assert DBG::*debugger-stack*)
  (let* ((frame (frm-get-some-frame))
         (maybe-steppable-frame
          (frm-find-supposed-stepped-frame frame)))
    (cond
     ((or *in-run-steppoint* *in-stepper-trap-frame-break*) ; we're already at step-point break, so which frame to step is predetermined
      (cond
       (maybe-steppable-frame
        (stepize-and-step-frame maybe-steppable-frame))
       (t ; FIXME separate this into function stepper-message
        (display-stepper-message "step-over: no stepizible frame found. Choose step-continue command in the listener (:sc or F9)")
        ))
      )
     ((frm-stepizible-frame-p frame)
      ; we are not at step point, but stepizible frame is selected
      (stepize-and-step-frame frame))
     ((frm-stepizible-frame-p maybe-steppable-frame)
      ; we are not as step point, try to get some stepizible frame
      (stepize-and-step-frame maybe-steppable-frame))
      ; (setf *stepped-source-is-shown-already-in-the-debugger* nil) ; for the trap
     (t ; ent
      (display-stepper-message "Selected frame is not steppable"))
     )))

(eval-when (:load-toplevel)
  ; step over allows us to try to initiate stepping at any entry to the debugger.
  (pushnew '(:so step-over "Step over")
           DBG::*default-debugger-commands*
           :test 'equalp)
  (pushnew '(:si step-into "Step into")
           DBG::*default-debugger-commands*
           :test 'equalp)
  )


(defun begin-stepping-into-from-stepping (fn)
  "We are at debugger, inside (run-steppoint). User wants to step into some function"
  (cond
   ((stepizible-function-name-p fn)
    (stepize-fn fn)
    (setf *step-into-flag* t)
    (setf *step-over-flag* nil)
      ; (setf *stepped-source-is-shown-already-in-the-debugger* nil) ; for the trap
    (DBG::dbg-continue))
   (t
    (display-stepper-message "function ~S is not steppable" fn)
    )
   ))


(defun begin-stepping-into-from-non-stepping (fn)
  "We are at debugger, but not inside (run-steppoint). User wants to step into some function"
  (cond
   ((stepizible-function-name-p fn)
    (stepize-fn fn)
    (setf *stepping-enabled* t)
    (dbg::dbg-continue))
   (t
    (display-stepper-message "function ~S is not steppable" fn)
    )))

(defun step-into (&rest ignore)
  (declare (ignore ignore))
  (cond
   (*in-run-steppoint*
    ; already in step mode
    (cond 
     ((null *stepper-call-to*)
      (DISPLAY-STEPPER-MESSAGE "Looks like a bug: *stepper-call-into* is nil while *in-run-steppoint* is t"))
     (t
      (begin-stepping-into-from-stepping *stepper-call-to*))
     ))          
   (*trace-break-function*
    ; trying to enter step mode from trace with break
    (begin-stepping-into-from-non-stepping *trace-break-function*)
    )
   (t
    (display-stepper-message "Unable to step into")
    ))
  )


;;-------------------------------------------------------
;;--- IDE COMMAND BINDINGS --------------------------
;;-------------------------------------------------------

(editor:defcommand "Step Into" (p)
     "Step Into (native code)"
     "Step Into (native code)"
  (declare (ignore p))
  (editor::execute-listener-command 'editor::execute-debugger-option
                                    ''step-into
                                    nil))

(EDITOR:bind-key "Step Into" "F4")

    
(editor:defcommand "Step Over" (p)
     "Step Over (native code)"
     "Step Over (native code)"
  (declare (ignore p))
  (editor::execute-listener-command 'editor::execute-debugger-option
                                    ''step-over
                                    nil))

(EDITOR:bind-key "Step Over" "F8")

(editor:defcommand "Step Continue" (p)
     "Step Continue (native code)"
     "Step Continue (native code)"
  (declare (ignore p))
  (editor::execute-listener-command 'editor::execute-debugger-option
                                    ''step-continue
                                    nil))

(EDITOR:bind-key "Step Continue" "F9")


;;-------------------------------------------------------
;;-------------------------------- INTERFACE ------------
;;-------------------------------------------------------
(defun ! (function &rest args)
  "Step function with args"
  (stepize-fn function)
  (let ((*stepping-enabled* t))
    (apply function args)))


(defun stop-stepping ()
  (setf *stepping-enabled* nil))


;;-------------------------------------------------------
;;-------------------------------- TESTS ----------------
;;-------------------------------------------------------

(defun sub-of-x-and-y (x &key y)
  "test function"
  (format t "~%sub-of-x-and-y is running. Args: ~S,~S~%" x y)
  (print "Calling break")
  ; cerror would work here as well
  (break "This is a simple call to (break). Type :so or (F8) in debug listener to enter stepping mode, or :c to avoid stepping")
  (+ y x))

(defun sub-of-x (x)
  "test function"
  (format t "~%sub-of-x is running. Arg: ~S~%" x)
  (+ 1 x))

(defun sub-with-no-args ()
  "test function"
  (print "sub-with-no-args is running"))


#-ncsdbg(capi:display-message "Now you can run tests (see package definition).~%As GUI debugger occurs for the first time, place debugger and editor windows so that they will be visible simultaneously")

; test-explicit-stepping-entry - explicit call to stepper -----------
(defun fact (x)
  (cond
   ((= x 0) 1)
   ((> x 0) (* x (fact (- x 1))))
   (t (error "wrong argument ~S for tst-fact" x))))
  
(defun test-explicit-stepping-entry ()
  #-ncsdbg(capi:display-message "do-test-fact - step tst-fact with the help of native-code-stepper:!")
  (! 'fact 1))

; test-break - turn to stepping from (break) -----------
(defun tfn-break (x)
  (+ (sub-of-x-and-y x :y 1.1)

     ; you can step into sub-of-x
     (sub-of-x 2)
     ; you can step into sub-of-x
     
     )
  )

(defun test-break ()
  #-ncsdbg(capi:display-message "test-break - an example of switching into stepping from the (break). Just press F8 in a debugger when (break) occurs")
  (tfn-break 2)
  )

; test-trace-break - turn to stepping from (trace break) -------
(defun tfn-trace-break ()
  (cons (sub-of-x 2)
        (sub-of-x 3)))

(defun test-trace-break ()
  #-ncsdbg(capi:display-message "test-trace-break - an example of switching into stepping from (trace (:break t). Just enter :si (F4) in debug listener when trace invokes (break)")
  (trace (tfn-trace-break :break t))
  (tfn-trace-break)
  )


(defun lambda-fact (n)
  (labels ((fact (i)
             (cond ((= i 0) 1)
                   (t (* i (fact (- i 1)))))))
    (fact n)))
 


 ; Direct call test. If use that, remove 
 ;  (when (= call-kind 0)
 ;      ; don't attempt to step direct calls as they
 ;      ; use stack in some other way. 
 ;   (return-from call-steppable-p nil))
 ; at the beginning of call-steppable-p 

(compile `(defun test-fnd ()
            (funcall ,#'sub-with-no-args)
            ))

; UNCOMMENT TO TEST AT LOAD TIME (stepize-fn 'test-fnd)
; UNCOMMENT TO TEST AT LOAD TIME (! 'test-fnd)


; some potentialy useful staff
; SYSTEM::disassembly-objetc-find-reference-for-offset 
