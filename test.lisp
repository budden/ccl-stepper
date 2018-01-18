(in-package :cl-user)

(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))

(eval-when (:compile-toplevel)
  (setf *compile-code-coverage* nil)
  (setf *disassemble-verbose* t)
  (setf *warn-if-redefine-kernel* nil))

(defstruct ms fld)

(defparameter *graal* (cons 0 0))

(declaim (notinline parameterless))
(proclaim '(notinline parameterless))

(declaim (ftype (function () (values))))

(defun parameterless ()
  (setf (car *graal*) 1)
  (values))

(defun ccl::f (x y)
  (declare (notinline cons))
  ; (parameterless)
  (cons x y))
