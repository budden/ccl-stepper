(in-package :cl-user)

; (declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0) (space 0)))

(eval-when (:compile-toplevel)
  (setf *compile-code-coverage* nil)
  (setf *disassemble-verbose* t)
  (setf *warn-if-redefine-kernel* nil)
  (setf *print-circle* t *print-length* 50 *print-level* 5))

