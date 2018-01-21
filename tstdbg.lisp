;;; -*- coding: utf-8; -*-

(in-package :native-code-stepper)

;;(print '#.(intern "F2" :cl-user))

;;(eval-when (:compile-toplevel)
;;  (trace :break t ccl::compile-named-function))
  
(defun cl-user::f2 (n)
  (break "SDF")
  (if (<= n 4)
      (print "Hello")
      (cl-user::f2 8)))

