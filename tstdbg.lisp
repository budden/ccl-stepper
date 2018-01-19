;;; -*- coding: utf-8; -*-

(in-package :native-code-stepper)

(print '#.(intern "F2" :cl-user))

(defun cl-user::f2 (n)
  (break)
  (if (<= n 1) 1
      (* n (cl-user::f2 (- n 1)))))

