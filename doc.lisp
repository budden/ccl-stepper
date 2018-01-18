;; -*- mode: lisp; coding : utf-8; -*- 

(in-package :ccl) 

#.(break "Это документ, а не исходник")


CCL::NX-RECORD-CODE-COVERAGE-ACODE - похоже, что она порождает код, записывающий coverage - её-то мы и перешибём. 


CCL::X862-CODE-COVERAGE-ENTRY в /y/yar/ccl/1.12dev/compiler/X86/x862.lisp генерирует наши 4 инструкции, а именно    



+7: 41 50        (pushq (% arg_x))
+9: 4d 8b 85 c9  (movq (@ '#<CODE-NOTE [NIL] for "/y/yar/c/c.lisp":51-84 "(defun f (x) (if (<= x 1) 1 x))" #x30200383A4ED> (% fn)) (% arg_x))
       00 00 00
+16: 49 c7 40 03  (movq ($ 0) (@ 3 (% arg_x)))
       00 00 00 00
+24: 41 58        (popq (% arg_x))


<- x862-lambda
  <- *x862-specials* (lambda-list)


(setq ccl:*warn-if-redefine-kernel* nil)

*st-graal*

Доступ к реальным записям (через изучение report-coverage)
(second (assoc "/y/yar/c/c.lisp" *code-covered-functions* :test 'equal))
а выглядит она так: 
("/y/yar/c/c.lisp" #(#<Compiled-function COMMON-LISP-USER::F #x3020052191DF>) :INFERRED 507107359972)

В ней функции - это fns , а маг.число - это id. 

(in-package :ccl)

(defparameter *haha* 
  (with-decoded-file-coverage 
    (coverage (assoc "/y/yar/c/c.lisp" *code-covered-functions* :test 'equal)) 
   *code-note-index*)) 

(defmacro lfunloop (for var in function &body loop-body)
  "Loop over immediates in function"

(maphash (lambda (k v) (declare (ignore v)) (watch k)) *haha*)

(handler-bind ((write-to-watched-object 
      #'(lambda (x) (format t "~A" (slot-value x 'object)) (invoke-restart 'skip)))) 
  (cl-user::f 5))

Соответственно, нам нужно научиться доставать эти метки из исходника, и тогда мы даже сможем
ставить брекпойнты. 


НО! Нам нужно иметь общий выключатель ходьбы. Тут два варианта:

либо у нас есть общий выключаталь ходьбы и дополнительный выключатель
либо в режиме пошаговой отладки постоянно срабатывает общий выключатель.

А режим пошаговой отладки включён всегда, когда есть хотя бы один breakpoint

Для начала пусть будет просто один пошаговый. Но потом можно сделать два, нужен будет ещё один на функцию (или на файл). у нас всего 

(length (ccl::all-objects-of-type 'function)) ~= 40000 функций. 
SPGVSet - наверное, это SPecialGlobalValueSet? Но где оно определено - я так и не понял, видимо, нужно искать просто gvset. 

SPGVSet находится в x86-spentry64.s, строка 1843

Поскольку всё равно на каждый чих вызов функции, есть ли смысл экономить? По кол-ву инструкций что вызов, что запись в переменную, что код наблюдения мало отличаются.


Текущий план:
1. Выяснить, как 
