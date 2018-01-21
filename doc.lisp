;; -*- mode: lisp; coding : utf-8; -*- 

(in-package :ccl) 

#.(break "Это документ, а не исходник")

ВСПОМИНАЕМ ПРО ОБЁРТЫВАНИЕ НЕПОСРЕДСТВЕННЫХ ССЫЛОК
==================================================

Текущий план:
1. Каждый вызов - это immediate. Подменяем его. В lw может быть, это и не работало, а в ccl работает. 
2. Делаем как в lw
3. Доделываем недоделанное в lw

Возможно, что вообще не нужно step-out? Например, если функция не вызвана из инструментированной, то продолжится выполнение, а это ведь плохо. 

compile -> backend-p2-compile *target-backend* == X862-COMPILE


ОБЗОР КОМПИЛЯЦИИ
===============

X862-COMPILE - генерирует машкод для x86

CCL::X862-INVOKE-FN - генерирует вызов? 

(define-x8664-vinsn (vstack-discard :vsp :pop :discard) (() - генерируют инструкции. 
! - генерирует код (vinsn). 

afunc-fcells позволяют отличить функции от переменных (проблема (print 'print))

*x862-constant-alist* - константы функции 
x86-immediate-label - порождает новую константу (судьба метки до сих пор неясна, ведь мы в самой функции её никак не используем). 

ccl::lfunloop - достаёт константы из уже готовой функции. 


;;;; 
(ccl::function-source-note (find-method #'print-object nil (mapcar 'find-class '(ccl::cdbx t))))
NIL


ПОСТАВИТЬ БРЕКПОЙНТ
===================
1. Узнать, в какой мы функции. map-heap-objects и all-objects-of-type

pc-source-map содержит закодированное соответствие форм смещениям, трассируй
CCL::X862-GENERATE-PC-SOURCE-MAP

Нам будет плохо, если мы упустим символ. В этом случае breakpoint не сработает. Т.е. нам нужно добавить ещё инфу о том, какой символ вызывается в этом месте. 


Наверное, надо разобраться как идёт инфа по компилятору. 


ХОДИМ ПО КОМПИЛЯТОРУ
====================
CCL::NX1-LAMBDA возвращает что-то вроде:

#<ACODE progn ((#<ACODE values ((#<ACODE call (#<ACODE immediate (BREAK)>
                                                                                                         (NIL
                                                                                                           (#<ACODE immediate ("SDF")>))

После ccl::rewrite-acode-form получили, в частности, 

<ACODE call (#<ACODE immediate (<=)> (NIL (#<ACODE fixnum (4)> #<ACODE lexical-reference (#<VAR N #x302003C40EED>)>)) NIL)>
acode.info = (T . #<SOURCE-NOTE "/y/yar/ccl-stepper/tstdbg.lisp":227-235 "(<= n 4)">)

Т.е. тут пока вся нужная информация вместе. Нужно теперь следить за судьбой этого call и смотреть, где нам нужно успеть эту инфу достать. 


punt-bindings ничего не испортило. 


