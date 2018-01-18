;; -*- mode: lisp; coding : utf-8; -*- 

(in-package :ccl) 

#.(break "Это документ, а не исходник")

ВСПОМИНАЕМ ПРО ОБЁРТЫВАНИЕ НЕПОСРЕДСТВЕННЫХ ССЫЛОК

Текущий план:
1. Каждый вызов - это immediate. Подменяем его. В lw может быть, это и не работало, а в ccl работает. 
2. Делаем как в lw
3. Доделываем недоделанное в lw

.ВОПРОС -(print 'print) - нет хода. 

compile -> backend-p2-compile *target-backend* == X862-COMPILE


ПРОБЛЕМА
(print 'print)

*next-nx-operators* - вот это мощь!
X862-CALL-FN - компилирует вызов ф-ии, у нас вызывается с параметром
#<ACODE immediate (PRINT)>

ccl::lfunloop - цикл по всем ссылкам ф-иию 

ЗАДАЧА - ИЗУЧИТЬ КОМПИЛЯЦИЮ И ЗАПИСЫВАТЬ ИНФУ О ТОМ, ЯВЛЯЕТСЯ ЛИ ССЫЛКА ВЫЗОВОМ ФУНКЦИИ. 

CCL::X862-INVOKE-FN - генерирует вызов? 

(define-x8664-vinsn (vstack-discard :vsp :pop :discard) (() - похоже, что они только генерируют инструкции, но не заполняют таблицу immediate-ов, к-рая нам нужна. Но мы её ищем. При этом, в X862-INVOKE-FN уже приходят #<ACODE immediate (PRINT)> и #<ACODE immediate (YAR)> - без инфы о том, что из них ф-я, а что - данное. 

! - генерирует код (vinsn). Но пока ещё неясно, нет ли vinsn-ов для формирования ДАННЫХ. 
Поднимемся выше и посмотрим, что приходит в X862-COMPILE - там уже созданы immediate или ещё нет. Да - туда уже приходит оно. Ищем, где оно возникает - нашли:

в nx1-compile-lambda . конкретнее.  

print попадает в *nx1-fcells* во время выполнения nx1-lambda, 
а потом записывается в afunc-fcells. Но только имя ф-ии, а не immediate, 
так что пользы немного. Нам всё же надо следить за историей immediate-а. 

ищем *nx1-fcells* 

nx1-call-form создаёт immediate, примерно так (но есть и другая ветвь там:) 

(make-acode (%nx1-operator call)
            (if (symbolp global-name)
                (nx1-immediate context (if context (nx1-note-fcell-ref gl)))))

похоже, что при создании immediate нет побочных эффектов, кроме записи в *nx1-fcells* 
Где ещё применяются *nx1-fcells* и afunc-fcells ? Единственный сток идёт в *x862-fcells*
смотрим его. 
x862-register-constant-p - для (defun f () (print 'list)) вызывается и для print, и для list, и возвращает nil. 

X862-STORE-IMMEDIATE - рядом попалась, вызывается. 

x862-symbol-locative-p - отличает print от list. Где используется? 

*x862-constant-alist* - живут в x862-compile

Похоже, вот этот кусок:


                  (emit-x86-lap-label frag-list vinsn-label)
                  (target-arch-case
                   (:x8632
                    (x86-lap-directive frag-list :long 0))
                   (:x8664
                    (x86-lap-directive frag-list :quad 0)))))

Попробуем для начала туда подсунуть ещё список того, что является функцией. 




