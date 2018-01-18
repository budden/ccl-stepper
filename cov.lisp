;; -*- coding: utf-8  -*- ;; 

(setq ccl:*compile-code-coverage* t)
(load (compile-file "/y/yar/c/c.lisp"))
(disassemble 'f)
(setq ccl:*compile-code-coverage* nil)
(load (compile-file "/y/yar/c/c.lisp"))
(disassemble 'f)





;(report-coverage "c-report")
;(clco::open-url "c-report.html")
