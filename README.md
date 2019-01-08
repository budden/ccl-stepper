# Прототип степпера для CCL. 

Как пользоваться

- Скачиваем релиз ccl 1.11 в c:\yar\ccl\tek

- Редактируем c:\yar\ccl\tek\.git\config так:
```
[remote "origin"]
	url = https://github.com/budden/ccl.git
```

- git pull
- git checkout stepper-2019 

- собираем согласно комментарию в https://budden73.livejournal.com/28995.html
(или инструкция для сборки - в lisp-kernel/win64/makefile)

- запускаем clcon на ccl (не забыть очистить кеш фаслов)

- выполняем hack-debugger-ccl.lisp

- `(in-package :NATIVE-CODE-STEPPER)`

- `(! 'ncse:fact 2)` - должен появиться отладчик с перезапуском "STEP-INTO"
