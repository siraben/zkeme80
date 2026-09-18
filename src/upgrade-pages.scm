(load "modules.scm")
(for-each (lambda (page) (format #t "~2,'0X " page)) (rom-upgrade-pages))
