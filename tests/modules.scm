(use-modules (srfi srfi-64))
(load "../src/modules.scm")

(define runner (test-runner-simple))
(test-runner-current runner)
(test-begin "rom-modules")
(define core (make-rom-module 'core 'resident '("core.fs")))
(define (allocation-text allocation layout reader)
  (utf8->string (u8-list->bytevector
                 (drop-right (module-source-bytes allocation layout reader) 1))))
(define (empty-source file) "")

(test-equal "current manifest allocation" '(1 3 4 5 7)
  (map rom-allocation-page module-layout))
(test-equal "lookup by name" 5 (module-page 'workbench))
(test-error "unknown module" (module-page 'missing))
(test-error "empty manifest" (allocate-rom-modules '()))
(test-error "core must be first"
  (allocate-rom-modules (list (make-rom-module 'editor 'resident '("edit.fs")) core)))
(test-error "core must be resident"
  (allocate-rom-modules (list (make-rom-module 'core 'tool '("core.fs")))))
(test-error "duplicate names" (allocate-rom-modules (list core core)))
(test-error "invalid kind"
  (allocate-rom-modules (list core (make-rom-module 'other 'unknown '("x.fs")))))
(test-error "empty source list"
  (allocate-rom-modules (list core (make-rom-module 'other 'tool '()))))
(test-error "unsafe generated constant name"
  (allocate-rom-modules (list core (make-rom-module (string->symbol "bad name") 'tool '("x.fs")))))
(test-error "generated constant must fit Forth name limit"
  (allocate-rom-modules
   (list core (make-rom-module (string->symbol (make-string 25 #\a)) 'tool '("x.fs")))))
(test-error "must preserve storage reservation"
  (allocate-rom-modules (list core) (delete 8 reserved-pages)))
(test-error "cannot reserve bootstrap page"
  (allocate-rom-modules (list core) (cons 1 reserved-pages)))
(test-error "reject duplicate reserved pages"
  (allocate-rom-modules (list core) (cons 8 reserved-pages)))
(test-error "page exhaustion"
  (allocate-rom-modules (list core (make-rom-module 'extra 'resident '("x.fs")))
                        (delete 1 (iota 64))))

(let* ((many (cons core (map (lambda (n)
                               (make-rom-module (string->symbol (format #f "mod~a" n))
                                                'resident '("x.fs"))) (iota 8))))
       (layout (allocate-rom-modules many)))
  (test-equal "allocation skips storage page" '(1 3 4 5 7 9 10 11 12)
    (map rom-allocation-page layout)))

(let* ((layout (allocate-rom-modules (list core)))
       (entry (car layout))
       (payload-size (- module-page-size (string-length "\nMENU-DEMO\n") 1)))
  (test-equal "exact source-page fit includes EOF" module-page-size
    (length (module-source-bytes entry layout
              (lambda (file) (make-string payload-size #\space)))))
  (test-error "oversized source rejected"
    (module-source-bytes entry layout
      (lambda (file) (make-string (1+ payload-size) #\space))))
  (test-error "embedded EOF rejected"
    (module-source-bytes entry layout (lambda (file) "hello\x00world"))))

(let* ((extra (make-rom-module 'editor 'resident '("edit.fs")))
       (layout (allocate-rom-modules (append rom-modules (list extra))))
       (workbench (find (lambda (entry)
                          (eq? 'workbench (rom-module-name (rom-allocation-module entry)))) layout))
       (tool (find (lambda (entry)
                    (eq? 'tests (rom-module-name (rom-allocation-module entry)))) layout)))
  (test-equal "appended resident receives control past tool" "\n9 LOAD-MODULE\n"
    (allocation-text workbench layout empty-source))
  (test-equal "only final resident starts desktop" "\nMENU-DEMO\n"
    (allocation-text (last layout) layout empty-source))
  (test-equal "tool does not chain or activate" "\n"
    (allocation-text tool layout empty-source)))

(let* ((joined (make-rom-module 'core 'resident '("one.fs" "two.fs")))
       (layout (allocate-rom-modules (list joined)))
       (bytes (module-source-bytes (car layout) layout
                                  (lambda (file) (if (string=? file "one.fs") "ONE" "TWO")))))
  (test-equal "joined sources have one EOF" 1 (count zero? bytes))
  (test-equal "file separator prevents joined tokens" "ONE\nTWO\nMENU-DEMO\n"
    (utf8->string (u8-list->bytevector (drop-right bytes 1)))))

(for-each
 (lambda (entry)
   (let ((bytes (module-source-bytes entry module-layout
                  (lambda (file) (read-module-file (string-append "src/" file))))))
     (test-assert (format #f "actual ~a module fits"
                          (rom-module-name (rom-allocation-module entry)))
       (<= (length bytes) module-page-size))))
 module-layout)

(test-equal "upgrade includes all code, excludes journal"
  '(0 1 2 3 4 5 7 60) (rom-upgrade-pages))
(test-end "rom-modules")
(exit (if (zero? (+ (test-runner-fail-count runner) (test-runner-xpass-count runner))) 0 1))
