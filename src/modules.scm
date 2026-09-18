(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (ice-9 format)
             (ice-9 regex)
             (ice-9 textual-ports)
             (rnrs bytevectors))

;; Source modules and physical allocations are distinct: consumers use names.
(define-record-type <rom-module>
  (make-rom-module name kind files)
  rom-module?
  (name rom-module-name)
  (kind rom-module-kind)
  (files rom-module-files))

(define-record-type <rom-allocation>
  (make-rom-allocation page module)
  rom-allocation?
  (page rom-allocation-page)
  (module rom-allocation-module))

(define rom-modules
  (list
   (make-rom-module 'core 'resident '("bootstrap-flash1.fs"))
   (make-rom-module 'storage 'resident '("os-services.fs" "os-storage.fs"))
   (make-rom-module 'desktop 'resident '("os-tasks.fs" "os-catalog.fs" "os-desktop.fs"))
   (make-rom-module 'workbench 'resident '("bootstrap-flash5.fs"))
   (make-rom-module 'tests 'tool '("bootstrap-flash4.fs"))))

;; Kernel, fixed-RAM template, object journal, swap sector, and boot/unlock code.
(define reserved-pages '(0 2 8 56 57 58 59 60 61 62 63))
(define module-page-size #x4000)

(define (validate-rom-modules modules)
  (unless (and (list? modules) (pair? modules) (every rom-module? modules))
    (error "ROM manifest must contain module records"))
  (for-each
   (lambda (module)
     (unless (and (symbol? (rom-module-name module))
                  (<= (string-length (symbol->string (rom-module-name module))) 24)
                  (string-match "^[a-z][a-z0-9-]*$"
                                (symbol->string (rom-module-name module))))
       (error "Invalid ROM module name" (rom-module-name module)))
     (unless (memq (rom-module-kind module) '(resident tool))
       (error "Invalid ROM module kind" (rom-module-kind module)))
     (unless (and (list? (rom-module-files module))
                  (pair? (rom-module-files module))
                  (every (lambda (file) (and (string? file) (not (string-null? file))))
                         (rom-module-files module)))
       (error "ROM module requires source filenames" (rom-module-name module))))
   modules)
  (let ((names (map rom-module-name modules)))
    (unless (= (length names) (length (delete-duplicates names eq?)))
      (error "Duplicate ROM module names" names)))
  (unless (and (eq? (rom-module-name (car modules)) 'core)
               (eq? (rom-module-kind (car modules)) 'resident))
    (error "First ROM module must be resident core (bootstrap page 1)")))

(define* (allocate-rom-modules modules #:optional (reserved reserved-pages))
  (validate-rom-modules modules)
  (unless (and (list? reserved)
               (every (lambda (page) (and (exact-integer? page) (<= 0 page 63))) reserved)
               (= (length reserved) (length (delete-duplicates reserved)))
               (every (lambda (page) (memv page reserved)) reserved-pages)
               (not (memv 1 reserved)))
    (error "Invalid reserved ROM pages" reserved))
  (let loop ((remaining modules) (page 1) (result '()))
    (cond ((null? remaining) (reverse result))
          ((>= page 64) (error "ROM module pages exhausted"))
          ((memv page reserved) (loop remaining (1+ page) result))
          (else (loop (cdr remaining) (1+ page)
                      (cons (make-rom-allocation page (car remaining)) result))))))

(define module-layout (allocate-rom-modules rom-modules))

(define* (module-page name #:optional (layout module-layout))
  (let ((entry (find (lambda (entry)
                      (eq? (rom-module-name (rom-allocation-module entry)) name))
                    layout)))
    (if entry (rom-allocation-page entry) (error "Unknown ROM module" name))))

(define (read-module-file file)
  (call-with-input-file file get-string-all))

;; A module has one EOF, regardless of how many source files it contains.
;; Activation follows the final resident, so appending residents is safe.
(define* (module-source-bytes entry #:optional (layout module-layout)
                              (read-source read-module-file))
  (unless (memq entry layout) (error "Module allocation is outside its layout"))
  (let* ((module (rom-allocation-module entry))
         (residents (filter (lambda (allocation)
                              (eq? (rom-module-kind (rom-allocation-module allocation))
                                   'resident)) layout))
         (tail (memq entry residents))
         (trailer (cond ((not tail) "\n")
                        ((pair? (cdr tail))
                         (format #f "\n~a LOAD-MODULE\n"
                                 (rom-allocation-page (cadr tail))))
                        (else "\nMENU-DEMO\n")))
         (source (string-join (map read-source (rom-module-files module)) "\n")))
    (when (string-index source #\nul)
      (error "ROM module source contains an embedded EOF" (rom-module-name module)))
    (let* ((encoded (string->utf8 (string-append source trailer)))
           (size (1+ (bytevector-length encoded))))
      (when (> size module-page-size)
        (error "ROM module exceeds one 16KiB source page; split its file group"
               (rom-module-name module) size))
      (append (bytevector->u8-list encoded) '(0)))))

(define (module-source entry)
  `((db ,(module-source-bytes entry))))

;; The upgrade contains code/template pages, never writable object storage.
(define* (rom-upgrade-pages #:optional (layout module-layout))
  (sort (delete-duplicates (append '(0 2 60) (map rom-allocation-page layout))) <))
