(use-modules (ice-9 format))

;; Source includes are relative to src; outputs belong to the caller's directory.
(let ((source-directory
       (string-append (dirname (canonicalize-path (current-filename))) "/src"))
      (output-directory (getcwd)))
  (dynamic-wind
    (lambda () (chdir source-directory))
    (lambda ()
      (load (string-append source-directory "/zkeme80.scm"))
      ((module-ref (current-module) 'make-rom+map)
       (string-append output-directory "/zkeme80.rom")
       (string-append output-directory "/zkeme80.ram-labelmap.json")))
    (lambda () (chdir output-directory))))
