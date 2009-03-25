#lang scheme/base

(require scheme/list
         scheme/path
         (only-in mzlib/etc this-expression-source-directory)
         (planet schematics/sake:1)
         "test.ss"
         "text-ui.ss")

(define here (this-expression-source-directory))
(define package-name
  (string-append (path->string (last (explode-path here))) ".plt"))


;; We can't use the Sake action:test as we want to use the
;; test.ss in this directory, not the one installed in the
;; PLaneT cache.
(define (schemeunit-action:test module-path binding)
  (let ([n-failures (run-tests (dynamic-require module-path binding))])
    (if (zero? n-failures)
        #t
        (raise
         (make-exn:fail (format "~a test(s) failed" n-failures)
                        (current-continuation-marks))))))

(define-task compile
  ()
  (action:compile "all-tests.ss"))

(define-task test
  (compile)
  (schemeunit-action:test "all-schemeunit-tests.ss" 'all-schemeunit-tests))

(define-task planet-install
  (test)
  (when (file-exists? "schemeunit.plt")
    (delete-file "schemeunit.plt"))
  (action:planet-archive ".")
  (rename-file-or-directory package-name "schemeunit.plt")
  (action:planet-remove "schematics" "schemeunit.plt" 3 4)
  (action:planet-install "schematics" "schemeunit.plt" 3 4))

(define-task all
  (planet-install))

(define-task default
  (all))
