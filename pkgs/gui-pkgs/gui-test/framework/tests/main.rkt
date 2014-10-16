#lang racket/base
(require launcher
         racket/cmdline
         racket/unit
         "debug.rkt"
         "test-suite-utils.rkt")

(define preferences-file (find-system-path 'pref-file))

(define old-preferences-file
  (let-values ([(base name _2) (split-path preferences-file)])
    (build-path base (string-append (path-element->string name) ".save"))))

(define-values (all-files interactive-files)
  (let* ([files (call-with-input-file (collection-file-path "README" "framework" "tests")
                  read)]
         [files (map (lambda (x)
                       (cond [(symbol? x) (symbol->string x)]
                             [(pair? x) (cons (car x) (map symbol->string
                                                           (cdr x)))]
                             [else (error "bad specs in README")]))
                     files)]
         [all (map (lambda (x) (if (pair? x) (cdr x) (list x))) files)]
         [interactive (map (lambda (x)
                             (if (and (pair? x) (eq? 'interactive (car x)))
                               (cdr x) '()))
                           files)])
    (values (apply append all) (apply append interactive))))

(define all? #f)
(define batch? #f) ; non-interactive (implied by no test-file args)
(define files-to-process null)
(define command-line-flags
  `((once-each
     [("-a" "--all")
      ,(lambda (flag) (set! all? #t))
      ("Run all of the tests")])
    (multi
     [("-o" "--only")
      ,(lambda (flag _only-these-tests)
         (set-only-these-tests! (cons (string->symbol _only-these-tests)
                                      (or (get-only-these-tests) null))))
      ("Only run test named <test-name>" "test-name")])))

(parse-command-line
 "framework-test" (current-command-line-arguments) command-line-flags
 (lambda (collected . files)
   (when (null? files) (set! batch? #t))
   (let* ([throwouts (remove* all-files files)]
          [files (remove* throwouts files)])
     (when (not (null? throwouts))
       (debug-printf admin "   ignoring files that don't occur in all-files: ~s\n" throwouts))
     (set! files-to-process
           (cond [all?   all-files]
                 [batch? (remove* interactive-files all-files)]
                 [else   files]))))
 `("Names of the tests; defaults to all non-interactive tests"))

(when (file-exists? preferences-file)
  (debug-printf admin "   saving prefs file ~a\n" preferences-file)
  (debug-printf admin "                  to ~a\n" old-preferences-file)
  (if (file-exists? old-preferences-file)
    (debug-printf admin "   backup prefs file exists, using that one\n")
    (begin (copy-file preferences-file old-preferences-file)
           (debug-printf admin "   saved prefs file\n"))))

(define jumped-out-tests '())

(for-each
 (lambda (x)
   (shutdown-mred)
   (load-framework-automatically #t)
   (let/ec k
     (dynamic-wind
       (lambda ()
         (set! jumped-out-tests (cons x jumped-out-tests))
         (set-section-name! x)
         (set-section-jump! k))
       (lambda ()
         (with-handlers ([(lambda (_) #t)
                          (lambda (exn)
                            (debug-printf schedule "~a\n"
                                          (if (exn? exn)
                                              (exn->str exn)
                                              exn)))])
           (debug-printf schedule "beginning ~a test suite\n" x)
           (dynamic-require `(lib ,x "framework" "tests") #f)
           (set! jumped-out-tests (remq x jumped-out-tests))
           (debug-printf schedule "PASSED ~a test suite\n" x)))
       (lambda ()
         (reset-section-name!)
         (reset-section-jump!)))))
 files-to-process)

(debug-printf schedule "ran ~a test~a\n" number-of-tests (if (= 1 number-of-tests) "" "s"))

(when (file-exists? old-preferences-file)
  (debug-printf admin "   restoring prefs file ~a\n" old-preferences-file)
  (debug-printf admin "                     to ~a\n" preferences-file)
  (delete-file preferences-file)
  (copy-file old-preferences-file preferences-file)
  (delete-file old-preferences-file)
  (debug-printf admin "   restored prefs file\n"))

(shutdown-listener)

(exit (cond
        [(not (null? jumped-out-tests))
         (eprintf "Test suites ended with exns ~s\n" jumped-out-tests)
         1]
        [(null? failed-tests)
         (printf "All tests passed.\n")
         0]
        [else
         (eprintf "FAILED tests:\n")
         (for-each (lambda (failed-test)
                     (eprintf "  ~a // ~a\n"
                              (car failed-test) (cdr failed-test)))
                   failed-tests)
         1]))

(module+ test
  (module config info
    (define timeout 360)))
