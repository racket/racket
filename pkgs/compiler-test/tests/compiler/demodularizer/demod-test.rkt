#lang racket
(require tests/eli-tester
         racket/runtime-path
         compiler/find-exe)

(define (capture-output command . args)
  (define o (open-output-string))
  (define e (open-output-string))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-output-port o]
                 [current-error-port e])
    (apply system* command args))
  (values (get-output-string o) (get-output-string e)))

(define (test-on-program filename [exceptions null])
  ;; run modular program, capture output
  (define-values (modular-output modular-error)
    (capture-output (find-exe) filename))
  
  (define demod-filename 
    (let-values ([(base filename dir?) (split-path filename)])
      (path->string
       (build-path
        (find-system-path 'temp-dir)
        (path-add-suffix filename #"_merged.zo")))))
  
  ;; demodularize
  (parameterize ([current-input-port (open-input-string "")])
    (apply system* (find-exe) "-l-" "raco" "demod" "-o" demod-filename
           (append exceptions
                   (list filename))))
  
  ;; run whole program
  (define-values (whole-output whole-error)
    (capture-output (find-exe) demod-filename))
  
  ;; compare output 
  (test
   #:failure-prefix (format "~a stdout" filename)
   whole-output => modular-output
   #:failure-prefix (format "~a stderr" filename)
   whole-error => modular-error)

  (when (null? exceptions)
    ;; try creating an executable
    (define exe-filename (build-path
                          (find-system-path 'temp-dir)
                          (if (eq? (system-type) 'windows)
                              "demod-exe.exe"
                              "demod-exe")))
    (system* (find-exe) "-l-" "raco" "exe" "-o" exe-filename demod-filename)
    (define-values (whole-exe-output whole-exe-error)
      (capture-output exe-filename))
    (test
     #:failure-prefix (format "~a exe stdout" filename)
     whole-exe-output => modular-output
     #:failure-prefix (format "~a exe stderr" filename)
     whole-exe-error => modular-error)))

(define-runtime-path tests "tests")

(define (modular-program? filename)
  (and (not (regexp-match #rx"merged" filename))
       (regexp-match #rx"rkt$" filename)))

(test
 (for ([i (in-list (directory-list tests))])
   (define ip (build-path tests i))
   (when (modular-program? ip)
     (printf "Checking ~a\n" ip)
     (test-on-program (path->string ip))
     (printf "Checking ~a, skip racket/private/pre-base\n" ip)
     (test-on-program (path->string ip)
                      (list "-e"
                            (path->string
                             (collection-file-path "pre-base.rkt" "racket/private")))))))
