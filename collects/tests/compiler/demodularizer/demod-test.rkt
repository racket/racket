#lang racket
(require tests/eli-tester
         racket/runtime-path)

(define (capture-output command . args)
  (define o (open-output-string))
  (define e (open-output-string))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-output-port o]
                 [current-error-port e])
    (apply system* command args))
  (values (get-output-string o) (get-output-string e)))

(define (test-on-program filename)
  ; run modular program, capture output
  (define-values (modular-output modular-error)
    (capture-output (find-executable-path "racket") filename))
  
  ; demodularize
  (parameterize ([current-input-port (open-input-string "")])
    (system* (find-executable-path "raco") "demod" filename))
  
  (define demod-filename 
    (path->string
     (path-add-suffix filename #".merged.rkt")))
  
  ; run whole program
  (define-values (whole-output whole-error)
    (capture-output (find-executable-path "racket") demod-filename))
  
  (display whole-error)
  
  ; compare output 
  (test
   #:failure-prefix (format "~a stdout" filename)
   whole-output => modular-output
   #:failure-prefix (format "~a stderr" filename)
   whole-error => modular-error))

(define-runtime-path tests "tests")

(define (modular-program? filename)
  (and (not (regexp-match #rx"merged" filename))
       (regexp-match #rx"rkt$" filename)))

(test-on-program "/Users/blake/Development/plt/collects/tests/compiler/demodularizer/tests/racket-5.rkt")

#;(test
 (for ([i (in-list (directory-list tests))])
   (define ip (build-path tests i))
   (when (modular-program? ip)
     (printf "Checking ~a\n" ip)
     (test-on-program (path->string ip)))))