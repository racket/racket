#lang racket/base
(require mzlib/pconvert
         racket/pretty
         lang/private/set-result)

(provide configure)

(define (configure options)
  ;; Set print-convert options:
  (booleans-as-true/false #t)
  (constructor-style-printing #t)
  (abbreviate-cons-as-list (memq 'abbreviate-cons-as-list options))
  (current-print-convert-hook
   (let ([ph (current-print-convert-hook)])
     (lambda (val basic sub)
       (cond
        [(equal? val set!-result) '(void)]
        [else (ph val basic sub)]))))
  (use-named/undefined-handler
   (lambda (x)
     (and (memq 'use-function-output-syntax options)
          (procedure? x)
          (object-name x))))
  (named/undefined-handler
   (lambda (x)
     (string->symbol
      (format "function:~a" (object-name x)))))
  ;; Set pretty-print options:
  (pretty-print-show-inexactness #t)
  (pretty-print-exact-as-decimal #t)

  ;; Set print handlers to use print-convert and pretty-print:
  (current-print
   (lambda (v)
     (unless (void? v)
       (pretty-print (print-convert v)))))
  (let ([orig (global-port-print-handler)])
    (global-port-print-handler
     (lambda (val port [depth 0])
       (parameterize ([global-port-print-handler orig])
         (let ([val (print-convert val)])
           (parameterize ([pretty-print-columns 'infinity])
             (pretty-print val port depth))))))))
