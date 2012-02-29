#lang racket/base
(require syntax/parse/private/residual ;; keep abs. path
         (only-in syntax/parse/private/residual-ct ;; keep abs. path
                  attr-name attr-depth)
         "kws.rkt")
(provide reflect-parser
         (struct-out reified)
         (struct-out reified-syntax-class)
         (struct-out reified-splicing-syntax-class))

#|
A Reified is
  (reified symbol ParserFunction nat (listof (list symbol nat)))
|#
(define-struct reified-base (name) #:transparent)
(define-struct (reified reified-base) (parser arity signature))
(define-struct (reified-syntax-class reified) ())
(define-struct (reified-splicing-syntax-class reified) ())

(define (reflect-parser obj e-arity e-attrs splicing?)
  ;; e-arity represents single call; min and max are same
  (define who (if splicing? 'reflect-splicing-syntax-class 'reflect-syntax-class))
  (if splicing?
      (unless (reified-splicing-syntax-class? obj)
        (raise-type-error who "reified splicing-syntax-class" obj))
      (unless (reified-syntax-class? obj)
        (raise-type-error who "reified syntax-class" obj)))
  (check-params who e-arity (reified-arity obj) obj)
  (adapt-parser who
                (for/list ([a (in-list e-attrs)])
                  (list (attr-name a) (attr-depth a)))
                (reified-signature obj)
                (reified-parser obj)
                splicing?))

(define (check-params who e-arity r-arity obj)
  (let ([e-pos (arity-minpos e-arity)]
        [e-kws (arity-minkws e-arity)])
    (check-arity/neg r-arity e-pos e-kws
                     (lambda (msg)
                       (raise-mismatch-error who (string-append msg ": ") obj)))))

(define (adapt-parser who esig0 rsig0 parser splicing?)
  (if (equal? esig0 rsig0)
      parser
      (let ([indexes
             (let loop ([esig esig0] [rsig rsig0] [index 0])
               (cond [(null? esig)
                      null]
                     [(and (pair? rsig) (eq? (caar esig) (caar rsig)))
                      (unless (= (cadar esig) (cadar rsig))
                        (wrong-depth who (car esig) (car rsig)))
                      (cons index (loop (cdr esig) (cdr rsig) (add1 index)))]
                     [(and (pair? rsig)
                           (string>? (symbol->string (caar esig))
                                     (symbol->string (caar rsig))))
                      (loop esig (cdr rsig) (add1 index))]
                     [else
                      (error who "reified syntax-class is missing declared attribute `~s'"
                             (caar esig))]))])
        (define (take-indexes result indexes)
          (let loop ([result result] [indexes indexes] [i 0])
            (cond [(null? indexes) null]
                  [(= (car indexes) i)
                   (cons (car result) (loop (cdr result) (cdr indexes) (add1 i)))]
                  [else
                   (loop (cdr result) indexes (add1 i))])))
        (make-keyword-procedure
         (lambda (kws kwargs x cx pr es fh cp rl success . rest)
           (keyword-apply parser kws kwargs x cx pr es fh cp rl
                          (if splicing?
                              (lambda (fh x cx pr . result)
                                (apply success fh x cx pr (take-indexes result indexes)))
                              (lambda (fh . result)
                                (apply success fh (take-indexes result indexes))))
                          rest))))))

(define (wrong-depth who a b)
  (error who
         "reified syntax-class has wrong depth for attribute `~s'; expected ~s, got ~s instead"
         (car a) (cadr a) (cadr b)))
