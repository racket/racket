(load-relative "loadtest.rktl")

(Section 'expobs)

(require '#%expobs)

(define generate-expobs-regression? #f)
(define checking-against-old-expander? #f)

(define expobs-traces-path
  (build-path (current-load-relative-directory)
	      "expobs-regression.rktd"))

(define (get-trace e)
  (struct stx-boundary (v) #:prefab)
  
  (define (stx-essence s)
    (define syms (make-hasheq))
    (define (stx-essence s)
      (cond
        [(syntax? s)
         (if checking-against-old-expander?
             (stx-essence (syntax->datum s))
             ;; We care about the outer countour of pairs versus syntax objects,
             ;; but not the interior details:
             (stx-boundary (stx-essence (syntax->datum s))))]
        [(pair? s) (cons (stx-essence (car s))
                         (stx-essence (cdr s)))]
        [(symbol? s) (or (hash-ref syms s #f)
                         (let ([new-s (string->symbol (format "s~a" (hash-count syms)))])
                           (hash-set! syms s new-s)
                           new-s))]
        [(or (number? s) (boolean? s) (keyword? s) (null? s)) s]
        [else '#:opaque]))
    (stx-essence s))

  (define trace '())
  (parameterize ([current-expand-observe (lambda (num args)
                                           (set! trace (cons (cons num (stx-essence args)) trace)))])
    (with-handlers ([exn:fail:syntax? void]) ; syntax error ok: check trace up to error
      (expand (if (and (pair? e) (eq? 'module (car e)))
                  e
                  `(#%expression ,e)))))
  (let ([l (reverse trace)])
    l))

;; ----------------------------------------

(when generate-expobs-regression?
  (define new-expected-traces (make-hash))
  
  (define (generate-trace e)
    (hash-set! new-expected-traces e (get-trace e)))

  (for-each generate-trace
            '((#%top . __x)
              __x
              (#%plain-app 1 2)
              (quote-syntax (stx-quoted))
              (quote quoted)
              (set! __x 99)
              (letrec-values ([(x) __y] [(y z) __w]) __x)
              (let-values ([(x) __y] [(y z) __w]) __x)
              (begin 1 __x (+ 3 4))
              (case-lambda [(x) x] [(x y) (+ x y)])
              (#%variable-reference __z)
              (begin0 '3 '5)
              (with-continuation-mark __x __y __z)
              (if 1 2 3)
              (lambda (x)
                (define y (+ x x))
                y)
              (let ()
                (define (ok x) '8)
                (ok 5))
              (let ()
                (define (ok x) '8)
                (define (second y) (ok y))
                (second 5))
              (let ()
                (define (ok x) (second x))
                (define (second y) 8)
                (ok 5))
              (let ()
                (define (first z) z)
                (define (ok x) (second x))
                (printf "extra expression\n")
                (define (second y) 8)
                (ok (first 5)))
              (#%stratified-body
                (define (first z) z)
                (define (ok x) (second x))
                (define (second y) 8)
                (ok (first 5)))
              (#%stratified-body
               (define (first z) z)
               (define (ok x) (second x))
               (define (second y) 8)
               (ok (first 5))
               ;; syntax error:
               (define more 'oops))
              (let ()
                (define-syntax (ok stx) (quote-syntax 8))
                (ok 5))
              (let ()
                (define-syntax (ok stx) (quote-syntax 8))
                (define-syntax (second stx) (quote-syntax (ok 6)))
                (second 5))
              (let ()
                (define-syntax (ok stx) (quote-syntax 8))
                (define (ident x) x)
                9)
              (let ()
                (define-syntax (ok stx) (quote-syntax 8))
                (define-syntax (second stx) (quote-syntax (ok 6)))
                (define (ident x) x)
                (define (second-ident y) y)
                (ident (second-ident (second))))
              (let ()
                (define-syntax-rule (ok x) x)
                (ok 5))
              (let ()
                (define-syntax (ok stx)
                  (local-expand (cadr (syntax-e stx)) 'expression #f))
                (ok 9))
              (let ()
                (define-syntax (ok stx)
                  (define-values (exp opaque)
                    (syntax-local-expand-expression (cadr (syntax-e stx))))
                  opaque)
                (#%expression (ok 9)))
              (let ()
                (define-syntax (lift stx)
                  (syntax-local-lift-expression #'(+ 1 2)))
                (lift))
              (let ()
                (define-syntax (lift stx)
                  (syntax-local-lift-require 'racket/list #'foldl))
                (lift))
              (module m '#%kernel
                5)
              (module m racket/base
                'done)
              (module m racket/base
                (define (proc x) x)
                (provide proc))
              (module m racket/base
                (define-syntax (ok stx) (quote-syntax 8))
                (ok)
                (list (ok) (ok)))
              (module m racket/base
                (require racket/list)
                foldl)
              (module m racket/base
                (define-syntax (ok stx)
                  (syntax-local-lift-require 'racket/list #'foldl))
                (ok))
              ))

  (call-with-output-file expobs-traces-path
    #:exists 'truncate
    (lambda (o)
      ((dynamic-require 'racket/pretty 'pretty-write) new-expected-traces o))))

;; ----------------------------------------

(define expected-traces
  (call-with-input-file expobs-traces-path read))

(define (trace-equal? t1 t2)
  (unless (= (length t1) (length t2))
    (printf "trace lengths differ\n"))
  (for ([v1 (in-list t1)]
        [v2 (in-list t2)]
        [i (in-naturals)])
    (unless (equal? v1 v2)
      (printf "different at ~a: ~s ~s\n" i v1 v2)))
  (equal? t1 t2))

(for ([(e trace) (in-hash expected-traces)])
  (test #t `(trace ,e) (trace-equal? (get-trace e) trace)))

;; ----------------------------------------

(report-errs)
