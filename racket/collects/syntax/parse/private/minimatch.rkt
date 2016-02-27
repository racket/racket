#lang racket/base
(require racket/struct
         (for-syntax racket/base racket/struct-info racket/struct))
(provide match ?)

(define-syntax (match stx)
  (syntax-case stx ()
    [(match e clause ...)
     #`(let ([x e])
         (match-c x
           clause ...
           [_ (error 'minimatch "match at ~s:~s:~s failed: ~e"
                     '#,(syntax-source stx)
                     '#,(syntax-line stx)
                     '#,(syntax-column stx)
                     x)]))]))

(define-syntax match-c
  (syntax-rules ()
    [(match-c x)
     (error 'minimatch)]
    [(match-c x [pattern result ...] clause ...)
     (let ([fail (lambda () (match-c x clause ...))])
       (match-p x pattern (let () result ...) (fail)))]))

;; (match-p id Pattern SuccessExpr FailureExpr)
(define-syntax (match-p stx)
  (syntax-case stx (quote cons list vector STRUCT ?)
    [(match-p x wildcard success failure)
     (and (identifier? #'wildcard) (free-identifier=? #'wildcard #'_))
     #'success]
    [(match-p x (quote lit) success failure)
     #'(if (equal? x (quote lit))
           success
           failure)]
    [(match-p x (cons p1 p2) success failure)
     #'(if (pair? x)
           (let ([x1 (car x)]
                 [x2 (cdr x)])
             (match-p x1 p1 (match-p x2 p2 success failure) failure))
           failure)]
    [(match-p x (list) success failure)
     #'(match-p x (quote ()) success failure)]
    [(match-p x (list p1 p ...) success failure)
     #'(match-p x (cons p1 (list p ...)) success failure)]
    [(match-p x (vector p ...) success failure)
     #'(if (and (vector? x) (= (vector-length x) (length '(p ...))))
           (let ([x* (vector->list x)])
             (match-p x* (list p ...) success failure))
           failure)]
    [(match-p x var success failure)
     (identifier? #'var)
     #'(let ([var x]) success)]
    [(match-p x (STRUCT S (p ...)) success failure)
     (identifier? #'S)
     (let ()
       (define (not-a-struct)
         (raise-syntax-error #f "expected struct name" #'S))
       (define si (syntax-local-value #'S not-a-struct))
       (unless (struct-info? si)
         (not-a-struct))
       (let* ([si (extract-struct-info si)]
              [predicate (list-ref si 2)]
              [accessors (reverse (list-ref si 3))])
         (unless (andmap identifier? accessors)
           (raise-syntax-error #f "struct has incomplete information" #'S))
         (with-syntax ([predicate predicate]
                       [(accessor ...) accessors])
           #'(if (predicate x)
                 (let ([y (list (accessor x) ...)])
                   (match-p y (list p ...) success failure))
                 failure))))]
    [(match-p x (? predicate pat ...) success failure)
     #'(if (predicate x)
           (match-p* ((x pat) ...) success failure)
           failure)]
    [(match-p x (S p ...) success failure)
     (identifier? #'S)
     (if (struct-info? (syntax-local-value #'S (lambda () #f)))
         #'(match-p x (STRUCT S (p ...)) success failure)
         (raise-syntax-error #f "bad minimatch form" stx #'S))]
    [(match-p x s success failure)
     (prefab-struct-key (syntax-e #'s))
     (with-syntax ([key (prefab-struct-key (syntax-e #'s))]
                   [(p ...) (struct->list (syntax-e #'s))])
       #'(let ([xkey (prefab-struct-key x)])
           (if (equal? xkey 'key)
               (let ([xps (struct->list x)])
                 (match-p xps (list p ...) success failure))
               failure)))]
    ))

(define-syntax match-p*
  (syntax-rules ()
    [(match-p* () success failure)
     success]
    [(match-p* ((x1 p1) . rest) success failure)
     (match-p x1 p1 (match-p* rest success failure) failure)]))

(define-syntax ?
  (lambda (stx)
    (raise-syntax-error #f "illegal use of minimatch form '?'" stx)))

(define-syntax STRUCT #f) ;; internal keyword
