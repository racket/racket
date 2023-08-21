#lang racket/base
(require (for-syntax racket/base racket/list racket/struct-info))
(provide match match-lambda ?)

(define-syntax (match-lambda stx)
  (syntax-case stx ()
    [(match-lambda clause ...)
     #`(lambda (x)
         (match-c x
           clause ...
           [_ (error 'minimatch-lambda "match at ~s:~s:~s failed: ~e"
                     '#,(syntax-source stx)
                     '#,(syntax-line stx)
                     '#,(syntax-column stx)
                     x)]))]))

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
    [(match-c x) (void)]
    [(match-c x [pattern result ...] clause ...)
     (let ([fail (lambda () (match-c x clause ...))])
       (match-p x pattern (let () result ...) (fail)))]))

;; (match-p id Pattern SuccessExpr FailureExpr)
(define-syntax (match-p stx)
  (syntax-case stx (quote cons list list* vector ?)
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
    [(match-p x (list* p) success failure)
     #'(match-p x p success failure)]
    [(match-p x (list* p1 p ...) success failure)
     #'(match-p x (cons p1 (list* p ...)) success failure)]
    [(match-p x (vector p ...) success failure)
     (with-syntax ([(i ...) (range (length (syntax->list #'(p ...))))])
       #'(if (and (vector? x) (= (vector-length x) (length '(p ...))))
             (match-ep* ([(vector-ref x 'i) p] ...) success failure)
             failure))]
    [(match-p x var success failure)
     (identifier? #'var)
     #'(let ([var x]) success)]
    [(match-p x (? predicate pat ...) success failure)
     #'(if (predicate x)
           (match-ep* ((x pat) ...) success failure)
           failure)]
    [(match-p x (S p ...) success failure)
     (identifier? #'S)
     (let ()
       (define si (syntax-local-value #'S (lambda () #f)))
       (unless (struct-info? si) (raise-syntax-error #f "bad minimatch form" stx #'S))
       (let* ([si (extract-struct-info si)]
              [predicate (list-ref si 2)]
              [accessors (reverse (list-ref si 3))])
         (unless (andmap identifier? accessors)
           (raise-syntax-error #f "struct has incomplete information" #'S))
         (unless (= (length accessors) (length (syntax->list #'(p ...))))
           (raise-syntax-error #f "struct pattern has incorrect number of subpatterns" #'S))
         (with-syntax ([predicate predicate]
                       [(accessor ...) accessors])
           #'(if (predicate x)
                 (match-ep* ([(accessor x) p] ...) success failure)
                 failure))))]
    [(match-p x pattern success failure)
     (raise-syntax-error 'minimatch "bad pattern" #'pattern)]
    ))

(define-syntax match-ep*
  (syntax-rules ()
    [(match-ep* () success failure)
     success]
    [(match-ep* ((e1 p1) . rest) success failure)
     (let ([y e1]) (match-p y p1 (match-ep* rest success failure) failure))]))

(define-syntax ?
  (lambda (stx)
    (raise-syntax-error #f "illegal use of minimatch form '?'" stx)))
