#lang scheme/base
(require (for-syntax scheme/base))
(provide match)

(define-syntax-rule (match stx clause ...)
  (let ([x stx]) (match-c x clause ...)))

(define-syntax match-c
  (syntax-rules ()
    [(match-c x)
     (error 'minimatch "match failed: ~s" x)]
    [(match-c x [pattern result ...] clause ...)
     (let ([fail (lambda () (match-c x clause ...))])
       (match-p x pattern (let () result ...) (fail)))]))

;; (match-p id Pattern SuccessExpr FailureExpr)
(define-syntax (match-p stx)
  (syntax-case stx (quote cons list)
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
    [(match-p x var success failure)
     (identifier? #'var)
     #'(let ([var x]) success)]
    [(match-p x s success failure)
     (prefab-struct-key (syntax-e #'s))
     (with-syntax ([key (prefab-struct-key (syntax-e #'s))]
                   [(p ...) (cdr (vector->list (struct->vector (syntax-e #'s))))])
       #'(let ([xkey (prefab-struct-key x)])
           (if (equal? xkey 'key)
               (let ([xps (cdr (vector->list (struct->vector x)))])
                 (match-p xps (list p ...) success failure))
               failure)))]))
