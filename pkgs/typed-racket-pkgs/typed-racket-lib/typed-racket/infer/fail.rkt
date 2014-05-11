#lang racket/base
(provide (all-defined-out) early-return)
(require (for-syntax racket/base syntax/parse racket/syntax) "../utils/early-return.rkt"
         racket/match)

;; like `match*`, but with `early-return` around the rhss
(define-syntax (match*/early stx)
  (define-syntax-class rhs
    (pattern (#:when e rhs ...)
             #:with r
             #'(#:when e (early-return rhs ...)))
    (pattern (rhs ...)
             #:with r
             #'((early-return rhs ...))))
 (syntax-parse stx
  [(_ e [c . r:rhs] ...)
   #'(match* e [c . r.r] ...)]))

(begin-for-syntax
  (define-splicing-syntax-class arg
    #:attributes (v name (arg 1))
    (pattern v:expr
      #:with name (generate-temporary #'v)
      #:with (arg ...) #'(name))
    (pattern (~seq kw:keyword v:expr)
      #:with name (generate-temporary #'v)
      #:with (arg ...) #'(kw name))))


;; (% f e ...) == (and e ... (f e ...)) but without repeated evaluation
(define-syntax (% stx)
  (syntax-parse stx
    [(_ f e:arg ...)
     #'(let/fail ([e.name e.v] ...)
         (f e.arg ... ...))]))

;; (%1 f e0 e ...) == (and e0 (f e0 e ...)) but without repeated evaluation
(define-syntax (%1 stx)
  (syntax-parse stx
    [(_ f e0:arg e:arg ...)
     #'(let/fail ([e0.name e0.v])
         (let ([e.name e.v] ...)
           (f e0.arg ... e.arg ... ...)))]))

;; like `let`, but if any bindings are #f, the whole expression produces #f
(define-syntax (let/fail stx)
  (syntax-parse stx
    [(let/fail () e ...) #'(let () e ...)]
    [(let/fail ([x rhs ...] . rest) body ...)
     #'(let ([x rhs ...])
         (and x
              (let/fail rest body ...)))]))

;; (for/list/fail e ...) == (and (andmap values (for/list e ...)) (for/list e ...)
;; but without wasted work
(define-syntax-rule (for/list/fail (cl ...) body ...)
  (% reverse
     (for/fold ([result null]) (cl ... #:break (not result))
       (let ([e (let () body ...)])
         (and e (cons e result))))))

;; like hash-union, but if combine ever produces #f, the overall result is #f
(define (hash-union/fail #:combine combine one two)
  (for/fold ([one one]) ([(k v) (in-hash two)] #:break (not one))
    (define d (if (hash-has-key? one k)
                  (combine (hash-ref one k) v)
                  v))
    (and d (hash-set one k d))))
