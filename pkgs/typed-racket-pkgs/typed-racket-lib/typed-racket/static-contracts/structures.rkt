#lang racket/base

(require racket/match racket/list racket/generic 
         (except-in racket/contract recursive-contract)
         "kinds.rkt" "constraints.rkt")

(provide
  (contract-out
    (struct recursive-contract ([names (listof identifier?)]
                                [values (listof static-contract?)]
                                [body static-contract?]))
    (struct recursive-contract-use ([name identifier?]))
    (struct combinator ([args sequence?]))
    [sc-map (static-contract? (static-contract? variance/c . -> . static-contract?) . -> . static-contract?)]
    [sc->contract (static-contract? (static-contract? . -> . syntax?) . -> . syntax?)]
    [sc->constraints (static-contract? (static-contract? . -> . contract-restrict?) . -> . contract-restrict?)]
    [static-contract? predicate/c]
    [sc? predicate/c]
    )


  prop:combinator-name
  gen:sc)

(define variance/c (or/c 'covariant 'contravariant 'invariant))

(define (recursive-contract-write-proc v port mode)
  (match-define (recursive-contract names vals body) v)
  (define recur
    (case mode
      [(#t) write]
      [(#f) display]
      [else (lambda (p port) (print p port mode))]))
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "rec/sc")
  (display " (" port)
  (define (recur-pair name val)
    (fprintf port "(~a " (syntax->datum name))
    (recur val port)
    (display ")" port))
  (recur-pair (first names) (first vals))
  (for ((name (rest names))
        (val (rest vals)))
       (display " " port)
       (recur-pair name val))
  (display ") " port)
  (recur body port)
  (display close port))

(define (recursive-contract-use-write-proc v port mode)
  (display (syntax->datum (recursive-contract-use-name v)) port))

(define (combinator-write-proc v port mode)
  (match-define (combinator args) v)
  (define name (combinator-name v))
  (define recur
    (case mode
      [(#t) write]
      [(#f) display]
      [else (lambda (p port) (print p port mode))]))
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port name)
  (for ((arg args))
       (display " " port)
       (recur arg port))
  (display close port))

(define-values (prop:combinator-name
                has-combinator-name?
                combinator-name)
  (make-struct-type-property 'combinator-name
    (lambda (v _) 
      (unless (string? v)
        (raise-argument-error
          'prop:combinator-name
          "string?"
          v))
      v)))

(define-generics sc
  [sc-map sc f]
  [sc->contract sc f]
  [sc->constraints sc f])


(struct static-contract ()
        #:transparent
        #:property prop:custom-print-quotable 'never)

(struct recursive-contract static-contract (names values body)
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f)
             (match v
               [(recursive-contract names values body)
                (recursive-contract names (map (λ (v) (f v 'covariant)) values) (f body 'covariant))]))]
        #:methods gen:custom-write [(define write-proc recursive-contract-write-proc)])

(struct recursive-contract-use static-contract (name)
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc->contract v f) (recursive-contract-use-name v))
           (define (sc->constraints v f) (variable-contract-restrict (recursive-contract-use-name v)))]
        #:methods gen:custom-write [(define write-proc recursive-contract-use-write-proc)])

(struct combinator static-contract (args)
        #:transparent
        #:property prop:combinator-name "combinator/sc"
        #:methods gen:custom-write [(define write-proc combinator-write-proc)])
