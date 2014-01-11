#lang racket/base

;; Internal structures for representing a static contract.

(require racket/match racket/list racket/generic 
         racket/contract
         "kinds.rkt" "constraints.rkt")

(provide
  (contract-out
    (struct recursive-sc ([names (listof identifier?)]
                          [values (listof static-contract?)]
                          [body static-contract?]))
    (struct recursive-sc-use ([name identifier?]))
    (struct combinator ([args sequence?]))
    (struct static-contract ())
    [sc-map (static-contract? (static-contract? variance/c . -> . static-contract?) . -> . static-contract?)]
    [sc-traverse (static-contract? (static-contract? variance/c . -> . any/c) . -> . void?)]
    [sc->contract (static-contract? (static-contract? . -> . syntax?) . -> . syntax?)]
    [sc->constraints (static-contract? (static-contract? . -> . contract-restrict?) . -> . contract-restrict?)]
    [sc-terminal-kind (static-contract? . -> . (or/c #f contract-kind?))]
    [sc? predicate/c]
    )


  prop:combinator-name
  gen:sc)

(define variance/c (or/c 'covariant 'contravariant 'invariant))

(define (recursive-sc-write-proc v port mode)
  (match-define (recursive-sc names vals body) v)
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
  (when (cons? names)
    (recur-pair (first names) (first vals))
    (for ((name (rest names))
          (val (rest vals)))
         (display " " port)
         (recur-pair name val)))
  (display ") " port)
  (recur body port)
  (display close port))

(define (recursive-sc-use-write-proc v port mode)
  (display (syntax->datum (recursive-sc-use-name v)) port))

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

;; Functionality that all static contracts should support
(define-generics sc
  ;; sc-map: static-contract? (static-contract? variance/c -> static-contract?) -> static-contract?
  ;; Takes a static contract and returns a similar one.
  ;; Each sub part should be replaced with the value of calling the supplied function on it. The
  ;; variance argument should be how the sub part relates to the static contract.
  [sc-map sc f]
  ;; sc-traverse: static-contract? (static-contract? variance/c -> any/c) -> void?
  ;; Takes a static contract and traverses it.  Each sub part should be called with supplied function.
  ;; The variance argument should be how the sub part relates to the static contract.
  [sc-traverse sc f]
  ;; sc->contract: static-contract? (static-contract? -> contract?) -> contract?
  ;; Takes a static contract and returns the corresponding contract.
  ;; The function argument should be used for sub parts of the static contract.
  [sc->contract sc f]
  ;; sc->constraints: static-contract? (static-contract? -> constraint-set?) -> constraint-set?
  ;; Takes a static contract and computes the constraint set for a static contract.
  ;; The function argument should be used for sub parts of the static contract.
  [sc->constraints sc f]
  ;; sc-terminal-kind: static-contract? -> (or/c #f contract-kind?)
  ;; Returns the kind of contract that this represents
  ;; Returns #f if it is not a terminal contract
  [sc-terminal-kind sc]
  #:fallbacks
  [(define (sc-terminal-kind v) #f)])

;; Super struct of static contracts
(struct static-contract ()
        #:transparent
        #:property prop:custom-print-quotable 'never)

;; Represents a recursive contract.
;; In each value and the body, each name is bound to a the corresponding value contract.
;; - names : (listof identifier?)
;; - values : (listof static-contract?)
;; - body : static-contract?
;; names and value must have the same length.
(struct recursive-sc static-contract (names values body)
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f)
             (match v
               [(recursive-sc names values body)
                (recursive-sc names (map (λ (v) (f v 'covariant)) values) (f body 'covariant))]))
           (define (sc-traverse v f)
             (match v
               [(recursive-sc names values body)
                (for-each (λ (v) (f v 'covariant)) values)
                (f body 'covariant)
                (void)]))]
        #:methods gen:custom-write [(define write-proc recursive-sc-write-proc)])

;; A use of a contract bound by recursive-sc
;; - name : identifier?
(struct recursive-sc-use static-contract (name)
        #:transparent
        #:methods gen:sc
          [(define (sc-map v f) v)
           (define (sc-traverse v f) (void))
           (define (sc->contract v f) (recursive-sc-use-name v))
           (define (sc->constraints v f) (variable-contract-restrict (recursive-sc-use-name v)))]
        #:methods gen:custom-write [(define write-proc recursive-sc-use-write-proc)])

;; Super struct of static contract combinators.
;; Provides printing functionality.
;; - args : (sequenceof static-contract?)
(struct combinator static-contract (args)
        #:transparent
        #:property prop:combinator-name "combinator/sc"
        #:methods gen:custom-write [(define write-proc combinator-write-proc)])
