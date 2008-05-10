
#lang scheme/base
(require scheme/match
         scheme/list
         "deriv-c.ss"
         "deriv-util.ss")
(provide find-derivs
         find-deriv
         find-derivs/syntax
         extract-all-fresh-names
         flatten-identifiers)

;; Utilities for finding subderivations

;; find-derivs : (deriv -> boolean) (deriv -> boolean) deriv -> (list-of deriv)
(define (find-derivs pred stop-short d)
  (let ([stop (lambda (x) (or (pred x) (stop-short x)))])
    (find-deriv/unit+join+zero pred stop d list append null)))

;; find-deriv : (deriv -> boolean) (deriv -> boolean) deriv -> deriv/#f
;; Finds the first deriv that matches; throws the rest away
(define (find-deriv pred stop-short d)
  (let* ([stop-short (or stop-short (lambda (x) #f))]
         [stop (lambda (x) (or (pred x) (stop-short x)))])
    (let/ec return (find-deriv/unit+join+zero pred stop d return (lambda _ #f) #f))))

;; find-deriv/unit+join+zero
;; Parameterized over monad operations for combining the results
;; For example, <list, append, null> collects the results into a list
(define (find-deriv/unit+join+zero pred stop-short d unit join zero)
  (define (loop d)
    (if (pred d)
        (join (unit d) (loop-inner d))
        (loop-inner d)))
  (define (loop-inner d)
    (match d
      [(? stop-short d) zero]
      [(Wrap mrule (_ _ _ _ _ locals _ _ _ next))
       (join (loops locals) (loop next))]
      [(Wrap tagrule (_ _ _ next))
       (loop next)]
      [(Wrap lift-deriv (_ _ first lift second))
       (join (loop first) (loop second))]
      [(struct local-expansion (_ _ _ _ deriv _ _ _))
       (loop deriv)]
      [(struct local-bind (_ _ _ bindrhs))
       (loop bindrhs)]
      [(Wrap p:define-syntaxes (_ _ _ _ rhs _))
       (loop rhs)]
      [(Wrap p:define-values (_ _ _ _ rhs))
       (loop rhs)]
      [(Wrap p:#%expression (_ _ _ _ inner _))
       (loop inner)]
      [(Wrap p:if (_ _ _ _ test then else))
       (join (loop test) (loop then) (loop else))]
      [(Wrap p:wcm (_ _ _ _ key value body))
       (join (loop key) (loop value) (loop body))]
      [(Wrap p:set! (_ _ _ _ _ rhs))
       (loop rhs)]
      [(Wrap p:set!-macro (_ _ _ _ deriv))
       (loop deriv)]
      [(Wrap p:begin (_ _ _ _ lderiv))
       (loop lderiv)]
      [(Wrap p:begin0 (_ _ _ _ first lderiv))
       (join (loop first) (loop lderiv))]
      [(Wrap p:#%app (_ _ _ _ lderiv))
       (loop lderiv)]
      [(Wrap p:lambda (_ _ _ _ _ body))
       (loop body)]
      [(Wrap p:case-lambda (_ _ _ _ rbs))
       (apply join (map loop (or rbs null)))]
      [(Wrap p:let-values (_ _ _ _ _ rhss body))
       (join (loops rhss) (loop body))]
      [(Wrap p:letrec-values (_ _ _ _ _ rhss body))
       (join (loops rhss) (loop body))]
      [(Wrap p:letrec-syntaxes+values (_ _ _ _ _ srhss _ vrhss body _))
       (join (loops srhss) (loops vrhss) (loop body))]
      [(Wrap p:module (_ _ _ _ _ _ _ check _ _ body _))
       (join (loop check) (loop body))]
      [(Wrap p:#%module-begin (_ _ _ _ _ pass1 pass2 _))
       (join (loops pass1) (loops pass2))]
      [(Wrap lderiv (_ _ _ derivs))
       (loops derivs)]
      [(Wrap bderiv (_ _ pass1 _ pass2))
       (join (loops pass1) (loop pass2))]
      [(Wrap b:defvals (_ head _ _ _))
       (loop head)]
      [(Wrap b:defstx (_ deriv _ _ _ rhs))
       (join (loop deriv) (loop rhs))]
      [(Wrap b:splice (_ head _ _ _))
       (loop head)]
      [(Wrap b:expr (_ head))
       (loop head)]
      ;;[(Wrap b:begin (_ head inner))
      ;; (join (loop head) (loop inner))]
      [(Wrap mod:cons (head))
       (loop head)]
      [(Wrap mod:prim (head _ prim))
       (join (loop head) (loop prim))]
      [(Wrap mod:splice (head _ _ _))
       (loop head)]
      [(Wrap mod:lift (head _ tail))
       (join (loop head) (loop tail))]
      [(Wrap mod:lift-end (tail))
       (loop tail)]
      [(Wrap clc (_ _ body))
       (loop body)]
      [(Wrap bind-syntaxes (rhs _))
       (loop rhs)]

      [else zero]))

  (define (loops ds)
    (if (list? ds)
        (apply join (map loop ds))
        zero))
  (loop d))

(define (find-derivs/syntax pred d)
  (find-derivs (match-lambda
                [(Wrap deriv (e1 e2))
                 (pred e1)]
                [_ #f])
               (lambda _ #f)
               d))

;; extract-all-fresh-names : Derivation -> syntaxlike
(define (extract-all-fresh-names d)
  (define (renaming-node? x)
    (or (p:lambda? x)
        ;;(p:case-lambda? x)
        (clc? x)
        (p:let-values? x)
        (p:letrec-values? x)
        (p:letrec-syntaxes+values? x)
        (b:defvals? x)
        (b:defstx? x)
        (p:define-values? x)
        (p:define-syntaxes? x)))
  (define (extract-fresh-names d)
    (match d
      [(Wrap p:lambda (e1 e2 rs ?1 renames body))
       (if renames
           (with-syntax ([(?formals . ?body) renames])
             #'?formals)
           null)]
      [(Wrap clc (_ renames _))
       (if renames
           (with-syntax ([(?formals . ?body) renames])
             #'?formals)
           null)]
      [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
       (if renames
           (with-syntax ([(((?vars ?rhs) ...) . ?body) renames])
             #'(?vars ...))
           null)]
      [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
       (if renames
           (with-syntax ([(((?vars ?rhs) ...) . ?body) renames])
             #'(?vars ...))
           null)]
      [(Wrap p:letrec-syntaxes+values (e1 e2 rs ?1 srenames srhss vrenames vrhss body _))
       (cons
        (if srenames
            (with-syntax ([(((?svars ?srhs) ...) ((?vvars ?vrhs) ...) . ?body)
                           srenames])
              #'(?svars ... ?vvars ...))
            null)
        (if vrenames
            (with-syntax ([(((?vvars ?vrhs) ...) . ?body) vrenames])
              #'(?vvars ...))
            null))]
      [(Wrap b:defvals (rename head ?1 rename2 ?2))
       (let ([head-e2 (wderiv-e2 head)])
         (if head-e2
             (with-syntax ([(?dv ?vars ?rhs) head-e2])
               #'?vars)
             null))]
      [(Wrap b:defstx (rename head ?1 rename2 ?2 rhs))
       (let ([head-e2 (wderiv-e2 head)])
         (if head-e2
             (with-syntax ([(?ds ?svars ?rhs) head-e2])
               #'?svars)
             null))]
      [(Wrap p:define-values (e1 e2 rs ?1 rhs))
       (if rhs
           (with-syntax ([(?dv ?vars ?rhs) e1])
             #'?vars)
           null)]
      [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs _))
       (if rhs
           (with-syntax ([(?ds ?svars ?srhs) e1])
             #'?svars)
           null)]
      [_ null]))

  (let ([all-renaming-forms
         (find-deriv/unit+join+zero
          renaming-node?
          (lambda (d) #f)
          d
          list
          append
          null)])
    (flatten-identifiers (map extract-fresh-names all-renaming-forms))))

;; flatten-identifiers : syntaxlike -> (list-of identifier)
(define (flatten-identifiers stx)
  (syntax-case stx ()
    [id (identifier? #'id) (list #'id)]
    [() null]
    [(x . y) (append (flatten-identifiers #'x) (flatten-identifiers #'y))]
    [else (error 'flatten-identifers "neither syntax list nor identifier: ~s"
                 (if (syntax? stx)
                     (syntax->datum stx)
                     stx))]))
