
(module debug mzscheme
  (require (lib "plt-match.ss"))
  (require "trace.ss"
           "deriv-util.ss"
           "hide.ss"
           "hiding-policies.ss"
           "deriv.ss")

  (provide (all-from "trace.ss")
           (all-from "deriv.ss")
           (all-from "deriv-util.ss")
           (all-from "hiding-policies.ss")
           (all-from "hide.ss")
           (all-from (lib "plt-match.ss"))
           find-deriv)

  (define (find-deriv pred d)
    (define (loop d)
      (match d
        [(? pred d) (list d)]
        [(AnyQ mrule (_ _ tx next))
         (append (loop tx) (loop next))]
        [(AnyQ lift-deriv (_ _ first lift second))
         (append (loop first) (loop lift) (loop second))]
        [(AnyQ transformation (_ _ _ _ _ locals))
         (loops locals)]
        [(struct local-expansion (_ _ _ _ deriv))
         (loop deriv)]
        [(struct local-bind (deriv))
         (loop deriv)]
        [(AnyQ p:define-syntaxes (_ _ _ rhs))
         (loop rhs)]
        [(AnyQ p:define-values (_ _ _ rhs))
         (loop rhs)]
        [(AnyQ p:if (_ _ _ _ test then else))
         (append (loop test) (loop then) (loop else))]
        [(AnyQ p:wcm (_ _ _ key value body))
         (append (loop key) (loop value) (loop body))]
        [(AnyQ p:set! (_ _ _ _ rhs))
         (loop rhs)]
        [(AnyQ p:set!-macro (_ _ _ deriv))
         (loop deriv)]
        [(AnyQ p:begin (_ _ _ lderiv))
         (loop lderiv)]
        [(AnyQ p:begin0 (_ _ _ first lderiv))
         (append (loop first) (loop lderiv))]
        [(AnyQ p:#%app (_ _ _ _ lderiv))
         (loop lderiv)]
        [(AnyQ p:lambda (_ _ _ _ body))
         (loop body)]
        [(AnyQ p:case-lambda (_ _ _ rbs))
         (apply append (map loop (map cdr (or rbs null))))]
        [(AnyQ p:let-values (_ _ _ _ rhss body))
         (append (loops rhss) (loop body))]
        [(AnyQ p:letrec-values (_ _ _ _ rhss body))
         (append (loops rhss) (loop body))]
        [(AnyQ p:letrec-syntaxes+values (_ _ _ _ srhss _ vrhss body))
         (append (loops srhss) (loops vrhss) (loop body))]
        [(AnyQ p:module (_ _ _ _ body))
         (loop body)]
        [(AnyQ p:#%module-begin (_ _ _ pass1 pass2))
         (append (loops pass1) (loops pass2))]
        [(AnyQ p:rename (_ _ _ _ inner))
         (loop inner)]
        [(AnyQ p:synth (_ _ _ subterms))
         (loops (map s:subterm-deriv subterms))]

        [(AnyQ lderiv (_ _ derivs))
         (loops derivs)]
        [(AnyQ bderiv (_ _ pass1 _ pass2))
         (append (loops pass1) (loop pass2))]
        [(AnyQ b:defvals (_ head))
         (loop head)]
        [(AnyQ b:defstx (_ deriv rhs))
         (append (loop deriv) (loop rhs))]
        [(AnyQ b:splice (_ head _))
         (loop head)]
        [(AnyQ b:expr (_ head))
         (loop head)]
        [(AnyQ b:begin (_ head inner))
         (append (loop head) (loop inner))]
        [(AnyQ mod:cons (head))
         (loop head)]
        [(AnyQ mod:prim (head prim))
         (append (loop head) (loop prim))]
        [(AnyQ mod:splice (head _))
         (loop head)]
        [(AnyQ mod:lift (head tail))
         (append (loop head) (loop tail))]
        [(AnyQ mod:lift-end (tail))
         (loop tail)]
        [(AnyQ mod:begin (head inner))
         (append (loop head) (loop inner))]

        [else null]))

    (define (loops ds)
      (if (list? ds)
          (apply append (map loop ds))
          null))

    (loop d))
  )
