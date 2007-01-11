
(module deriv-util mzscheme
  (require "deriv.ss"
           (lib "plt-match.ss"))
  (provide IntW
           ErrW
           AnyQ
           IntQ
           
           Wrap
           lift/wrap
           rewrap
           rewrap/nt
           outer-rewrap
           lift/deriv-e1
           lift/deriv-e2
           wrapped?

           find-derivs
           find-deriv
           find-derivs/syntax)

  ;; IntW
  ;; Matches only interrupted wraps
  (define-match-expander IntW
    (syntax-rules ()
      [(IntW S (var ...))
       (struct interrupted-wrap (_ (struct S (var ...))))]
      [(IntW S (var ...) tag)
       (struct interrupted-wrap (tag (struct S (var ...))))]))

  ;; ErrW
  ;; Matches only error wraps
  (define-match-expander ErrW
    (syntax-rules ()
      [(ErrW S (var ...))
       (struct error-wrap (_ _ (struct S (var ...))))]
      [(ErrW S (var ...) exn)
       (struct error-wrap (exn _ (struct S (var ...))))]
      [(ErrW S (var ...) tag exn)
       (struct error-wrap (exn tag (struct S (var ...))))]))
  
  ;; AnyQ matcher
  ;; Matches unwrapped, interrupted wrapped, or error wrapped
  (define-match-expander AnyQ
    (syntax-rules ()
      [(AnyQ S (var ...))
       (app unwrap (struct S (var ...)))]
      [(AnyQ S (var ...) exni)
       (and (app unwrap (struct S (var ...)))
            (app extract-exni exni))]))
  
  ;; IntQ
  ;; Matches interrupted wraps and unwrapped structs
  (define-match-expander IntQ
    (syntax-rules ()
      [(IntQ S (var ...))
       (? not-error-wrap? (app unwrap (struct S (var ...))))]
      [(IntQ S (var ...) tag)
       (? not-error-wrap? 
          (app unwrap (struct S (var ...)))
          (app extract-tag tag))]))

  (define-match-expander Wrap
    (syntax-rules ()
      [(Wrap x)
       (app unwrap x)]))
  
  (define (unwrap x)
    (match x
      [(struct interrupted-wrap (tag inner))
       inner]
      [(struct error-wrap (exn tag inner))
       inner]
      [else x]))

  (define (extract-exni x)
    (match x
      [(struct interrupted-wrap (tag inner))
       (cons #f tag)]
      [(struct error-wrap (exn tag inner))
       (cons exn tag)]
      [else #f]))

  (define (extract-tag x)
    (match x
      [(struct interrupted-wrap (tag inner))
       tag]
      [(struct error-wrap (exn tag inner))
       tag]
      [else #f]))

  (define (not-error-wrap? x)
    (not (error-wrap? x)))

  ;; lift/wrap : ('a -> 'b) boolean -> Wrap('a) -> Wrap('b)
  (define (lift/wrap f preserve-tag?)
    (lambda (x)
      (match x
        [(struct interrupted-wrap (tag inner))
         (make-interrupted-wrap (and preserve-tag? tag) (f inner))]
        [(struct error-wrap (exn tag inner))
         (make-error-wrap exn (and preserve-tag? tag) (f inner))]
        [x
         (f x)])))

  ;; rewrap : Wrap('a) 'b -> Wrap('b)
  (define (rewrap x y)
    (if (wrapped? y)
        y
        ((lift/wrap (lambda (x) y) #t) x)))
  
  ;; rewrap/nt : Wrap('a) 'b -> Wrap('b)
  (define (rewrap/nt x y)
    (if (wrapped? y)
        y
        ((lift/wrap (lambda (x) y) #f) x)))

  (define (outer-rewrap x y)
    (if (and (wrapped? x) (not (wrapped? y)))
        (make-interrupted-wrap #f y)
        y))
  
  (define (lift/deriv-e1 x)
    (match x
      [(AnyQ deriv (e1 _)) e1]))

  (define (lift/deriv-e2 x)
    (match x
      [(AnyQ deriv (_ e2)) e2]))
  
  (define (wrapped? x)
    (or (interrupted-wrap? x)
        (error-wrap? x)))

  ;; Utilities for finding subderivations
  
  ;; find-derivs : (deriv -> boolean) (deriv -> boolean) deriv -> (list-of deriv)
  (define (find-derivs pred stop-short d)
    (find-deriv/unit+join+zero pred stop-short d list append null))

  ;; find-deriv : (deriv -> boolean) (deriv -> boolean) deriv -> deriv/#f
  ;; Finds the first deriv that matches; throws the rest away
  (define (find-deriv pred stop-short d)
    (let/ec return (find-deriv/unit+join+zero pred stop-short d return (lambda _ #f) #f)))

  ;; find-deriv/unit+join+zero
  ;; Parameterized over monad operations for combining the results
  ;; For example, <list, append, null> collects the results into a list
  (define (find-deriv/unit+join+zero pred stop-short d unit join zero)
    (define (loop d)
      (match d
        [(? pred d) (unit d)]
        [(? stop-short d) zero]
        [(AnyQ mrule (_ _ tx next))
         (join (loop tx) (loop next))]
        [(AnyQ lift-deriv (_ _ first lift second))
         (join (loop first) (loop lift) (loop second))]
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
         (join (loop test) (loop then) (loop else))]
        [(AnyQ p:wcm (_ _ _ key value body))
         (join (loop key) (loop value) (loop body))]
        [(AnyQ p:set! (_ _ _ _ rhs))
         (loop rhs)]
        [(AnyQ p:set!-macro (_ _ _ deriv))
         (loop deriv)]
        [(AnyQ p:begin (_ _ _ lderiv))
         (loop lderiv)]
        [(AnyQ p:begin0 (_ _ _ first lderiv))
         (join (loop first) (loop lderiv))]
        [(AnyQ p:#%app (_ _ _ _ lderiv))
         (loop lderiv)]
        [(AnyQ p:lambda (_ _ _ _ body))
         (loop body)]
        [(AnyQ p:case-lambda (_ _ _ rbs))
         (apply join (map loop (map cdr (or rbs null))))]
        [(AnyQ p:let-values (_ _ _ _ rhss body))
         (join (loops rhss) (loop body))]
        [(AnyQ p:letrec-values (_ _ _ _ rhss body))
         (join (loops rhss) (loop body))]
        [(AnyQ p:letrec-syntaxes+values (_ _ _ _ srhss _ vrhss body))
         (join (loops srhss) (loops vrhss) (loop body))]
        [(AnyQ p:module (_ _ _ _ body))
         (loop body)]
        [(AnyQ p:#%module-begin (_ _ _ pass1 pass2))
         (join (loops pass1) (loops pass2))]
        [(AnyQ p:rename (_ _ _ _ inner))
         (loop inner)]
        [(AnyQ p:synth (_ _ _ subterms))
         (loops (map s:subterm-deriv subterms))]

        [(AnyQ lderiv (_ _ derivs))
         (loops derivs)]
        [(AnyQ bderiv (_ _ pass1 _ pass2))
         (join (loops pass1) (loop pass2))]
        [(AnyQ b:defvals (_ head))
         (loop head)]
        [(AnyQ b:defstx (_ deriv rhs))
         (join (loop deriv) (loop rhs))]
        [(AnyQ b:splice (_ head _))
         (loop head)]
        [(AnyQ b:expr (_ head))
         (loop head)]
        [(AnyQ b:begin (_ head inner))
         (join (loop head) (loop inner))]
        [(AnyQ mod:cons (head))
         (loop head)]
        [(AnyQ mod:prim (head prim))
         (join (loop head) (loop prim))]
        [(AnyQ mod:splice (head _))
         (loop head)]
        [(AnyQ mod:lift (head tail))
         (join (loop head) (loop tail))]
        [(AnyQ mod:lift-end (tail))
         (loop tail)]
        [(AnyQ mod:begin (head inner))
         (join (loop head) (loop inner))]

        [else zero]))

    (define (loops ds)
      (if (list? ds)
          (apply join (map loop ds))
          zero))
    (loop d))

  (define (find-derivs/syntax pred d)
    (find-derivs (match-lambda
                  [(AnyQ deriv (e1 e2))
                   (pred e1)]
                  [_ #f])
                 (match-lambda
                  [(AnyQ p:module (_ _ _ _ _)) #t]
                  [(AnyQ lift-deriv (_ _ _ _ _)) #t]
                  [_ #f])
                 d))
  )
