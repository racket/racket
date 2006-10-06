
(module deriv mzscheme
  (require (lib "contract.ss")
           (lib "stx.ss" "syntax")
           "deriv-c.ss")

  ;; NO CONTRACTS
  
  (provide (all-from "deriv-c.ss"))
  ;; CONTRACTS

#;  (begin
  (define (stx-list-like? x)
    (or (syntax? x)
        (null? x)
        (and (pair? x) (syntax? (car x)) (stx-list-like? (cdr x)))))

  (define (maybe c) (or/c c false/c))
  
  (define node/c (or/c deriv? lderiv? bderiv? transformation? brule? modrule?))
  (define errnode/c (or/c prule? transformation? lderiv? brule? modrule?))
  (define tag/c (maybe symbol?))
  (define syntax/f (maybe syntax?))
  (define syntaxes/c stx-list-like?)
  (define syntaxes/f (maybe syntaxes/c))

  (define (anyw C)
    (or/c (struct/c error-wrap exn? tag/c C)
          (struct/c interrupted-wrap tag/c C)))
  (define (anyq C)
    (or/c C (anyw C)))
  (define (intw C)
    (struct/c interrupted-wrap tag/c C))
  (define (intq C)
    (or/c C (intw C)))

  (define resolves/c (listof identifier?))

  (provide/contract
   (struct deriv
           ([e1 syntax?]
            [e2 syntax/f]))
   (struct (mrule deriv)
           ([e1 syntax?]
            [e2 syntax/f]
            [transformation (anyq transformation?)]
            [next (maybe (anyq deriv?))]))
   (struct (lift-deriv deriv)
           ([e1 syntax?]
            [e2 syntax/f]
            [first deriv?]
            [lift-stx syntax?]
            [second (anyq deriv?)]))
   (struct (lift/let-deriv deriv)
           ([e1 syntax?]
            [e2 syntax/f]
            [first deriv?]
            [lift-stx syntax?]
            [second (anyq deriv?)]))
   (struct transformation
           ([e1 syntax?]
            [e2 syntax/f]
            [resolves resolves/c]
            [me1 syntax?]
            [me2 syntax/f]
            [locals (listof (or/c local-expansion? local-lift? local-lift-end? local-bind?))]))
   (struct (prule deriv)
           ([e1 syntax?]
            [e2 syntax/f]
            [resolves resolves/c]))
   (struct (p:#%app prule)
           ([e1 syntax?]
            [e2 syntax/f]
            [resolves resolves/c]
            [tagged-stx syntax?]
            [lderiv (anyq (maybe lderiv?))]))

   (struct lderiv
           ([es1 syntaxes/c]
            [es2 syntaxes/f]
            [derivs (listof (anyq deriv?))]))

   (struct interrupted-wrap
           ([tag (or/c symbol? false/c)]
            [inner node/c]))
   (struct error-wrap
           ([exn exn?]
            [tag (or/c symbol? false/c)]
            [inner errnode/c])))


  (provide ;(struct deriv (e1 e2))
           ;(struct mrule (transformation next))
           ;(struct lift-deriv (first lift-stx second))
           ;(struct lift/let-deriv (first lift-stx second))
           
           ;(struct transformation (e1 e2 resolves me1 me2 locals))

           (struct local-expansion (e1 e2 me1 me2 deriv))
           (struct local-lift (expr id))
           (struct local-lift-end (decl))
           (struct local-bind (deriv))

           ;(struct prule (resolves))
           (struct p:variable ())
           (struct p:define-syntaxes (rhs))
           (struct p:define-values (rhs))
           (struct p:if (full? test then else))
           (struct p:wcm (key mark body))
           (struct p:set! (id-resolves rhs))
           (struct p:set!-macro (deriv))
           (struct p:begin (lderiv))
           (struct p:begin0 (first lderiv))
           ;(struct p:#%app (tagged-stx lderiv))
           (struct p:lambda (renames body))
           (struct p:case-lambda (renames+bodies))
           (struct p:let-values (renames body))
           (struct p:letrec-values (renames rhss body))
           (struct p:letrec-syntaxes+values (srenames srhss vrenames vrhss body))
           (struct p:module (one-body-form? body))
           (struct p:#%module-begin (pass1 pass2))
           (struct p::STOP ())
           (struct p:#%datum (tagged-stx))
           (struct p:#%top (tagged-stx))
           (struct p:quote ())
           (struct p:quote-syntax ())
           (struct p:require ())
           (struct p:require-for-syntax ())
           (struct p:require-for-template ())
           (struct p:provide ())
           (struct p:stop ())
           (struct p:unknown ())
           (struct p:rename (renames inner))

           (struct p:synth (subterms))
           (struct s:subterm (path deriv))
           (struct s:rename (path before after))

           ;(struct lderiv (es1 es2 derivs))
           (struct bderiv (es1 es2 pass1 trans pass2))
           
           (struct brule (renames))
           (struct b:defvals (head))
           (struct b:defstx (deriv rhs))
           (struct b:splice (head tail))
           (struct b:expr (head))
           (struct b:begin (head inner))

           (struct modrule ())
           (struct mod:cons (head))
           (struct mod:prim (head prim))
           (struct mod:skip ())
           (struct mod:splice (head tail))
           (struct mod:lift (head tail))
           (struct mod:lift-end (tail))
           (struct mod:begin (head inner))

           ;(struct interrupted-wrap (tag inner))
           ;(struct error-wrap (exn tag inner))
           )
           

  ;; Well-formedness
  
  ;; Predicates on well-formed derivations
  #;
  (define (wf-ok-deriv? x)
    (match x
      [($ pderiv e1 e2 prule)
       (and (syntax? e1)
            (syntax? e2)
            (wf-ok-prule? prule))]
      [($ mderiv e1 e2 mrule next)
       (and (syntax? e1)
            (syntax? e2)
            (wf-ok-mrule? mrule)
            (wf-ok-deriv? next))]
      [else #f]))

  #;
  (define (wf-ok-mrule? x)
    (match x
      [($ mrule e1 e2 rs me1 me2 locals)
       (and (syntax? e1)
            (syntax? e2)
            (list? rs)
            (andmap identifier? rs)
            (syntax? me1)
            (syntax? me2)
            (list? locals)
            (andmap wf-ok-deriv? locals))]
      [else #f]))

  #;
  (define (wf-ok-basic-prule? x)
    (match x
      [($ prule e1 e2 rs)
       (and (syntax? e1)
            (syntax? e2)
            (list? rs)
            (andmap identifier? rs))]
      [else #f]))

  #;
  (define (wf-ok-prule? x)
    (and (wf-ok-basic-prule? x)
         (match x
           [($ p:variable _ _ _) #t]
           [($ p:define-syntaxes _ _ _ rhs)
            (wf-ok-deriv? rhs)]
           [($ p:define-values _ _ _ rhs)
            (wf-ok-deriv? rhs)]
           [($ p:if _ _ _ test then else)
            (and (wf-ok-deriv? test)
                 (wf-ok-deriv? then)
                 (wf-ok-deriv? else))]
           [($ p:wcm _ _ _ key value body)
            (and (wf-ok-deriv? key)
                 (wf-ok-deriv? value)
                 (wf-ok-deriv? body))]
           [($ p:set! _ _ _ id-rs rhs)
            (and (list? id-rs)
                 (andmap identifier? id-rs)
                 (wf-ok-deriv? rhs))]
           [($ p:set!-macro _ _ _ deriv)
            (wf-ok-deriv? deriv)]
           [($ p:begin _ _ _ lderiv)
            (wf-ok-lderiv? lderiv)]
           [($ p:begin0 _ _ _ first lderiv)
            (and (wf-ok-deriv? first)
                 (wf-ok-lderiv? lderiv))]
           [($ p:#%app _ _ _ lderiv)
            (wf-ok-lderiv? lderiv)]
           [($ p:lambda _ _ _ renames body)
            (and (pair? renames)
                 (syntax? (car renames))
                 (syntax? (cdr renames))
                 (wf-ok-bderiv? body))]
           [($ p:case-lambda _ _ _ (renames+bodies ...))
            (andmap (lambda (r+b)
                      (and (pair? r+b)
                           (pair? (car r+b))
                           (syntax? (caar r+b))
                           (syntax? (cdar r+b))
                           (wf-ok-bderiv? (cdr r+b))))
                    renames+bodies)]
           [($ p:let-values _ _ _ (renames ...) (rhss ...) body)
            (and (andmap (lambda (r)
                           (and (pair? r)
                                (syntax? (car r))
                                (syntax? (cdr r))))
                         renames)
                 (andmap wf-ok-deriv? rhss)
                 (= (length renames) (length rhss))
                 (wf-ok-bderiv? body))]
           [($ p:letrec-values _ _ _ (renames ...) (rhss ...) body)
            (and (andmap (lambda (r)
                           (and (pair? r)
                                (syntax? (car r))
                                (syntax? (cdr r))))
                         renames)
                 (andmap wf-ok-deriv? rhss)
                 (= (length renames) (length rhss))
                 (wf-ok-bderiv? body))]
           [($ p:letrec-syntaxes+values _ _ _ 
               (srenames ...) (srhss ...) (vrenames ...) (vrhss ...) body)
            (and (andmap (lambda (r)
                           (and (pair? r) (syntax? (car r)) (syntax? (cdr r))))
                         srenames)
                 (andmap wf-ok-deriv? srhss)
                 (= (length srenames) (length srhss))
                 (andmap (lambda (r)
                           (and (pair? r) (syntax? (car r)) (syntax? (cdr r))))
                         vrenames)
                 (andmap wf-ok-deriv? vrhss)
                 (= (length vrenames) (length vrhss))
                 (wf-ok-bderiv? body))]
           [($ p::STOP _ _ _) #t]
           [else #f])))
  
  #;
  (define (wf-ok-lderiv? x)
    (match x
      [($ lderiv es1 es2 derivs)
       (and (list? es1)
            (andmap syntax? es1)
            (list? es2)
            (andmap syntax? es2)
            (list? derivs)
            (andmap wf-ok-lderiv? derivs))]
      [else #f]))

  #;
  (define (wf-ok-bderiv? x)
    (define (wf-ok-brule? x)
      (match x
        [($ brskip renames next)
         (and (void renames)
              (wf-ok-brule? next))]
        [($ brcons renames head next)
         (and (void renames)
              (wf-ok-deriv? head)
              (wf-ok-brule? next))]
        [($ brdefstx renames deriv rhs next)
         (and (void renames)
              (wf-ok-deriv? deriv)
              (wf-ok-deriv? rhs)
              (wf-ok-brule? next))]
        [($ brsplice tail next)
         (and (list? tail)
              (andmap syntax? tail)
              (wf-ok-brule? next))]
        [else #f]))
    (match x
      [($ bderiv es1 es2 pass1 trans pass2)
       (and (wf-ok-brule? pass1)
            (wf-ok-lderiv? pass2))]
      [else #f]))
  
  #;
  (define (wf-exn-deriv? x)
    #f)
  )
  )
