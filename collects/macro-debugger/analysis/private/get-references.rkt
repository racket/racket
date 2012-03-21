#lang racket/base
(require racket/match
         macro-debugger/model/deriv
         unstable/struct
         "util.rkt")
(provide deriv->refs)

;; ========

;; phase : (parameterof nat)
(define phase (make-parameter 0))
(define (add-disappeared-uses?) #t)

;; ========

;; deriv->refs : *Deriv* -> Refs
;; *Deriv* = Deriv | LDeriv | BRule | ModRule | ... (anything from deriv.rkt)
(define (deriv->refs deriv0)

  ;; refs : (listof Refs), mutable
  (define refs null)

  (define (recur . args)
    (let check ([arg args])
      (cond [(syntax? arg) (error 'deriv->refs "internal error on ~s" arg)]
            [(list? arg) (for-each check arg)]
            [else (void)]))
    (for ([arg (in-list args)])
      (if (list? arg)
          (apply recur arg)
          (analyze-deriv arg))))
  (define (recur/phase-up . args)
    (parameterize ((phase (add1 (phase))))
      (apply recur args)))
  (define (add-refs! rs)
    (set! refs (append rs refs)))
  (define (add! ids [mode 'reference])
    (let ([p (phase)])
      (add-refs! (for/list ([id (in-list ids)])
                   (ref p id mode (identifier-binding id p))))))
  (define (add/binding! id binding mode)
    (add-refs! (list (ref (phase) id mode binding))))

  ;; analyze/quote-syntax : stx -> void
  ;; Current approach: estimate that an identifier in a syntax template
  ;; may be used at (sub1 (phase)) or (phase).
  ;; FIXME: Allow for more conservative choices, too.
  ;; FIXME: #%top, #%app, #%datum, etc?
  ;; FIXME: Track tentative (in quote-syntax) references separately?
  (define (analyze/quote-syntax qs-stx)
    (let ([phases (for/list ([offset '(0 1 -1 2 -2)]) (+ (phase) offset))]
          [stx (syntax-case qs-stx ()
                 [(_quote-syntax x) #'x])])
      (define (add*! id)
        (add-refs! (for/list ([p (in-list phases)])
                     (ref p id 'quote-syntax (identifier-binding id p)))))
      (let loop ([stx stx])
        (let ([d (if (syntax? stx) (syntax-e stx) stx)])
          (cond [(identifier? stx) (add*! stx)]
                [(pair? d)
                 (loop (car d))
                 (loop (cdr d))]
                [(vector? d)
                 (map loop (vector->list d))]
                [(prefab-struct-key d)
                 (map loop (struct->list d))]
                [(box? d)
                 (loop (unbox d))]
                [else
                 (void)])))))

  (define (analyze-deriv deriv)
    ;; Handle common base (ie, resolves) part of derivs, if applicable
    (match deriv
      [(base z1 z2 resolves ?1)
       (add! resolves)
       (when (and (syntax? z2) (add-disappeared-uses?))
         (let ([uses (syntax-property z2 'disappeared-use)])
           (add! (let loop ([x uses] [onto null])
                   (cond [(identifier? x) (cons x onto)]
                         [(pair? x) (loop (car x) (loop (cdr x) onto))]
                         [else onto]))
                 'disappeared-use)))]
      [_
       (void)])
    ;; Handle individual variants
    (match deriv
      [(lift-deriv z1 z2 first lift-stx second)
       (recur first second)]
      [(tagrule z1 z2 tagged-stx next)
       (recur next)]
      [(lift/let-deriv z1 z2 first lift-stx second)
       (recur first second)]
      [(mrule z1 z2 rs ?1 me1 locals me2 ?2 etx next)
       (recur locals next)]
      [(local-exn exn)
       (void)]
      [(local-expansion z1 z2 for-stx? me1 inner lifted me2 opaque)
       ((if for-stx? recur/phase-up recur) inner)]
      [(local-lift expr ids)
       (void)]
      [(local-lift-end decl)
       (void)]
      [(local-lift-require req expr mexpr)
       (void)]
      [(local-lift-provide prov)
       (void)]
      [(local-bind names ?1 renames bindrhs)
       (recur bindrhs)]
      [(local-value name ?1 resolves bound? binding)
       #|
       Beware: in one common case, local-member-name, the binding of name is
       mutated (because used as binder in class body), so original binding is lost!
       Use binding instead.
       |#
       (when (and bound? (pair? binding))
         (add/binding! name binding 'syntax-local-value))]
      [(track-origin before after)
       (void)]
      [(local-remark contents)
       (void)]
      [(p:variable z1 z2 rs ?1)
       (void)]
      [(p:module z1 z2 rs ?1 locals tag rename check tag2 ?3 body shift)
       (recur locals check body)]
      [(p:#%module-begin z1 z2 rs ?1 me body ?2 subs)
       (recur body subs)]
      [(p:define-syntaxes z1 z2 rs ?1 prep rhs locals)
       (recur prep locals)
       (recur/phase-up rhs)]
      [(p:define-values z1 z2 rs ?1 rhs)
       (recur rhs)]
      [(p:begin-for-syntax z1 z2 rs ?1 prep body locals)
       (recur prep locals)
       (recur/phase-up body)]
      [(p:#%expression z1 z2 rs ?1 inner untag)
       (recur inner)]
      [(p:if z1 z2 rs ?1 test then else)
       (recur test then else)]
      [(p:wcm z1 z2 rs ?1 key mark body)
       (recur key mark body)]
      [(p:set! _ _ _ _ id-resolves ?2 rhs)
       (add! id-resolves)
       (recur rhs)]
      [(p:set!-macro _ _ _ _ deriv)
       (recur deriv)]
      [(p:#%app _ _ _ _ lderiv)
       (recur lderiv)]
      [(p:begin _ _ _ _ lderiv)
       (recur lderiv)]
      [(p:begin0 _ _ _ _ first lderiv)
       (recur first lderiv)]
      [(p:lambda _ _ _ _ renames body)
       (recur body)]
      [(p:case-lambda _ _ _ _ renames+bodies)
       (recur renames+bodies)]
      [(p:let-values _ _ _ _ renames rhss body)
       (recur rhss body)]
      [(p:letrec-values _ _ _ _ renames rhss body)
       (recur rhss body)]
      [(p:letrec-syntaxes+values _ _ _ _ srenames prep sbindrhss vrenames vrhss body tag)
       (recur prep sbindrhss vrhss body)]
      [(p:provide _ _ _ _ inners ?2)
       (recur inners)]
      [(p:require _ _ _ _ locals)
       (recur locals)]
      [(p:submodule _ _ _ _ exp)
       (recur exp)]
      [(p:submodule* _ _ _ _)
       (void)]
      [(p:#%stratified-body _ _ _ _ bderiv)
       (recur bderiv)]
      [(p:stop _ _ _ _) (void)]
      [(p:unknown _ _ _ _) (void)]
      [(p:#%top _ _ _ _)
       (void)]
      [(p:#%datum _ _ _ _) (void)]
      [(p:quote _ _ _ _) (void)]
      [(p:quote-syntax z1 z2 _ _)
       (when z2 (analyze/quote-syntax z2))]
      [(p:#%variable-reference _ _ _ _)
       (void)]
      [(lderiv _ _ ?1 derivs)
       (recur derivs)]
      [(bderiv _ _ pass1 trans pass2)
       (recur pass1 pass2)]
      [(b:error ?1)
       (void)]
      [(b:expr _ head)
       (recur head)]
      [(b:splice _ head ?1 tail ?2)
       (recur head)]
      [(b:defvals _ head ?1 rename ?2)
       (recur head)]
      [(b:defstx _ head ?1 rename ?2 prep bindrhs)
       (recur head prep bindrhs)]
      [(bind-syntaxes rhs locals)
       (recur/phase-up rhs)
       (recur locals)]
      [(clc ?1 renames body)
       (recur body)]
      [(module-begin/phase pass1 pass2 pass3)
       (recur pass1 pass2 pass3)]
      [(mod:prim head rename prim)
       (recur head prim)]
      [(mod:splice head rename ?1 tail)
       (recur head)]
      [(mod:lift head locals renames tail)
       (recur head locals)]
      [(mod:lift-end tail)
       (void)]
      [(mod:cons head locals)
       (recur head locals)]
      [(mod:skip)
       (void)]
      ;; Shouldn't occur in module expansion.
      ;; (Unless code calls 'expand' at compile-time; weird, but possible.)
      [(ecte _ _ locals first second locals2)
       (recur locals first second locals2)]
      [(bfs:lift lderiv lifts)
       (recur lderiv)]
      [#f
       (void)]))

  (analyze-deriv deriv0)
  refs)
