#lang racket/base
(require (for-syntax racket/base
                     "dset.rkt"
                     racket/syntax
                     syntax/parse/private/minimatch
                     racket/private/stx ;; syntax/stx
                     racket/private/sc)
         syntax/parse/private/residual
         racket/private/stx
         racket/performance-hint
         racket/private/promise)
(provide template
         template/loc
         datum-template
         quasitemplate
         quasitemplate/loc
         define-template-metafunction
         ??
         ?@)

;; ============================================================
;; Syntax of templates

;; A Template (T) is one of:
;;   - pattern-variable
;;   - constant (including () and non-pvar identifiers)
;;   - (metafunction . T)
;;   - (H . T)
;;   - (H ... . T), (H ... ... . T), etc
;;   - (?? T T)
;;   - #(T*)
;;   - #s(prefab-struct-key T*)
;;   * (unsyntax expr)

;; A HeadTemplate (H) is one of:
;;   - T
;;   - (?? H)
;;   - (?? H H)
;;   - (?@ . T)
;;   * (unquote-splicing expr)

(define-syntaxes (?? ?@)
  (let ([tx (lambda (stx) (raise-syntax-error #f "not allowed as an expression" stx))])
    (values tx tx)))

(define-syntax ?@! #f) ;; private, escape-ignoring version of ?@, used by unsyntax-splicing

;; ============================================================
;; Compile-time

;; Parse template syntax into a Guide (AST--the name is left over from
;; when the "guide" was a data structure interpreted at run time).

;; The AST representation is designed to coincide with the run-time
;; support, so compilation is just (datum->syntax #'here guide).

;; A Guide (G) is one of:
;; - (list 't-resyntax G)     ;; template is syntax; re-syntax result
;; - (list 't-const)          ;; constant
;; - (list 't-var PVar Boolean) ;; pattern variable
;; - (list 't-cons/p G G)     ;; template is non-syntax pair => no restx, use {car,cdr}
;; - (list 't-vector G)       ;; template is non-syntax vector
;; - (list 't-struct G)       ;; template is non-syntax prefab struct
;; - (list 't-box G)          ;; template is non-syntax box
;; - (list 't-dots HG (listof (listof PVar)) Nat G/#f #f Boolean)
;; - (list 't-dots  G (listof (listof PVar)) Nat G/#f #t Boolean)
;; - (list 't-append/p HG G)  ;; template is non-syntax pair => no restx, use {car,cdr}
;; - (list 't-escaped G)
;; - (list 't-orelse G G)
;; - (list 't-metafun Id G)
;; - (list 't-relocate G Id)        ;; relocate syntax
;; - (list 't-resyntax/loc G Id)    ;; like t-resyntax, but use alt srcloc
;; For 't-var and 't-dots, the final boolean indicates whether the template
;; fragment is in the left-hand side of an orelse (??).

;; A HeadGuide (HG) is one of:
;; - (list 'h-t G)
;; - (list 'h-orelse HG HG/#f)
;; - (list 'h-splice G)

;; A PVar is (pvar Id Id Boolean Nat/#f)
;;
;; The first identifier (var) is from the syntax-mapping or attribute-binding.
;; The second (lvar) is a local variable name used to hold its value (or parts
;; thereof) in ellipsis iteration. The boolean is #f if var is trusted to have a
;; (Listof^depth Syntax) value, #t if it needs to be checked.
;;
;; The depth-delta associated with a depth>0 pattern variable is the difference
;; between the pattern variable's depth and the depth at which it is used. (For
;; depth 0 pvars, it's #f.) For example, in
;;
;;   (with-syntax ([x #'0]
;;                 [(y ...) #'(1 2)]
;;                 [((z ...) ...) #'((a b) (c d))])
;;     (template (((x y) ...) ...)))
;;
;; the depth-delta for x is #f, the depth-delta for y is 1, and the depth-delta for
;; z is 0. Coincidentally, the depth-delta is the same as the depth of the ellipsis
;; form at which the variable should be moved to the loop-env. That is, the
;; template above should be interpreted as roughly similar to
;;
;;   (let ([x (pvar-value-of x)]
;;         [y (pvar-value-of y)]
;;         [z (pvar-value-of z)])
;;     (for ([Lz (in-list z)]) ;; depth 0
;;       (for ([Ly (in-list y)] ;; depth 1
;;             [Lz (in-list Lz)])
;;         (___ x Ly Lz ___))))

(begin-for-syntax

 (define-logger template)

 (struct pvar (var lvar check? dd) #:prefab)
 (struct template-metafunction (var))

 (define (ht-guide? x)  (match x [(list 'h-t _) #t] [_ #f]))
 (define (ht-guide-t x) (match x [(list 'h-t g) g]))

 (define const-guide '(t-const))
 (define (const-guide? x) (equal? x const-guide))

 ;; ----------------------------------------
 ;; Parsing templates

 ;; parse-template : Syntax Boolean -> (values (listof PVar) Guide)
 (define (parse-template t stx?)
   ;; env : Hasheq[ (cons syntax-mapping Nat) => PVar ]
   (define env (make-hasheq))

   ;; parse-t : Stx Nat Boolean Boolean -> (values (dsetof PVar) Guide)
   (define (parse-t t depth esc? in-try?)
     (cond [(stx-pair? t)
            (if (identifier? (stx-car t))
                (parse-t-pair/command t depth esc? in-try?)
                (parse-t-pair/dots t depth esc? in-try?))]
           [else (parse-t-nonpair t depth esc? in-try?)]))

   ;; parse-t-pair/command : Stx Nat Boolean Boolean -> ...
   ;; t is a stxpair w/ id in car; check if it is a "command" (metafun, escape, etc)
   (define (parse-t-pair/command t depth esc? in-try?)
     (syntax-case t (??)
       [(DOTS template)
        (and (not esc?) (free-identifier=? #'DOTS (quote-syntax ...)))
        (let-values ([(drivers guide) (parse-t #'template depth #t in-try?)])
          (values drivers `(t-escaped ,guide)))]
       [(?? t1 t2)
        (not esc?)
        (let-values ([(drivers1 guide1) (parse-t #'t1 depth esc? #t)]
                     [(drivers2 guide2) (parse-t #'t2 depth esc? in-try?)])
          (values (dset-union drivers1 drivers2) `(t-orelse ,guide1 ,guide2)))]
       [(mf-id . _)
        (and (not esc?) (lookup-metafun #'mf-id))
        (let-values ([(mf) (lookup-metafun #'mf-id)]
                     [(drivers guide) (parse-t (stx-cdr t) depth esc? in-try?)])
          (unless stx? (wrong-syntax "metafunctions not supported" #'mf-id))
          (values drivers `(t-metafun ,(template-metafunction-var mf) ,guide)))]
       [_ (parse-t-pair/dots t depth esc? in-try?)]))

   ;; parse-t-pair/dots : Stx Nat Boolean Boolean -> ...
   ;; t is a stx pair; check for dots
   (define (parse-t-pair/dots t depth esc? in-try?)
     (define head (stx-car t))
     (define-values (tail nesting)
       (let loop ([tail (stx-cdr t)] [nesting 0])
         (if (and (not esc?) (stx-pair? tail) (stx-dots? (stx-car tail)))
             (loop (stx-cdr tail) (add1 nesting))
             (values tail nesting))))
     (if (zero? nesting)
         (parse-t-pair/normal t depth esc? in-try?)
         (let-values ([(hdrivers hguide) (parse-h head (+ depth nesting) esc? in-try?)]
                      [(tdrivers tguide)
                       (if (null? tail)
                           (values (dset) #f)
                           (parse-t tail depth esc? in-try?))])
           (when (dset-empty? hdrivers)
             (wrong-syntax head "no pattern variables before ellipsis in template"))
           (when (dset-empty? (dset-filter hdrivers (pvar/dd<=? depth)))
             (let ([bad-dots ;; select the nestingth (last) ellipsis as the bad one
                    (stx-car (stx-drop nesting t))])
               ;; FIXME: improve error message?
               (wrong-syntax bad-dots "too many ellipses in template")))
           ;; hdrivers is (listof (dsetof pvar)); compute pvars new to each level
           (define hdriverss ;; per level
             (for/list ([i (in-range nesting)])
               (dset-filter hdrivers (pvar/dd<=? (+ depth i)))))
           (define new-hdriverss ;; per level
             (let loop ([raw hdriverss] [last (dset)])
               (cond [(null? raw) null]
                     [else
                      (define new-hdrivers (dset->list (dset-subtract (car raw) last)))
                      (cons new-hdrivers (loop (cdr raw) (car raw)))])))
           (values (dset-union hdrivers tdrivers)
                   (let ([cons? (ht-guide? hguide)]
                         [hguide (if (ht-guide? hguide) (ht-guide-t hguide) hguide)])
                     (resyntax t `(t-dots ,hguide ,new-hdriverss ,nesting ,tguide ,cons? ,in-try?)))))))

   ;; parse-t-pair/normal : Stx Nat Boolean Boolean -> ...
   ;; t is a normal stx pair
   (define (parse-t-pair/normal t depth esc? in-try?)
     (define-values (hdrivers hguide) (parse-h (stx-car t) depth esc? in-try?))
     (define-values (tdrivers tguide) (parse-t (stx-cdr t) depth esc? in-try?))
     (values (dset-union hdrivers tdrivers)
             (let ([kind (if (ht-guide? hguide) 't-cons/p 't-append/p)]
                   [hguide (if (ht-guide? hguide) (ht-guide-t hguide) hguide)])
               (resyntax t `(,kind ,hguide ,tguide)))))

   ;; parse-t-nonpair : Stx Nat Boolean Boolean -> ...
   ;; PRE: t is not a stxpair
   (define (parse-t-nonpair t depth esc? in-try?)
     (syntax-case t (?? ?@)
       [id
        (identifier? #'id)
        (cond [(and (not esc?)
                    (or (free-identifier=? #'id (quote-syntax ...))
                        (free-identifier=? #'id (quote-syntax ??))
                        (free-identifier=? #'id (quote-syntax ?@))))
               (wrong-syntax #'id "illegal use")]
              [(lookup-metafun #'id)
               (wrong-syntax t "illegal use of syntax metafunction")]
              [(lookup #'id depth)
               => (lambda (pvar) (values (dset pvar) `(t-var ,pvar ,in-try?)))]
              [else (values (dset) const-guide)])]
       [vec
        (vector? (syntax-e #'vec))
        (let-values ([(drivers guide)
                      (parse-t (vector->list (syntax-e #'vec)) depth esc? in-try?)])
          (values drivers (if (const-guide? guide) const-guide (resyntax t `(t-vector ,guide)))))]
       [pstruct
        (prefab-struct-key (syntax-e #'pstruct))
        (let-values ([(drivers guide)
                      (let ([elems (cdr (vector->list (struct->vector (syntax-e #'pstruct))))])
                        (parse-t elems depth esc? in-try?))])
          (values drivers (if (const-guide? guide) const-guide (resyntax t `(t-struct ,guide)))))]
       [#&template
        (let-values ([(drivers guide)
                      (parse-t #'template depth esc? in-try?)])
          (values drivers (if (const-guide? guide) const-guide (resyntax t `(t-box ,guide)))))]
       [const
        (values (dset) const-guide)]))

   ;; parse-h : Syntax Nat Boolean Boolean -> (values (dsetof PVar) HeadGuide)
   (define (parse-h h depth esc? in-try?)
     (syntax-case h (?? ?@ ?@!)
       [(?? t)
        (not esc?)
        (let-values ([(drivers guide) (parse-h #'t depth esc? #t)])
          (values drivers `(h-orelse ,guide #f)))]
       [(?? t1 t2)
        (not esc?)
        (let-values ([(drivers1 guide1) (parse-h #'t1 depth esc? #t)]
                     [(drivers2 guide2) (parse-h #'t2 depth esc? in-try?)])
          (values (dset-union drivers1 drivers2)
                  (if (and (ht-guide? guide1) (ht-guide? guide2))
                      `(h-t (t-orelse ,(ht-guide-t guide1) ,(ht-guide-t guide2)))
                      `(h-orelse ,guide1 ,guide2))))]
       [(?@ . _)
        (not esc?)
        (let-values ([(drivers guide) (parse-t (stx-cdr h) depth esc? in-try?)])
          (values drivers `(h-splice ,guide)))]
       [(?@! . _)
        (let-values ([(drivers guide) (parse-t (stx-cdr h) depth esc? in-try?)])
          (values drivers `(h-splice ,guide)))]
       [t
        (let-values ([(drivers guide) (parse-t #'t depth esc? in-try?)])
          (values drivers `(h-t ,guide)))]))

   ;; lookup : Identifier Nat -> PVar/#f
   (define (lookup id depth)
     (define variable? (if stx? syntax-pattern-variable? s-exp-pattern-variable?))
     (let ([v (syntax-local-value/record id variable?)])
       (cond [(syntax-pattern-variable? v)
              (hash-ref! env (cons v depth)
                (lambda ()
                  (define pvar-depth (syntax-mapping-depth v))
                  (define attr
                    (let ([attr (syntax-local-value (syntax-mapping-valvar v) (lambda () #f))])
                      (and (attribute-mapping? attr) attr)))
                  (define var (if attr (attribute-mapping-var attr) (syntax-mapping-valvar v)))
                  (define check? (and attr (not (attribute-mapping-syntax? attr))))
                  (cond [(zero? pvar-depth)
                         (pvar var var check? #f)]
                        [(>= depth pvar-depth)
                         (define lvar (car (generate-temporaries #'(pv_))))
                         (pvar var lvar check? (- depth pvar-depth))]
                        [else
                         (wrong-syntax id "missing ellipses with pattern variable in template")])))]
             [(s-exp-pattern-variable? v)
              (hash-ref! env (cons v depth)
                (lambda ()
                  (define pvar-depth (s-exp-mapping-depth v))
                  (define var (s-exp-mapping-valvar v))
                  (define check? #f)
                  (cond [(zero? pvar-depth)
                         (pvar var var #f #f)]
                        [(>= depth pvar-depth)
                         (define lvar (car (generate-temporaries #'(pv_))))
                         (pvar var lvar #f (- depth pvar-depth))]
                        [else
                         (wrong-syntax id "missing ellipses with pattern variable in template")])))]
             [else
              ;; id is a literal; check that for all x s.t. id = x.y, x is not an attribute
              (for ([pfx (in-list (dotted-prefixes id))])
                (let ([pfx-v (syntax-local-value pfx (lambda () #f))])
                  (when (and (syntax-pattern-variable? pfx-v)
                             (let ([valvar (syntax-mapping-valvar pfx-v)])
                               (attribute-mapping? (syntax-local-value valvar (lambda () #f)))))
                    (wrong-syntax id "undefined nested attribute of attribute `~a'" (syntax-e pfx)))))
              #f])))

   ;; resyntax : Stx Guide -> Guide
   (define (resyntax t g) (if (and stx? (syntax? t)) `(t-resyntax ,g) g))

   (let-values ([(drivers guide) (parse-t t 0 #f #f)])
     (values (dset->list drivers) guide)))

 ;; lookup-metafun : Identifier -> Metafunction/#f
 (define (lookup-metafun id)
   (syntax-local-value/record id template-metafunction?))

 (define (dotted-prefixes id)
   (let* ([id-string (symbol->string (syntax-e id))]
          [dot-locations (map car (regexp-match-positions* #rx"\\.[^.]" id-string))])
     (for/list ([loc (in-list dot-locations)])
       (datum->syntax id (string->symbol (substring id-string 0 loc))))))

 (define (stx-dots? x) (and (identifier? x) (free-identifier=? x (quote-syntax ...))))

 (define (cons/p-guide g1 g2)
   (if (and (const-guide? g1) (const-guide? g2)) const-guide `(t-cons/p ,g1 ,g2)))

 (define ((pvar/dd<=? expected-dd) x)
   (let ([dd (pvar-dd x)]) (and dd (<= dd expected-dd))))

 (define (stx-drop n x) (for/fold ([x x]) ([i (in-range n)]) (stx-cdr x)))

 (define (restx ctx v) (if (syntax? ctx) (datum->syntax ctx v ctx ctx) v))

 ;; ----------------------------------------
 ;; Relocating (eg, template/loc)

 ;; Only relocate if relocation would affect a syntax pair originating
 ;; from template structure. For example:
 ;;   (template/loc loc-stx (1 2 3))    => okay
 ;;   (template/loc loc-stx pvar)       => don't relocate

 ;; relocate-guide : Guide Id -> Guide
 (define (relocate-guide g0 loc-id)
   (define (error/no-relocate)
     (wrong-syntax #f "cannot apply syntax location to template"))
   (define (loop g)
     (match g
       [(list 't-resyntax g1)
        (list 't-resyntax/loc g1 loc-id)]
       [(list 't-const)
        `(t-relocate ,g ,loc-id)]
       ;; ----
       [(list 't-escaped g1)
        (list 't-escaped (loop g1))]
       [(list 't-orelse g1 g2)
        (list 't-orelse (loop g1) (loop g2))]
       ;; ----
       ;; Variables shouldn't be relocated.
       [(list 't-var pvar in-try?)  g]
       ;; ----
       ;; Otherwise, cannot relocate: t-metafun, anything else?
       [_ (error/no-relocate)]))
   (loop g0))

 ;; ----------------------------------------
 ;; Compilation

 ;; compile-guide : Guide -> Syntax[Expr]
 (define (compile-guide g) (datum->syntax #'here g))

 ;; ----------------------------------------

 ;; do-template : Syntax Syntax Id/#f Boolean -> Syntax
 (define (do-template ctx tstx loc-id stx?)
   (with-disappeared-uses
     (parameterize ((current-syntax-context ctx))
       (define-values (pvars pre-guide) (parse-template tstx stx?))
       (define guide (if loc-id (relocate-guide pre-guide loc-id) pre-guide))
       (syntax-arm
        (with-syntax ([t tstx]
                      [quote-template (if stx? #'quote-syntax #'quote)]
                      [((var . pvar-val-var) ...)
                       (for/list ([pvar (in-list pvars)] #:when (pvar-dd pvar))
                         (cons (pvar-lvar pvar) (pvar-var pvar)))])
          #`(let ([var pvar-val-var] ...)
              (let ([tstx0 (quote-template t)])
                (#,(compile-guide guide) tstx0))))))))
 )

(define-syntax (template stx)
  (syntax-case stx ()
    [(template t)
     (do-template stx #'t #f #t)]
    [(template t #:properties _)
     (begin
       (log-template-error "template #:properties argument no longer supported: ~e" stx)
       (do-template stx #'t #f))]))

(define-syntax (template/loc stx)
  (syntax-case stx ()
    [(template/loc loc-expr t)
     (syntax-arm
      (with-syntax ([main-expr (do-template stx #'t #'loc-var #t)])
        #'(let ([loc-var (handle-loc '?/loc loc-expr)])
            main-expr)))]))


(define-syntax (datum-template stx)
  (syntax-case stx ()
    [(datum-template t)
     (do-template stx #'t #f #f)]))

(define (handle-loc who x)
  (if (syntax? x) x (raise-argument-error who "syntax?" x)))

;; ============================================================

(begin-for-syntax
  ;; process-quasi : Syntax -> (list Syntax[with-syntax-bindings] Syntax[expr])
  (define (process-quasi t0)
    (define bindings null)
    (define (add! binding) (set! bindings (cons binding bindings)))
    (define (process t depth)
      (define (loop t) (process t depth))
      (define (loop- t) (process t (sub1 depth)))
      (define (loop+ t) (process t (add1 depth)))
      (syntax-case t (unsyntax unsyntax-splicing quasitemplate)
        [(unsyntax expr)
         (cond [(zero? depth)
                (with-syntax ([(us) (generate-temporaries #'(us))]
                              [ctx (datum->syntax #'expr 'ctx #'expr)])
                  (add! (list #'us #'(check-unsyntax expr (quote-syntax ctx))))
                  #'us)]
               [else
                (restx t (cons (stx-car t) (loop- (stx-cdr t))))])]
        [((unsyntax-splicing expr) . _)
         (cond [(zero? depth)
                (with-syntax ([(us) (generate-temporaries #'(us))]
                              [ctx (datum->syntax #'expr 'ctx #'expr)])
                  (add! (list #'us #'(check-unsyntax-splicing expr (quote-syntax ctx))))
                  (restx t (cons #'(?@! . us) (loop (stx-cdr t)))))]
               [else
                (let ([tcar (stx-car t)]
                      [tcdr (stx-cdr t)])
                  (restx t (cons (restx tcar (cons (stx-car tcar) (loop- (stx-cdr tcar))))
                                 (loop tcdr))))])]
        [(quasitemplate _)
         (restx t (cons (stx-car t) (loop+ (stx-cdr t))))]
        [unsyntax
         (raise-syntax-error #f "misuse within quasitemplate" t0 t)]
        [unsyntax-splicing
         (raise-syntax-error #f "misuse within quasitemplate" t0 t)]
        [_
         (let ([d (if (syntax? t) (syntax-e t) t)])
           (cond [(pair? d) (restx t (cons (loop (car d)) (loop (cdr d))))]
                 [(vector? d) (restx t (list->vector (loop (vector->list d))))]
                 [(box? d) (restx t (box (loop (unbox d))))]
                 [(prefab-struct-key d)
                  => (lambda (key)
                       (apply make-prefab-struct key (loop (cdr (vector->list (struct->vector d))))))]
                 [else t]))]))
    (define t* (process t0 0))
    (list (reverse bindings) t*)))

(define-syntax (quasitemplate stx)
  (syntax-case stx ()
    [(quasitemplate t)
     (with-syntax ([(bindings t*) (process-quasi #'t)])
       #'(with-syntax bindings (template t*)))]))

(define-syntax (quasitemplate/loc stx)
  (syntax-case stx ()
    [(quasitemplate/loc loc-expr t)
     (with-syntax ([(bindings t*) (process-quasi #'t)])
       #'(with-syntax bindings
           (template/loc (handle-loc 'quasitemplate/loc loc-expr) t*)))]))

(define (check-unsyntax v ctx)
  (datum->syntax ctx v ctx))
(define (check-unsyntax-splicing v ctx)
  (unless (stx-list? v) (raise-argument-error 'unsyntax-splicing "syntax->list" v))
  (datum->syntax ctx v ctx))

;; ============================================================

(define-syntax (define-template-metafunction stx)
  (syntax-case stx ()
    [(dsm (id arg ...) . body)
     #'(dsm id (lambda (arg ...) . body))]
    [(dsm id expr)
     (identifier? #'id)
     (with-syntax ([(internal-id) (generate-temporaries #'(id))])
       #'(begin (define internal-id expr)
                (define-syntax id
                  (template-metafunction (quote-syntax internal-id)))))]))


;; ============================================================
;; Run-time support

;; Template transcription involves traversing the template syntax object,
;; substituting pattern variables etc. The interpretation of the template is
;; known at compile time, but we still need the template syntax at run time,
;; because it is the basis for generated syntax objects (via datum->syntax).

;; A template fragment (as opposed to the whole template expression) is compiled
;; to a function of type (Stx -> Stx). It receives the corresponding template
;; stx fragment as its argument. Pattern variables are passed through the
;; environment. We rely on Racket's inliner and optimizer to simplify the
;; resulting code to nearly first-order so that a new tree of closures is not
;; allocated for each template transcription.

;; Note: as an optimization, we track syntax vs non-syntax pairs in the template
;; so we can generate more specific code (hopefully smaller and faster).

(define-syntax (t-var stx)
  (syntax-case stx ()
    [(t-var #s(pvar var lvar check? _) in-try?)
     (cond [(syntax-e #'check?)
            #`(lambda (stx) (check-stx stx lvar in-try?))]
           [else
            #`(lambda (stx) lvar)])]))

(define-syntax (t-dots stx)
  (syntax-case stx ()
    ;; Case 1: (x ...) where x is trusted.
    [(t-dots (t-var #s(pvar _ lvar #f _) _) _drivers 1 #f #t _)
     (begin
       (log-template-debug "dots case 1: (x ...) where x is trusted")
       #'(lambda (stx) lvar))]
    ;; General case
    [(t-dots head ((#s(pvar _ lvar check? _) ...) ...) nesting tail cons? in-try?)
     (let ([cons? (syntax-e #'cons?)]
           [lvarss (map syntax->list (syntax->list #'((lvar ...) ...)))]
           [check?ss (syntax->datum #'((check? ...) ...))])
       (log-template-debug "dots general case: nesting = ~s, cons? = ~s, #vars = ~s"
                           (syntax-e #'nesting) cons? (apply + (map length lvarss)))
       ;; AccElem = Stx if cons? is true, (Listof Stx) otherwise
       ;; gen-level : (Listof PVar) Syntax[(Listof AccElem) -> (Listof AccElem)]
       ;;          -> Syntax[(Listof AccElem) -> (Listof AccElem)]
       (define (gen-level lvars check?s inner)
         (with-syntax ([(lvar ...) lvars]
                       [(var-value ...) (map var-value-expr lvars check?s)])
           #`(lambda (acc)
               (let loop ([acc acc] [lvar var-value] ...)
                 (check-same-length lvar ...)
                 (if (and (pair? lvar) ...)
                     (loop (let ([lvar (car lvar)] ...)
                             (#,inner acc)) ;; inner has free refs to {var ...}
                           (cdr lvar) ...)
                     acc)))))
       ;; var-value-expr : Id Boolean -> Syntax[List]
       (define (var-value-expr lvar check?)
         (if check? #`(check-list/depth stx #,lvar 1 in-try?) lvar))
       (define head-loop-code
         (let nestloop ([lvarss lvarss] [check?ss check?ss] [old-lvars null] [old-check?s null])
           (cond [(null? lvarss)
                  #'(lambda (acc) (cons (head stx) acc))]
                 [else
                  (define lvars* (append (car lvarss) old-lvars))
                  (define check?s* (append (car check?ss) old-check?s))
                  (gen-level lvars* check?s*
                             (nestloop (cdr lvarss) (cdr check?ss) lvars* check?s*))])))
       (if cons?
           #`(t-dots1* (lambda (stx) (#,head-loop-code null)) nesting (or tail (t-const)))
           #`(t-dots*  (lambda (stx) (#,head-loop-code null)) nesting (or tail (t-const)))))]))

(begin-encourage-inline

(define (stx-cadr x) (stx-car (stx-cdr x)))
(define (stx-cddr x) (stx-cdr (stx-cdr x)))
(define (stx-caddr x) (stx-car (stx-cdr (stx-cdr x))))
(define (stx-drop n x) (for/fold ([x x]) ([i (in-range n)]) (stx-cdr x)))
(define (restx basis val)
  (if (syntax? basis) (datum->syntax basis val basis basis) val))

(define ((t-resyntax g) stx) (datum->syntax stx (g (syntax-e stx)) stx stx))
(define ((t-relocate g loc) stx)
  (define new-stx (g stx))
  (datum->syntax new-stx (syntax-e new-stx) loc new-stx))
(define ((t-resyntax/loc g loc) stx)
  (datum->syntax stx (g (syntax-e stx)) loc stx))

(define ((t-const) stx) stx)
(define ((t-append/p h t) stx) (append (h (car stx)) (t (cdr stx))))
(define ((t-cons/p h t) stx) (cons (h (car stx)) (t (cdr stx))))
(define ((t-dots*  h n t) stx) (revappend* (h (car stx)) (t (stx-drop (add1 n) stx))))
(define ((t-dots1* h n t) stx) (revappend  (h (car stx)) (t (stx-drop (add1 n) stx))))
(define ((t-escaped g) stx) (g (stx-cadr stx)))
(define ((t-orelse g1 g2) stx)
  (with-handlers ([absent-pvar? (lambda (e) (if g2 (g2 (stx-caddr stx)) null))])
    (g1 (stx-cadr stx))))
(define ((t-vector g) stx) (list->vector (g (vector->list stx))))
(define ((t-box g) stx) (box (g (unbox stx))))
(define ((t-struct g) stx)
  (define key (prefab-struct-key stx))
  (define elems (cdr (vector->list (struct->vector stx))))
  (apply make-prefab-struct key (g elems)))
(define ((t-metafun mf g) stx)
  (define stx* (if (syntax? stx) stx (datum->syntax #f stx)))
  (define v (restx stx* (cons (stx-car stx) (g (stx-cdr stx)))))
  (apply-metafun mf stx* v))
(define ((h-t g) stx) (list (g stx)))
(define (h-orelse g1 g2) (t-orelse g1 g2))
(define ((h-splice g) stx)
  (let ([r (g (stx-cdr stx))])
    (or (stx->list r) (error/splice stx r))))
#| end begin-encourage-inline |#)

(define (apply-metafun mf stx v)
  (define mark (make-syntax-introducer))
  (define old-mark (current-template-metafunction-introducer))
  (parameterize ((current-template-metafunction-introducer mark))
    (define r (call-with-continuation-barrier (lambda () (mf (mark (old-mark v))))))
    (unless (syntax? r)
      (raise-syntax-error #f "result of template metafunction was not syntax" stx))
    (old-mark (mark r))))

(define (error/splice stx r)
  (raise-syntax-error 'template "splicing template did not produce a syntax list" stx))

;; revappend* : (Listof (Listof X)) (Listof X) -> (Listof X)
(define (revappend* xss ys)
  (if (null? xss) ys (revappend* (cdr xss) (append (car xss) ys))))

;; revappend : (Listof X) (Listof X) -> (Listof X)
(define (revappend xs ys)
  (if (null? xs) ys (revappend (cdr xs) (cons (car xs) ys))))

(define current-template-metafunction-introducer
  (make-parameter (lambda (stx) (if (syntax-transforming?) (syntax-local-introduce stx) stx))))

;; Used to indicate absent pvar in template; ?? catches
;; Note: not an exn, don't need continuation marks
(struct absent-pvar (ctx))

(define (check-stx ctx v in-try?)
  (cond [(syntax? v) v]
        [(promise? v) (check-stx ctx (force v) in-try?)]
        [(and in-try? (eq? v #f)) (raise (absent-pvar ctx))]
        [else (err/not-syntax ctx v)]))

(define (check-list/depth ctx v0 depth0 in-try?)
  (let depthloop ([v v0] [depth depth0])
    (cond [(zero? depth) v]
          [(and (= depth 1) (list? v)) v]
          [else
           (let loop ([v v])
             (cond [(null? v)
                    null]
                   [(pair? v)
                    (let ([new-car (depthloop (car v) (sub1 depth))]
                          [new-cdr (loop (cdr v))])
                      ;; Don't copy unless necessary
                      (if (and (eq? new-car (car v)) (eq? new-cdr (cdr v)))
                          v
                          (cons new-car new-cdr)))]
                   [(promise? v)
                    (loop (force v))]
                   [(and in-try? (eq? v #f))
                    (raise (absent-pvar ctx))]
                   [else (err/not-syntax ctx v0)]))])))

;; FIXME: use raise-syntax-error instead, pass stx args
(define check-same-length
  (case-lambda
    [(a) (void)]
    [(a b)
     (unless (= (length a) (length b))
       (error 'syntax "incompatible ellipsis match counts for template"))]
    [(a . bs)
     (define alen (length a))
     (for ([b (in-list bs)])
       (unless (= alen (length b))
         (error 'template "incompatible ellipsis match counts for template")))]))

;; Note: slightly different from error msg in syntax/parse/private/residual:
;; here says "contains" instead of "is bound to", because might be within list
(define (err/not-syntax ctx v)
  (raise-syntax-error #f (format "attribute contains non-syntax value\n  value: ~e" v) ctx))
