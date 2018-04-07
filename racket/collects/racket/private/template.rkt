(module template '#%kernel
(#%require "stx.rkt" "small-scheme.rkt" "performance-hint.rkt"
           (rename "small-scheme.rkt" define -define)
           (rename "small-scheme.rkt" define-syntax -define-syntax)
           "ellipses.rkt"
           (for-syntax "stx.rkt" "small-scheme.rkt"
                       (rename "small-scheme.rkt" define -define)
                       (rename "small-scheme.rkt" define-syntax -define-syntax)
                       "member.rkt" "sc.rkt" '#%kernel))
(#%provide syntax
           syntax/loc
           datum
           ~? ~@
           ~@! signal-absent-pvar
           (protect
            (for-syntax attribute-mapping
                        attribute-mapping?
                        attribute-mapping-name
                        attribute-mapping-var
                        attribute-mapping-depth
                        attribute-mapping-check
                        metafunction metafunction?)))

;; ============================================================
;; Syntax of templates

;; A Template (T) is one of:
;;   - pattern-variable
;;   - constant (including () and non-pvar identifiers)
;;   - (metafunction . T)
;;   - (H . T)
;;   - (H ... . T), (H ... ... . T), etc
;;   - (... T)          -- escapes inner ..., ~?, ~@
;;   - (~? T T)
;;   - #(T*)            -- actually, vector->list interpreted as T
;;   - #s(prefab-struct-key T*) -- likewise

;; A HeadTemplate (H) is one of:
;;   - T
;;   - (~? H)
;;   - (~? H H)
;;   - (~@ . T)

(define-syntax ~@! #f) ;; private, escape-ignoring version of ~@, used by unsyntax-splicing

;; ============================================================
;; Compile-time

;; Parse template syntax into a Guide (AST--the name is left over from
;; when the "guide" was a data structure interpreted at run time).

;; The AST representation is designed to coincide with the run-time
;; support, so compilation is just (datum->syntax #'here guide). The
;; variants listed below are the ones recognized and treated specially
;; by other functions (eg optimize-resyntax, relocate-guide).

;; A Guide (G) is one of:
;; - (list 't-resyntax Expr Expr G)
;; - (list 't-const Expr)     ;; constant
;; - (list 't-var Id)         ;; trusted pattern variable
;; - (list 't-list G ...)
;; - (list 't-list* G ... G)
;; - (list 't-append HG G)
;; - (list 't-orelse G G)
;; - (list 't-subst Expr Expr '({Subst} ...) Expr ...) ;; apply susbstitutions
;;   -- where Subst = Nat           ;; replace nth car with arg
;;                  | 'tail Nat     ;; replace nth cdr with arg
;;                  | 'append Nat   ;; replace nth car by appending arg
;;                  | 'recur Nat    ;; replace nth car by recurring on it with arg
;; - other expression (must be pair!)

;; A HeadGuide (HG) is one of:
;; - (list 'h-t G)
;; - other expression (must be pair!)

;; A PVar is (pvar Id Id Id/#f Nat/#f)
;;
;; The first identifier (var) is from the syntax-mapping or attribute-binding.
;; The second (lvar) is a local variable name used to hold its value (or parts
;; thereof) in ellipsis iteration. The third is #f if var is trusted to have a
;; (Listof^depth Syntax) value, or an Id reference to a Checker procedure (see
;; below) if it needs to be checked.
;;
;; The depth-delta associated with a depth>0 pattern variable is the difference
;; between the pattern variable's depth and the depth at which it is used. (For
;; depth 0 pvars, it's #f.) For example, in
;;
;;   (with-syntax ([x #'0]
;;                 [(y ...) #'(1 2)]
;;                 [((z ...) ...) #'((a b) (c d))])
;;     (template (((x y z) ...) ...)))
;;
;; the depth-delta for x is #f, the depth-delta for y is 1, and the depth-delta
;; for z is 0. The depth-delta (or depth "delay") is also the depth of the
;; ellipsis form where the variable begins to be iterated over. That is, the
;; template above should be interpreted roughly as
;;
;;   (let ([Lx (pvar-value-of x)]
;;         [Ly (pvar-value-of y)]
;;         [Lz (pvar-value-of z)])
;;     (for/list ([Lz (in-list Lz)]) ;; depth 0
;;       (for/list ([Ly (in-list Ly)] ;; depth 1
;;                  [Lz (in-list Lz)])
;;         (___ Lx Ly Lz ___))))

(begin-for-syntax

  (define here-stx (quote-syntax here))

  (define template-logger (make-logger 'template (current-logger)))

  ;; (struct pvar (var lvar check dd) #:prefab)
  (define-values (struct:pv pvar pvar? pvar-ref pvar-set!)
    (make-struct-type 'pvar #f 4 0 #f null 'prefab #f '(0 1 2 3)))
  (define (pvar-var pv) (pvar-ref pv 0))
  (define (pvar-lvar pv) (pvar-ref pv 1))
  (define (pvar-check pv) (pvar-ref pv 2))
  (define (pvar-dd pv) (pvar-ref pv 3))

  ;; An Attribute is an identifier statically bound to a syntax-mapping
  ;; (see sc.rkt) whose valvar is an identifier statically bound to an
  ;; attribute-mapping.

  ;; (struct attribute-mapping (var name depth check) ...)
  ;; check : #f (trusted) or Id, ref to Checker
  ;; Checker = ( Any d:Nat b:Boolean Syntax/#f -> (Listof^d (if b Syntax Any)) )
  (define-values (struct:attribute-mapping attribute-mapping attribute-mapping?
                                           attribute-mapping-ref _attribute-mapping-set!)
    (make-struct-type 'attribute-mapping #f 4 0 #f null (current-inspector)
                      (lambda (self stx)
                        (if (attribute-mapping-check self)
                            (let ([source-name
                                   (or (let loop ([p (syntax-property stx 'disappeared-use)])
                                         (cond [(identifier? p) p]
                                               [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                                               [else #f]))
                                       (attribute-mapping-name self))])
                              (define code
                                `(,(attribute-mapping-check self)
                                  ,(attribute-mapping-var self)
                                  ,(attribute-mapping-depth self)
                                  #t
                                  (quote-syntax ,source-name)))
                              (datum->syntax here-stx code stx))
                            (attribute-mapping-var self)))))
  (define (attribute-mapping-var a) (attribute-mapping-ref a 0))
  (define (attribute-mapping-name a) (attribute-mapping-ref a 1))
  (define (attribute-mapping-depth a) (attribute-mapping-ref a 2))
  (define (attribute-mapping-check a) (attribute-mapping-ref a 3))

  ;; (struct metafunction (var))
  (define-values (struct:metafunction metafunction metafunction? metafunction-ref _mf-set!)
    (make-struct-type 'syntax-metafunction #f 1 0 #f null (current-inspector)))
  (define (metafunction-var mf) (metafunction-ref mf 0))

  (define (ht-guide? x)
    (if (and (pair? x) (eq? (car x) 'h-t)) #t #f))
  (define (ht-guide-t x)
    (if (and (pair? x) (eq? (car x) 'h-t)) (cadr x) #f))

  (define (const-guide? x) (or (and (pair? x) (eq? (car x) 't-const)) (equal? x '(t-list))))
  (define (const-guide-v x)
    (if (eq? (car x) 't-list)
        null
        (let ([e (cadr x)])
          (if (eq? (car e) 'syntax-e) (syntax-e (cadr (cadr e))) (cadr e)))))

  (define (cons-guide g1 g2)
    (cond [(eq? (car g2) 't-list) (list* 't-list g1 (cdr g2))]
          [(eq? (car g2) 't-list*) (list* 't-list* g1 (cdr g2))]
          [else (list 't-list* g1 g2)]))

  ;; ----------------------------------------
  ;; Parsing templates

  ;; parse-template : Syntax Syntax Boolean -> (values (listof PVar) Guide (Listof Id))
  (define (parse-template ctx t stx?)
    ;; env : Hasheq[ (cons syntax-mapping Nat) => PVar ]
    (define env (make-hasheq))

    ;; wrong-syntax : Syntax Format-String Any ... -> (error)
    (define (wrong-syntax x fmt . args) (raise-syntax-error #f (apply format fmt args) ctx x))

    ;; disappeared-uses : (Listof Id)
    (define disappeared-uses null)
    ;; disappeared! : Id -> Void
    (define (disappeared! id) (set! disappeared-uses (cons id disappeared-uses)))

    ;; parse-t : Stx Nat Boolean -> (values (dsetof PVar) Guide)
    (define (parse-t t depth esc?)
      (cond [(stx-pair? t)
             (if (identifier? (stx-car t))
                 (parse-t-pair/command t depth esc?)
                 (parse-t-pair/dots t depth esc?))]
            [else (parse-t-nonpair t depth esc?)]))

    ;; parse-t-pair/command : Stx Nat Boolean -> ...
    ;; t is a stxpair w/ id in car; check if it is a "command" (metafun, escape, etc)
    (define (parse-t-pair/command t depth esc?)
      (cond [esc?
             (parse-t-pair/dots t depth esc?)]
            [(parse-form t (quote-syntax ...) 1)
             => (lambda (t)
                  (disappeared! (car t))
                  (define-values (drivers guide) (parse-t (cadr t) depth #t))
                  ;; Preserve t-escaped so that (t-escaped (t-const _)) != (t-const _)
                  (values drivers `(t-escaped ,guide)))]
            [(parse-form t (quote-syntax ~?) 2)
             => (lambda (t)
                  (disappeared! (car t))
                  (define t1 (cadr t))
                  (define t2 (caddr t))
                  (define-values (drivers1 guide1) (parse-t t1 depth esc?))
                  (define-values (drivers2 guide2) (parse-t t2 depth esc?))
                  (values (dset-union drivers1 drivers2) `(t-orelse ,guide1 ,guide2)))]
            [(lookup-metafun (stx-car t))
             => (lambda (mf)
                  (unless stx? (wrong-syntax (stx-car t) "metafunctions are not supported"))
                  (disappeared! (stx-car t))
                  (define-values (drivers guide) (parse-t (stx-cdr t) depth esc?))
                  (values drivers
                          `(t-metafun ,(metafunction-var mf) ,guide
                                      (quote-syntax
                                       ,(let ([tstx (and (syntax? t) t)])
                                          (datum->syntax tstx (cons (stx-car t) #f) tstx tstx))))))]
            [else (parse-t-pair/dots t depth esc?)]))

    ;; parse-t-pair/dots : Stx Nat Boolean -> ...
    ;; t is a stx pair; check for dots
    (define (parse-t-pair/dots t depth esc?)
      (define head (stx-car t))
      (define-values (tail nesting)
        (let loop ([tail (stx-cdr t)] [nesting 0])
          (if (and (not esc?) (stx-pair? tail)
                   (let ([x (stx-car tail)])
                     (and (identifier? x) (free-identifier=? x (quote-syntax ...)))))
              (begin (disappeared! (stx-car tail)) (loop (stx-cdr tail) (add1 nesting)))
              (values tail nesting))))
      (if (zero? nesting)
          (parse-t-pair/normal t depth esc?)
          (let-values ([(hdrivers hguide) (parse-h head (+ depth nesting) esc?)]
                       [(tdrivers tguide) (parse-t tail depth esc?)])
            (when (dset-empty? hdrivers)
              (wrong-syntax head "no pattern variables before ellipsis in template"))
            (when (dset-empty? (dset-filter hdrivers (pvar/dd<=? depth)))
              (let ([bad-dots ;; select the nestingth (last) ellipsis as the bad one
                     (stx-car (stx-drop nesting t))])
                ;; FIXME: improve error message?
                (wrong-syntax bad-dots "too many ellipses in template")))
            ;; hdrivers is (listof (dsetof pvar))
            (define hdriverss ;; per level
              (let loop ([i 0])
                (if (< i nesting)
                    (cons (dset-filter hdrivers (pvar/dd<=? (+ depth i)))
                          (loop (add1 i)))
                    null)))
            (define at-stx (datum->syntax #f '... head))
            (define hg
              (let loop ([hdriverss hdriverss])
                (cond [(null? (cdr hdriverss))
                       (let ([cons? (ht-guide? hguide)]
                             [hguide (if (ht-guide? hguide) (ht-guide-t hguide) hguide)])
                         `(t-dots ,cons? ,hguide ,(car hdriverss)
                                  (quote ,head) (quote-syntax ,at-stx)))]
                      [else (let ([inner (loop (cdr hdriverss))])
                              `(t-dots #f ,inner ,(car hdriverss)
                                       (quote ,head) (quote-syntax ,at-stx)))])))
            (values (dset-union hdrivers tdrivers)
                    (if (equal? tguide '(t-list))
                        (resyntax t hg)
                        (resyntax t `(t-append ,hg ,tguide)))))))

    ;; parse-t-pair/normal : Stx Nat Boolean -> ...
    ;; t is a normal stx pair
    (define (parse-t-pair/normal t depth esc?)
      (define-values (hdrivers hguide) (parse-h (stx-car t) depth esc?))
      (define-values (tdrivers tguide) (parse-t (stx-cdr t) depth esc?))
      (values (dset-union hdrivers tdrivers)
              (resyntax t
                        (if (ht-guide? hguide)
                            (let ([hguide (ht-guide-t hguide)])
                              (if (and (const-guide? hguide) (const-guide? tguide))
                                  (const-guide t)
                                  (cons-guide hguide tguide)))
                            (if (equal? tguide '(t-list))
                                hguide
                                `(t-append ,hguide ,tguide))))))

    ;; parse-t-nonpair : Syntax Nat Boolean -> ...
    ;; PRE: t is not a stxpair
    (define (parse-t-nonpair t depth esc?)
      (define td (if (syntax? t) (syntax-e t) t))
      (cond [(identifier? t)
             (cond [(and (not esc?)
                         (or (free-identifier=? t (quote-syntax ...))
                             (free-identifier=? t (quote-syntax ~?))
                             (free-identifier=? t (quote-syntax ~@))))
                    (wrong-syntax t "illegal use")]
                   [(lookup-metafun t)
                    (wrong-syntax t "illegal use of syntax metafunction")]
                   [(lookup t depth)
                    => (lambda (pvar)
                         (disappeared! t)
                         (values (dset pvar)
                                 (cond [(pvar-check pvar)
                                        => (lambda (check)
                                             `(#%expression
                                               (,check ,(pvar-lvar pvar) 0 #t (quote-syntax ,t))))]
                                       [else `(t-var ,(pvar-lvar pvar))])))]
                   [else (values (dset) (const-guide t))])]
            [(vector? td)
             (define-values (drivers guide) (parse-t (vector->list td) depth esc?))
             (values drivers
                     (cond [(const-guide? guide) (const-guide t)]
                           [else (resyntax t `(t-vector ,guide))]))]
            [(prefab-struct-key td)
             => (lambda (key)
                  (define-values (drivers guide)
                    (let ([elems (cdr (vector->list (struct->vector td)))])
                      (parse-t elems depth esc?)))
                  (values drivers
                          (cond [(const-guide? guide) (const-guide t)]
                                [else (resyntax t `(t-struct (quote ,key) ,guide))])))]
            [(box? td)
             (define-values (drivers guide) (parse-t (unbox td) depth esc?))
             (values drivers (if (const-guide? guide) (const-guide t) (resyntax t `(t-box ,guide))))]
            [else (values (dset) (const-guide t))]))

    ;; parse-h : Syntax Nat Boolean -> (values (dsetof PVar) HeadGuide)
    (define (parse-h h depth esc?)
      (cond [(and (not esc?) (parse-form h (quote-syntax ~?) 1))
             => (lambda (h)
                  (disappeared! (car h))
                  (define-values (drivers guide) (parse-h (cadr h) depth esc?))
                  (values drivers `(h-orelse ,guide null)))]
            [(and (not esc?) (parse-form h (quote-syntax ~?) 2))
             => (lambda (h)
                  (disappeared! (car h))
                  (define-values (drivers1 guide1) (parse-h (cadr h) depth esc?))
                  (define-values (drivers2 guide2) (parse-h (caddr h) depth esc?))
                  (values (dset-union drivers1 drivers2)
                          (if (and (ht-guide? guide1) (ht-guide? guide2))
                              `(h-t (t-orelse ,(ht-guide-t guide1) ,(ht-guide-t guide2)))
                              `(h-orelse ,guide1 ,guide2))))]
            [(and (stx-pair? h)
                  (let ([h-head (stx-car h)])
                    (and (identifier? h-head)
                         (or (and (free-identifier=? h-head (quote-syntax ~@)) (not esc?))
                             (free-identifier=? h-head (quote-syntax ~@!))))))
             (disappeared! (stx-car h))
             (define-values (drivers guide) (parse-t (stx-cdr h) depth esc?))
             (values drivers `(h-splice ,guide (quote ,h) (quote-syntax ,(stx-car h))))]
            [else
             (define-values (drivers guide) (parse-t h depth esc?))
             (values drivers `(h-t ,guide))]))

    ;; lookup : Identifier Nat -> PVar/#f
    (define (lookup id depth)
      (define (make-pvar var check pvar-depth)
        (cond [(zero? pvar-depth)
               (pvar var var check #f)]
              [(>= depth pvar-depth)
               (pvar var (gentemp) check (- depth pvar-depth))]
              [(zero? depth)
               (wrong-syntax id "missing ellipsis with pattern variable in template")]
              [else
               (wrong-syntax id "too few ellipses for pattern variable in template")]))
      (define (hash-ref! h k proc)
        (let ([v (hash-ref h k #f)]) (if v v (let ([v* (proc)]) (hash-set! h k v*) v*))))
      (let ([v (syntax-local-value id (lambda () #f))])
        (cond [(syntax-pattern-variable? v)
               (hash-ref! env (cons v depth)
                 (lambda ()
                   (define pvar-depth (syntax-mapping-depth v))
                   (define attr
                     (let ([attr (syntax-local-value (syntax-mapping-valvar v) (lambda () #f))])
                       (and (attribute-mapping? attr) attr)))
                   (define var (if attr (attribute-mapping-var attr) (syntax-mapping-valvar v)))
                   (define check (and attr (attribute-mapping-check attr)))
                   (make-pvar var check pvar-depth)))]
              [(s-exp-pattern-variable? v)
               (hash-ref! env (cons v depth)
                 (lambda ()
                   (define pvar-depth (s-exp-mapping-depth v))
                   (define var (s-exp-mapping-valvar v))
                   (make-pvar var #f pvar-depth)))]
              [else
               ;; id is a constant; check that for all x s.t. id = x.y, x is not an attribute
               (for-each
                (lambda (pfx)
                  (let ([pfx-v (syntax-local-value pfx (lambda () #f))])
                    (if (and (syntax-pattern-variable? pfx-v)
                             (let ([valvar (syntax-mapping-valvar pfx-v)])
                               (attribute-mapping? (syntax-local-value valvar (lambda () #f)))))
                        (wrong-syntax id "undefined nested attribute of attribute `~a'"
                                      (syntax-e pfx))
                        (void))))
                (dotted-prefixes id))
               #f])))

    ;; resyntax : Stx Guide -> Guide
    (define (resyntax t0 g)
      (if (and stx? (syntax? t0))
          (cond [(const-guide? g) (const-guide t0)]
                [else (optimize-resyntax t0 g)])
          g))

    ;; optimize-resyntax : Syntax Guide -> Guide
    (define (optimize-resyntax t0 g)
      (define HOLE (datum->syntax #f '_))
      (define (finish i rt rs re)
        (values (sub1 i) (reverse rs) (reverse re)
                (datum->syntax t0 (apply list* (reverse rt)) t0 t0)))
      (define (loop-gs list*? gs i rt rs re)
        (cond [(null? gs)
               (finish i (cons null rt) rs re)]
              [(and list*? (null? (cdr gs)))
               (loop-g (car gs) i rt rs re)]
              [else
               (define g0 (car gs))
               (cond [(const-guide? g0)
                      (let ([const (const-guide-v g0)])
                        (loop-gs list*? (cdr gs) (add1 i) (cons const rt) rs re))]
                     [(eq? (car g0) 't-subst) ;; (t-subst LOC STX <substs>)
                      (let ([subt (cadr (list-ref g0 2))] ;; extract from (quote-syntax _)
                            [subargs (list-tail g0 3)])
                        (loop-gs list*? (cdr gs) (add1 i) (cons subt rt)
                                 (list* i 'recur rs) (cons `(list . ,subargs) re)))]
                     [else (loop-gs list*? (cdr gs) (add1 i) (cons HOLE rt)
                                    (cons i rs) (cons g0 re))])]))
      (define (loop-g g i rt rs re)
        (cond [(eq? (car g) 't-list) (loop-gs #f (cdr g) i rt rs re)]
              [(eq? (car g) 't-list*) (loop-gs #t (cdr g) i rt rs re)]
              [(eq? (car g) 't-append)
               (loop-g (caddr g) (add1 i) (cons HOLE rt)
                       (list* i 'append rs) (cons (cadr g) re))]
              [(eq? (car g) 't-const)
               (let ([const (const-guide-v g)])
                 (finish i (cons const rt) rs re))]
              [else (finish i (cons HOLE rt) (list* i 'tail rs) (cons g re))]))
      (define-values (npairs substs exprs t*) (loop-g g 0 null null null))
      (cond [(and substs
                  ;; Tunable condition for choosing whether to create a t-subst.
                  ;; Avoid creating useless (t-subst loc stx '(tail 0) g).
                  (<= (length substs) (* 2 npairs)))
             #;(log-message template-logger 'debug
                            (format "OPTIMIZED ~s" (syntax->datum t0)) #f)
             `(t-subst #f (quote-syntax ,t*) (quote ,substs) . ,exprs)]
            [else
             #;(log-message template-logger 'debug
                            (format "NOT opt   ~s" (syntax->datum t0)) #f)
             (let ([rep (datum->syntax t0 'STX t0 t0)])
               `(t-resyntax #f (quote-syntax ,rep) ,g))]))

    ;; const-guide : Any -> Guide
    (define (const-guide x)
      (cond [(null? x) `(t-list)]
            [(not stx?) `(t-const (quote ,x))]
            [(syntax? x) `(t-const (quote-syntax ,x))]
            [else `(t-const (syntax-e (quote-syntax ,(datum->syntax #f x))))]))

    (let-values ([(drivers guide) (parse-t t 0 #f)])
      (values (dset->list drivers) guide disappeared-uses)))

  ;; parse-form : Stx Id Nat -> (list[arity+1] Syntax)
  (define (parse-form stx form-id arity)
    (and (stx-pair? stx)
         (let ([stx-h (stx-car stx)] [stx-t (stx-cdr stx)])
           (and (identifier? stx-h) (free-identifier=? stx-h form-id)
                (let ([stx-tl (stx->list stx-t)])
                  (and (list? stx-tl)
                       (= (length stx-tl) arity)
                       (cons stx-h stx-tl)))))))

  ;; lookup-metafun : Identifier -> Metafunction/#f
  (define (lookup-metafun id)
    (define v (syntax-local-value id (lambda () #f)))
    (and (metafunction? v) v))

  (define (dotted-prefixes id)
    (let* ([id-string (symbol->string (syntax-e id))]
           [dot-locations
            (let loop ([i 0])
              (if (< i (string-length id-string))
                  (if (eqv? (string-ref id-string i) #\.)
                      (cons i (loop (add1 i)))
                      (loop (add1 i)))
                  null))])
      (map (lambda (loc) (datum->syntax id (string->symbol (substring id-string 0 loc))))
           dot-locations)))

  (define (pvar/dd<=? expected-dd)
    (lambda (x) (let ([dd (pvar-dd x)]) (and dd (<= dd expected-dd)))))

  (define gentemp-counter 0)
  (define (gentemp)
    (set! gentemp-counter (add1 gentemp-counter))
    ((make-syntax-introducer)
     (datum->syntax #f (string->symbol (format "pv_~s" gentemp-counter)))))

  (define (stx-drop n x)
    (if (zero? n) x (stx-drop (sub1 n) (stx-cdr x))))

  ;; ----------------------------------------
  ;; Deterministic Sets
  ;; FIXME: detect big unions, use hash table

  (define (dset . xs) xs)
  (define (dset-empty? ds) (null? ds))
  (define (dset-filter ds pred) (filter pred ds))
  (define (dset->list ds) ds)
  (define (dset-union ds1 ds2)
    (if (pair? ds1)
        (let ([elem (car ds1)])
          (if (member elem ds2)
              (dset-union (cdr ds1) ds2)
              (dset-union (cdr ds1) (cons (car ds1) ds2))))
        ds2))

  (define (filter keep? xs)
    (if (pair? xs)
        (if (keep? (car xs))
            (cons (car xs) (filter keep? (cdr xs)))
            (filter keep? (cdr xs)))
        null))

  ;; ----------------------------------------
  ;; Relocating (eg, syntax/loc)

  ;; Only relocate if relocation would affect a syntax pair originating
  ;; from template structure. For example (x,y are pvars):
  ;;   (syntax/loc loc-stx (1 2 3))    => relocate
  ;;   (syntax/loc loc-stx y)          => don't relocate
  ;;   (syntax/loc loc-stx (x ... . y) => relocate iff at least one x!
  ;; Deciding whether to relocate after the fact is hard. But with explicit
  ;; t-resyntax, it's much easier.

  ;; relocate-guide : Syntax Guide Id -> Guide
  (define (relocate-guide ctx g0 loc-id)
    (define (loop g)
      (define gtag (car g))
      (cond [(eq? gtag 't-resyntax)
             `(t-resyntax ,loc-id . ,(cddr g))]
            [(eq? gtag 't-const)
             `(t-relocate ,g ,loc-id)]
            [(eq? gtag 't-subst)
             `(t-subst ,loc-id . ,(cddr g))]
            ;; ----
            [(eq? gtag 't-escaped)
             `(t-escaped ,(loop (cadr g)))]
            [(eq? gtag 't-orelse)
             `(t-orelse ,(loop (cadr g)) ,(loop (caddr g)))]
            ;; ----
            ;; Nothing else should be relocated
            [else g]))
    (loop g0))

  ;; ----------------------------------------

  ;; do-template : Syntax Syntax Id/#f Boolean -> Syntax
  (define (do-template ctx tstx loc-id stx?)
    (define-values (pvars pre-guide disappeared-uses)
      (parse-template ctx tstx stx?))
    (define guide (if loc-id (relocate-guide ctx pre-guide loc-id) pre-guide))
    (define ell-pvars (filter pvar-dd pvars))
    (define pre-code
      (if (const-guide? guide)
          (if stx? `(quote-syntax ,tstx) `(quote ,tstx))
          (let ([lvars (map pvar-lvar ell-pvars)]
                [valvars (map pvar-var ell-pvars)])
            `(let (,@(map list lvars valvars))
               ,(datum->syntax here-stx guide)))))
    (define code (syntax-arm (datum->syntax here-stx pre-code ctx)))
    (syntax-property code 'disappeared-use (map syntax-local-introduce disappeared-uses)))
  )

(define-syntax (syntax stx)
  (define s (syntax->list stx))
  (if (and (list? s) (= (length s) 2))
      (do-template stx (cadr s) #f #t)
      (raise-syntax-error #f "bad syntax" stx)))

(define-syntax (syntax/loc stx)
  (define s (syntax->list stx))
  (if (and (list? s) (= (length s) 3))
      (let ([loc-id (quote-syntax loc)])
        (define code
          `(let ([,loc-id (check-loc (quote ,(car s)) ,(cadr s))])
             ,(do-template stx (caddr s) loc-id #t)))
        (syntax-arm (datum->syntax here-stx code stx)))
      (raise-syntax-error #f "bad syntax" stx)))

(define-syntax (datum stx)
  (define s (syntax->list stx))
  (if (and (list? s) (= (length s) 2))
      (do-template stx (cadr s) #f #f)
      (raise-syntax-error #f "bad syntax" stx)))

;; check-loc : Symbol Any -> (U Syntax #f)
;; Raise exn if not syntax. Returns same syntax if suitable for srcloc
;; (ie, if at least syntax-source or syntax-position set), #f otherwise.
(define (check-loc who x)
  (if (syntax? x)
      (if (or (syntax-source x) (syntax-position x))
          x
          #f)
      (raise-argument-error who "syntax?" x)))

;; ============================================================
;; Run-time support

;; (t-dots cons? hguide hdrivers) : Expr[(Listof Syntax)]
(define-syntax (t-dots stx)
  (define s (syntax->list stx))
  (define cons? (syntax-e (list-ref s 1)))
  (define head (list-ref s 2))
  (define drivers (map syntax-e (syntax->list (list-ref s 3)))) ;; (Listof PVar)
  (define in-stx (list-ref s 4))
  (define at-stx (list-ref s 5))
  (cond
    ;; Case 1: (x ...) where x is trusted
    [(and cons? (let ([head-s (syntax->list head)])
                  (and (pair? head-s) (eq? (syntax-e (car head-s)) 't-var))))
     head]
    ;; General case
    [else
     ;; var-value-expr : Id Id/#'#f -> Expr[List]
     (define (var-value-expr lvar check)
       (if (syntax-e check) `(,check ,lvar 1 #f #f) lvar))
     (define lvars (map pvar-lvar drivers))
     (define checks (map pvar-check drivers))
     (define code
       `(let ,(map list lvars (map var-value-expr lvars checks))
          ,(if (> (length lvars) 1) `(check-same-length ,in-stx ,at-stx . ,lvars) '(void))
          ,(if cons?
               `(map (lambda ,lvars ,head) . ,lvars)
               `(apply append (map (lambda ,lvars ,head) . ,lvars)))))
     (datum->syntax here-stx code stx)]))

(define-syntaxes (t-orelse h-orelse)
  (let ()
    (define (orelse-transformer stx)
      (define s (syntax->list stx))
      (datum->syntax here-stx
                     `(t-orelse* (lambda () ,(cadr s)) (lambda () ,(caddr s)))))
    (values orelse-transformer orelse-transformer)))

(#%require (rename '#%kernel t-const    #%expression)
           (rename '#%kernel t-var      #%expression)
           ;; (rename '#%kernel t-append   append)
           (rename '#%kernel t-list     list)
           (rename '#%kernel t-list*    list*)
           (rename '#%kernel t-escaped  #%expression)
           (rename '#%kernel t-vector   list->vector)
           (rename '#%kernel t-box      box-immutable)
           (rename '#%kernel h-t        list))

(begin-encourage-inline

(define (t-append xs ys) (if (null? ys) xs (append xs ys)))
(define (t-resyntax loc stx g) (datum->syntax stx g (or loc stx) stx))
(define (t-relocate g loc) (datum->syntax g (syntax-e g) (or loc g) g))
(define (t-orelse* g1 g2)
  ((let/ec escape
     (with-continuation-mark
       absent-pvar-escape-key
       (lambda () (escape g2))
       (let ([v (g1)]) (lambda () v))))))
(define (t-struct key g) (apply make-prefab-struct key g))
(define (t-metafun mf g stx)
  (mf (datum->syntax stx (cons (stx-car stx) g) stx stx)))
(define (h-splice g in-stx at-stx)
  (if (stx-list? g) (stx->list g) (error/splice g in-stx at-stx)))

#| end begin-encourage-inline |#)

;; t-subst : Syntax/#f Syntax Substs Any ... -> Syntax
;; where Substs = '() | (cons Nat Substs) | (list* (U 'tail 'append 'recur) Nat Substs)
;; There is one arg for each index in substs. See also defn of Guide above.
(define (t-subst loc stx substs . args)
  (define (loop/mode s i mode seek substs args)
    (cond [(< i seek) (cons (car s) (loop/mode (cdr s) (add1 i) mode seek substs args))]
          [(eq? mode #f) (cons (car args) (loop (cdr s) (add1 i) substs (cdr args)))]
          [(eq? mode 'tail) (car args)]
          [(eq? mode 'append) (append (car args) (loop (cdr s) (add1 i) substs (cdr args)))]
          [(eq? mode 'recur) (cons (apply t-subst #f (car s) (car args))
                                   (loop (cdr s) (add1 i) substs (cdr args)))]))
  (define (loop s i substs args)
    (cond [(null? substs) s]
          [(symbol? (car substs))
           (loop/mode s i (car substs) (cadr substs) (cddr substs) args)]
          [else (loop/mode s i #f (car substs) (cdr substs) args)]))
  (define v (loop (syntax-e stx) 0 substs args))
  (datum->syntax stx v (or loc stx) stx))

(define absent-pvar-escape-key (gensym 'absent-pvar-escape))

;; signal-absent-pvar : -> escapes or #f
;; Note: Only escapes if in ~? form.
(define (signal-absent-pvar)
  (let ([escape (continuation-mark-set-first #f absent-pvar-escape-key)])
    (if escape (escape) #f)))

;; error/splice : Any Stx Stx -> (escapes)
(define (error/splice r in-stx at-stx)
  (raise-syntax-error 'syntax
    (format "splicing template did not produce a syntax list\n  got: ~e" r) in-stx at-stx))

;; check-same-length : Stx Stx List ... -> Void
(define check-same-length
  (case-lambda
    [(in at a) (void)]
    [(in at a b)
     (if (= (length a) (length b))
         (void)
         (raise-syntax-error 'syntax "incompatible ellipsis match counts for template"
                             (list in '...) at))]
    [(in at a . bs)
     (define alen (length a))
     (for-each (lambda (b)
                 (if (= alen (length b))
                     (void)
                     (raise-syntax-error 'syntax "incompatible ellipsis match counts for template"
                                         (list in '...) at)))
               bs)]))

)
