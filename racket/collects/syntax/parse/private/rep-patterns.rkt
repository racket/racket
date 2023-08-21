#lang racket/base
(require (submod "residual.rkt" ct)
         "rep-attrs.rkt"
         "minimatch.rkt"
         "tree-util.rkt"
         racket/syntax)
(provide (all-defined-out))

;; Uses Arguments from kws.rkt

;; ------------------------------------------------------------
;; Stage 1: Parsing, first pass

;; Pattern parsing is done (in rep.rkt) in two passes. In pass 1, stxclass refs
;; are not required to be bound, and so patterns like `x:sc` and `(~var x sc)`
;; are left as "fixup" patterns to be resolved in pass 2.

;; SinglePattern ::=
;; | (pat:any)
;; | (pat:svar Id)  -- "simple" var, no stxclass
;; | (pat:var/p Id/#f Expr Arguments (Listof IAttr) Expr[Role] SCOpts) -- var with parser
;; | (pat:literal Id Expr Expr)
;; | (pat:datum Datum)
;; | (pat:action ActionPattern SinglePattern)
;; | (pat:head HeadPattern SinglePattern)
;; | (pat:dots (listof EllipsisHeadPattern) SinglePattern)
;; | (pat:andu (Listof (U SinglePattern ActionPattern)))
;; | (pat:or (listof IAttr) (listof SinglePattern) (listof (listof IAttr)))
;; | (pat:not SinglePattern)
;; | (pat:vector SinglePattern)
;; | (pat:box SinglePattern)
;; | (pat:pstruct key SinglePattern)
;; | (pat:describe SinglePattern Expr[String/#f] Boolean Expr[Role])
;; | (pat:delimit SinglePattern)
;; | (pat:commit SinglePattern)
;; | (pat:reflect Expr[RSC] Arguments (Listof SAttr) Id/#f (Listof IAttr))
;; | (pat:post SinglePattern)
;; | (pat:integrated Id/#f Id String Expr[Role])
;; | (pat:fixup Syntax Id/#f Id Id Arguments String Expr[Role] Id/#f)
;; | (pat:and/fixup Syntax (Listof (U {S,H,A}Pattern)))

;; ListPattern ::=
;; | (pat:datum '())
;; | (pat:action ActionPattern ListPattern)
;; | (pat:head HeadPattern ListPattern)
;; | (pat:pair SinglePattern ListPattern)
;; | (pat:dots EllipsisHeadPattern ListPattern)

;; ActionPattern ::=
;; | (action:cut)
;; | (action:fail Expr Expr)
;; | (action:bind IAttr Expr)
;; | (action:and (Listof ActionPattern))
;; | (action:parse SinglePattern Expr)
;; | (action:do (Listof Def/Expr))
;; | (action:undo (Listof Def/Expr))
;; | (action:post ActionPattern)

;; HeadPattern ::=
;; | (hpat:single SinglePattern)
;; | (hpat:var/p Id/#f Expr Arguments (Listof IAttr) Expr[Role] SCOpts)
;; | (hpat:seq ListPattern)
;; | (hpat:action ActionPattern HeadPattern)
;; | (hpat:andu (Listof (U Headpattern ActionPattern))) -- at least one HeadPattern
;; | (hpat:or (Listof IAttr) (Listof HeadPattern) (Listof (Listof IAttr)))
;; | (hpat:describe HeadPattern Expr[String/#f] Boolean Expr[Role])
;; | (hpat:delimit HeadPattern)
;; | (hpat:commit HeadPattern)
;; | (hpat:reflect Expr[RSC] Arguments (Listof SAttr) Id/#f (Listof IAttr))
;; | (hpat:post HeadPattern)
;; | (hpat:peek HeadPattern)
;; | (hpat:peek-not HeadPattern)

;; EllipsisHeadPattern ::=
;; | (ehpat (Listof IAttr) HeadPattern RepConstraint Boolean)

;; RepConstraint ::=
;; | (rep:once Expr Expr Expr)
;; | (rep:optional Expr Expr (Listof BindAction))
;; | (rep:bounds Nat PosInt/+inf.0 Expr Expr Expr)
;; | #f

;; BindAction ::= (action:bind IAttr Expr)
;; SideClause ::= ActionPattern

;; ------------------------------------------------------------
;; Stage 2: Parsing, pass 2

;; SinglePattern ::= ....
;; X (pat:fixup Syntax Id/#f Id Id Arguments String Expr[Role] Id/#f)
;; X (pat:and/fixup Syntax (Listof (U {S,H,A}Pattern)))

;; Note: pat:action can change to hpat:action; pat:andu cannot change.

;; ------------------------------------------------------------
;; Stage 3: Specialize pair patterns

;; Rewrite (pat:head (hpat:single headp) tailp) => (pat:pair headp tailp).
;; Rewrite (pat:head (hpat:seq lp[end]) tailp) -> lp[tailp].

;; FIXME/TODO: also do the following:
;; - add pat:seq-end
;; - rewrite (pat:head (hpat:seq (pat:head h1 t1)) t2) => (pat:head h1 (pat:head (hpat:seq t1) t2))

;; SinglePattern ::= ....
;; + (pat:pair SinglePattern SinglePattern)

;; ListPattern ::=
;; + (pat:pair SinglePattern ListPattern)

;; ------------------------------------------------------------
;; Stage 4a: Normalize and patterns

;; SinglePattern ::= ....
;; X (pat:action ActionPattern SinglePattern)

;; ActionPattern ::= ....
;; X (action:and (Listof ActionPattern))

;; HeadPattern ::=
;; X (hpat:action ActionPattern HeadPattern)

;; ------------------------------------------------------------
;; Stage 4b: Add *:ord wrappers for *:and components

;; SinglePattern ::= ....
;; X (pat:andu (Listof (U SinglePattern ActionPattern)))
;; + (pat:action ActionPattern SinglePattern)
;; + (pat:and (Listof SinglePattern))
;; + (pat:ord SinglePattern UninternedSymbol Nat)

;; ActionPattern ::= ....
;; + (action:ord ActionPattern UninternedSymbol Nat)
;; + (action:and (Listof ActionPattern))

;; HeadPattern ::= ....
;; X (hpat:andu (Listof (U HeadPattern ActionPattern)))
;; + (hpat:action ActionPattern HeadPattern)
;; + (hpat:and HeadPattern SinglePattern)
;; + (hpat:ord HeadPattern UninternedSymbol Nat)

;; ------------------------------------------------------------
;; Stage 5: Switch to pat:seq-end in list patterns

;; ListPattern ::= ...
;; X (pat:datum '())
;; + (pat:seq-end)

;; ------------------------------------------------------------
;; Stage 6 (Optional): Simplify patterns (see "parse-interp.rkt")

;; SinglePattern ::=
;; + (pat:simple (Listof IAttr) Simple)

;; ------------------------------------------------------------

(define-struct pat:any () #:prefab)
(define-struct pat:svar (name) #:prefab)
(define-struct pat:var/p (name parser argu nested-attrs role opts) #:prefab)
(define-struct pat:literal (id input-phase lit-phase) #:prefab)
(define-struct pat:datum (datum) #:prefab)
(define-struct pat:action (action inner) #:prefab)
(define-struct pat:head (head tail) #:prefab)
(define-struct pat:dots (heads tail) #:prefab)
(define-struct pat:andu (patterns) #:prefab)
(define-struct pat:and (patterns) #:prefab)
(define-struct pat:or (attrs patterns attrss) #:prefab)
(define-struct pat:not (pattern) #:prefab)
(define-struct pat:pair (head tail) #:prefab)
(define-struct pat:vector (pattern) #:prefab)
(define-struct pat:box (pattern) #:prefab)
(define-struct pat:pstruct (key pattern) #:prefab)
(define-struct pat:describe (pattern description transparent? role) #:prefab)
(define-struct pat:delimit (pattern) #:prefab)
(define-struct pat:commit (pattern) #:prefab)
(define-struct pat:reflect (obj argu attr-decls name nested-attrs) #:prefab)
(define-struct pat:ord (pattern group index) #:prefab)
(define-struct pat:post (pattern) #:prefab)
(define-struct pat:integrated (name predicate description role) #:prefab)
(define-struct pat:fixup (stx bind varname scname argu sep role parser*) #:prefab)
(define-struct pat:and/fixup (stx patterns) #:prefab)
(define-struct pat:simple (iattrs simple) #:prefab)
(define-struct pat:seq-end () #:prefab)

(define-struct action:cut () #:prefab)
(define-struct action:fail (when message) #:prefab)
(define-struct action:bind (attr expr) #:prefab)
(define-struct action:and (patterns) #:prefab)
(define-struct action:parse (pattern expr) #:prefab)
(define-struct action:do (stmts) #:prefab)
(define-struct action:undo (stmts) #:prefab)
(define-struct action:ord (pattern group index) #:prefab)
(define-struct action:post (pattern) #:prefab)

(define-struct hpat:single (pattern) #:prefab)
(define-struct hpat:var/p (name parser argu nested-attrs role scopts) #:prefab)
(define-struct hpat:seq (inner) #:prefab)
(define-struct hpat:action (action inner) #:prefab)
(define-struct hpat:andu (patterns) #:prefab)
(define-struct hpat:and (head single) #:prefab)
(define-struct hpat:or (attrs patterns attrss) #:prefab)
(define-struct hpat:describe (pattern description transparent? role) #:prefab)
(define-struct hpat:delimit (pattern) #:prefab)
(define-struct hpat:commit (pattern) #:prefab)
(define-struct hpat:reflect (obj argu attr-decls name nested-attrs) #:prefab)
(define-struct hpat:ord (pattern group index) #:prefab)
(define-struct hpat:post (pattern) #:prefab)
(define-struct hpat:peek (pattern) #:prefab)
(define-struct hpat:peek-not (pattern) #:prefab)

(define-struct ehpat (attrs head repc check-null?) #:prefab)
(define-struct rep:once (name under-message over-message) #:prefab)
(define-struct rep:optional (name over-message defaults) #:prefab)
(define-struct rep:bounds (min max name under-message over-message) #:prefab)

(define repc:plus (make-rep:bounds 1 +inf.0 #f #f #f)) ;; used for ...+

(module simple racket/base
  ;; S ::=
  ;; | _                  -- match any, don't bind
  ;; | var                -- match any, bind
  ;; | 'id                -- match id, bind
  ;; | 'expr              -- match expr, bind
  ;; | 'seq-end
  ;; | ()                 -- match null
  ;; | (S . S)            -- match pair
  ;; | #s(dots S Nat #f)  -- match (S ... . ())
  ;; | #s(dots S Nat #t)  -- match (S ...+ . ())
  ;; | #s(datum Datum)
  ;; | #s(describe S String Bool)
  (struct sim:dots (simple nattrs plus?) #:prefab)
  (struct sim:datum (datum) #:prefab)
  (struct sim:describe (simple desc transp?) #:prefab)
  (provide (all-defined-out)))

;; ============================================================

(define (single-pattern? x)
  (or (pat:any? x)
      (pat:svar? x)
      (pat:var/p? x)
      (pat:literal? x)
      (pat:datum? x)
      (pat:action? x)
      (pat:head? x)
      (pat:dots? x)
      (pat:andu? x)
      (pat:and? x)
      (pat:or? x)
      (pat:not? x)
      (pat:pair? x)
      (pat:vector? x)
      (pat:box? x)
      (pat:pstruct? x)
      (pat:describe? x)
      (pat:delimit? x)
      (pat:commit? x)
      (pat:reflect? x)
      (pat:ord? x)
      (pat:post? x)
      (pat:integrated? x)
      (pat:fixup? x)
      (pat:and/fixup? x)
      (pat:simple? x)
      (pat:seq-end? x)))

(define (action-pattern? x)
  (or (action:cut? x)
      (action:bind? x)
      (action:fail? x)
      (action:and? x)
      (action:parse? x)
      (action:do? x)
      (action:undo? x)
      (action:ord? x)
      (action:post? x)))

(define (head-pattern? x)
  (or (hpat:single? x)
      (hpat:var/p? x)
      (hpat:seq? x)
      (hpat:action? x)
      (hpat:andu? x)
      (hpat:and? x)
      (hpat:or? x)
      (hpat:describe? x)
      (hpat:delimit? x)
      (hpat:commit? x)
      (hpat:reflect? x)
      (hpat:ord? x)
      (hpat:post? x)
      (hpat:peek? x)
      (hpat:peek-not? x)))

(define (ellipsis-head-pattern? x)
  (ehpat? x))

(define (single-or-head-pattern? x)
  (or (single-pattern? x)
      (head-pattern? x)))

(define (*pattern? x)
  (and (struct? x)
       (or (single-pattern? x)
           (action-pattern? x)
           (head-pattern? x)
           (ellipsis-head-pattern? x))))

;; ============================================================

(define (wf-S? x)
  (match x
    [(pat:any) #t]
    [(pat:svar name) #t]
    [(pat:var/p name parser argu nested-attrs role opts) #t]
    [(pat:literal id input-phase lit-phase) #t]
    [(pat:datum datum) #t]
    [(pat:action ap sp) (and (wf-A? ap) (wf-S? sp))]
    [(pat:head headp tailp) (and (wf-H? headp) (wf-S? tailp))]
    [(pat:dots heads tailp) (and (andmap wf-EH? heads) (wf-S? tailp))]
    [(pat:andu ps) (andmap wf-A/S? ps)]
    [(pat:and ps) (andmap wf-S? ps)]
    [(pat:or attrs ps attrss) (andmap wf-S? ps)]
    [(pat:not sp) (wf-S? sp)]
    [(pat:pair headp tailp) (and (wf-S? headp) (wf-S? tailp))]
    [(pat:vector sp) (wf-S? sp)]
    [(pat:box sp) (wf-S? sp)]
    [(pat:pstruct key sp) (wf-S? sp)]
    [(pat:describe sp description transparent? role) (wf-S? sp)]
    [(pat:delimit sp) (wf-S? sp)]
    [(pat:commit sp) (wf-S? sp)]
    [(pat:reflect obj argu attr-decls name nested-attrs) #t]
    [(pat:ord sp group index) (wf-S? sp)]
    [(pat:post sp) (wf-S? sp)]
    [(pat:integrated name predicate description role) #t]
    [(pat:fixup stx bind varname scname argu sep role parser*) #t]
    [(pat:and/fixup stx ps) (andmap wf-A/S/H? ps)]
    [(pat:simple iattrs simple) #t] ;; Doesn't check wf-simple.
    [(pat:seq-end) #f] ;; Should only occur in ListPattern!
    [_ #f]))

(define (wf-L? x)
  (match x
    [(pat:datum '()) #t]
    [(pat:seq-end) #t]
    [(pat:action ap sp) (and (wf-A? ap) (wf-L? sp))]
    [(pat:head headp tailp) (and (wf-H? headp) (wf-L? tailp))]
    [(pat:dots heads tailp) (and (andmap wf-EH? heads) (wf-L? tailp))]
    [(pat:pair headp tailp) (and (wf-S? headp) (wf-L? tailp))]
    [_ #f]))

(define (wf-A? x)
  (match x
    [(action:cut) #t]
    [(action:fail cnd msg) #t]
    [(action:bind attr expr) #t]
    [(action:and ps) (andmap wf-A? ps)]
    [(action:parse sp expr) (wf-S? sp)]
    [(action:do stmts) #t]
    [(action:undo stmts) #t]
    [(action:ord sp group index) (wf-A? sp)]
    [(action:post sp) (wf-A? sp)]
    [_ #f]))

(define (wf-H? x)
  (match x
    [(hpat:single sp) (wf-S? sp)]
    [(hpat:var/p name parser argu nested-attrs role scopts) #t]
    [(hpat:seq sp) (wf-L? sp)]
    [(hpat:action ap sp) (and (wf-A? ap) (wf-H? sp))]
    [(hpat:andu ps) (andmap wf-A/H? ps)]
    [(hpat:and hp sp) (and (wf-H? hp) (wf-S? sp))]
    [(hpat:or attrs ps attrss) (andmap wf-H? ps)]
    [(hpat:describe sp description transparent? role) (wf-H? sp)]
    [(hpat:delimit sp) (wf-H? sp)]
    [(hpat:commit sp) (wf-H? sp)]
    [(hpat:reflect obj argu attr-decls name nested-attrs) #t]
    [(hpat:ord sp group index) (wf-H? sp)]
    [(hpat:post sp) (wf-H? sp)]
    [(hpat:peek sp) (wf-H? sp)]
    [(hpat:peek-not sp) (wf-H? sp)]
    [_ #f]))

(define (wf-EH? x)
  (match x
    [(ehpat _ hp _ _) (wf-H? hp)]
    [_ #f]))

(define (wf-A/S? p)
  (cond [(action-pattern? p) (wf-A? p)]
        [(single-pattern? p) (wf-S? p)]
        [else #f]))

(define (wf-A/H? p)
  (cond [(action-pattern? p) (wf-A? p)]
        [(head-pattern? p) (wf-H? p)]
        [else #f]))

(define (wf-A/S/H? p)
  (cond [(action-pattern? p) (wf-A? p)]
        [(single-pattern? p) (wf-S? p)]
        [(head-pattern? p) (wf-H? p)]
        [else #f]))

;; ============================================================

;; pattern-transform : *Pattern (*Pattern -> *Pattern) -> *Pattern
(define (pattern-transform p for-pattern [root? #t])
  (define (for-node x) (if (*pattern? x) (for-pattern x) x))
  (tree-transform p for-node root?))

;; pattern-transform-preorder : *Pattern (*Pattern (X -> X) -> *Pattern) -> *Pattern
(define (pattern-transform-preorder p for-pattern [root? #t])
  (define (for-node x recur) (if (*pattern? x) (for-pattern x recur) (recur)))
  (tree-transform-preorder p for-node root?))

;; pattern-reduce{,-left} : *Pattern (*Pattern -> X) (X ... -> X) -> X
(define (pattern-reduce p for-pattern reduce [root? #t])
  (define (for-node x recur) (if (*pattern? x) (for-pattern x recur) (recur)))
  (tree-reduce p for-node reduce root?))
(define (pattern-reduce-left p for-pattern reduce [root? #t])
  (define (for-node x recur) (if (*pattern? x) (for-pattern x recur) (recur)))
  (tree-reduce-left p for-node reduce root?))

;; pattern-ormap : *Pattern (*Pattern -> X/#f) -> X/#f
(define (pattern-ormap p for-pattern [root? #t])
  (define (for-node x recur) (if (*pattern? x) (for-pattern x recur) (recur)))
  (tree-ormap p for-node root?))

;; ============================================================

(define pattern? single-pattern?)

(define (coerce-head-pattern p)
  (if (head-pattern? p) p (hpat:single p)))

(define (head-pattern-not-single? hp)
  (and (head-pattern? hp) (not (hpat:single? hp))))

;; check-pattern : *Pattern -> *Pattern
;; Does attr computation to catch errors, but returns same pattern.
(define (check-pattern p)
  (void (pattern-attrs p))
  p)

;; pattern-attrs-table : Hasheq[*Pattern => (Listof IAttr)]
(define pattern-attrs-table (make-weak-hasheq))
(define pattern-attrs*-table (make-weak-hasheq))

;; pattern-attrs : *Pattern -> (Listof IAttr)
(define (pattern-attrs p [do-has-attr? #f])
  (define (for-pattern p recur)
    (define table (if do-has-attr? pattern-attrs*-table pattern-attrs-table))
    (hash-ref! table p (lambda () (for-pattern* p recur))))
  (define (for-pattern* p recur)
    (match p
      ;; -- S patterns
      [(pat:svar name)
       (list (attr name 0 #t))]
      [(pat:var/p name _ _ nested-attrs _ _)
       (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
      [(pat:reflect _ _ _ name nested-attrs)
       (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
      [(pat:or iattrs ps _)
       iattrs]
      [(pat:not _)
       null]
      [(pat:integrated name _ _ _)
       (if name (list (attr name 0 #t)) null)]
      [(pat:fixup _ bind _ _ _ _ _ _)
       (if bind (list (attr bind 0 #t)) null)]
      [(pat:simple iattrs _) iattrs]
      ;; -- A patterns
      [(action:bind attr expr)
       (list attr)]
      [(action:do _)
       (if do-has-attr? (list #f) null)]
      ;; -- H patterns
      [(hpat:var/p name _ _ nested-attrs _ _)
       (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
      [(hpat:reflect _ _ _ name nested-attrs)
       (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
      [(hpat:or iattrs ps _)
       iattrs]
      [(hpat:peek-not _)
       null]
      ;; EH patterns
      [(ehpat iattrs _ _ _)
       iattrs]
      [_ (recur)]))
  (pattern-reduce p for-pattern (lambda iattrss (append-iattrs iattrss))))

;; ------------------------------------------------------------

;; pattern-has-cut? : *Pattern -> Boolean
;; Returns #t if p *might* cut (~!, not within ~delimit-cut).
(define (pattern-has-cut? p)
  (define (for-pattern p recur)
    (match p
      [(pat:var/p _ _ _ _ _ opts) (not (scopts-delimit-cut? opts))]
      [(pat:not _) #f]
      [(pat:delimit _) #f]
      [(pat:commit _) #f]
      [(pat:fixup _ _ _ _ _ _ _ _) #t]
      [(action:cut) #t]
      [(hpat:var/p _ _ _ _ _ opts) (not (scopts-delimit-cut? opts))]
      [(hpat:delimit _) #f]
      [(hpat:commit _) #f]
      [_ (recur)]))
  (pattern-reduce p for-pattern (lambda xs (ormap values xs))))

;; ============================================================

(define (create-pat:or ps)
  (define attrss (map pattern-attrs ps))
  (pat:or (union-iattrs attrss) ps attrss))

(define (create-hpat:or ps)
  (define attrss (map pattern-attrs ps))
  (hpat:or (union-iattrs attrss) ps attrss))

;; create-ehpat : HeadPattern RepConstraint Syntax -> EllipsisHeadPattern
(define (create-ehpat head repc head-stx)
  (let* ([iattrs0 (pattern-attrs head)]
         [iattrs (repc-adjust-attrs iattrs0 repc)])
    (define nullable (hpat-nullable head))
    (define unbounded-iterations?
      (cond [(rep:once? repc) #f]
            [(rep:optional? repc) #f]
            [(rep:bounds? repc) (eq? (rep:bounds-max repc) +inf.0)]
            [else #t]))
    (when (and (eq? nullable 'yes) unbounded-iterations?)
      (when #f (wrong-syntax head-stx "nullable ellipsis-head pattern"))
      (when #t (log-syntax-parse-error "nullable ellipsis-head pattern: ~e" head-stx)))
    (ehpat iattrs head repc (case nullable [(yes unknown) unbounded-iterations?] [(no) #f]))))

(define (repc-adjust-attrs iattrs repc)
  (cond [(rep:once? repc)
         iattrs]
        [(rep:optional? repc)
         (map attr-make-uncertain iattrs)]
        [(or (rep:bounds? repc) (eq? #f repc))
         (map increase-depth iattrs)]
        [else
         (error 'repc-adjust-attrs "INTERNAL ERROR: unexpected: ~e" repc)]))

;; ----

(define (action/head-pattern->list-pattern p)
  (cond [(action-pattern? p)
         (pat:action p (pat:any))]
        [(hpat:seq? p)
         ;; simplification: just extract list pattern from hpat:seq
         (hpat:seq-inner p)]
        [else
         (pat:head p (pat:datum '()))]))

(define (action-pattern->single-pattern a)
  (pat:action a (pat:any)))

(define (proper-list-pattern? p)
  (or (and (pat:datum? p) (eq? (pat:datum-datum p) '()))
      (and (pat:pair? p) (proper-list-pattern? (pat:pair-tail p)))
      (and (pat:head? p) (proper-list-pattern? (pat:head-tail p)))
      (and (pat:dots? p) (proper-list-pattern? (pat:dots-tail p)))
      (and (pat:action? p) (proper-list-pattern? (pat:action-inner p)))))

;; ----

(define-syntax-rule (define/memo (f x) body ...)
  (define f
    (let ([memo-table (make-weak-hasheq)])
      (lambda (x)
        (hash-ref! memo-table x (lambda () body ...))))))

;; ============================================================

;; An AbsFail is a Nat encoding the bitvector { sub? : 1, post? : 1 }
;; Finite abstraction of failuresets based on progress bins. That is:
(define AF-NONE 0)  ;; cannot fail
(define AF-SUB  1)  ;; can fail with progress < POST
(define AF-POST 2)  ;; can fail with progress >= POST
(define AF-ANY  3)  ;; can fail with progress either < or >= POST

;; AF-nz? : AbsFail -> Boolean
(define (AF-nz? af) (not (= af AF-NONE)))

;; AF<? : AbsFail AbsFail -> Boolean
;; True if every failure in af1 has strictly less progress than any failure in af2.
;; Note: trivially satisfied if either side cannot fail.
(define (AF<? af1 af2)
  ;; (0, *), (*, 0), (1, 2)
  (or (= af1 AF-NONE)
      (= af2 AF-NONE)
      (and (= af1 AF-SUB) (= af2 AF-POST))))

;; pattern-AF-table : Hasheq[*Pattern => AbsFail]
(define pattern-AF-table (make-weak-hasheq))

;; pattern-AF : *Pattern -> AbsFail
(define (pattern-AF p)
  (define (for-pattern p recur)
    (hash-ref pattern-AF-table p (lambda () (for-pattern* p recur))))
  (define (for-pattern* p recur)
    (cond [(pat:var/p? p) AF-ANY]
          [(pat:literal? p) AF-SUB]
          [(pat:datum? p) AF-SUB]
          [(pat:head? p) AF-ANY]
          [(pat:dots? p) AF-ANY]
          [(pat:not? p) AF-SUB]
          [(pat:pair? p) AF-SUB]
          [(pat:vector? p) AF-SUB]
          [(pat:box? p) AF-SUB]
          [(pat:pstruct? p) AF-SUB]
          [(pat:reflect? p) AF-ANY]
          [(pat:post? p) (if (AF-nz? (pattern-AF (pat:post-pattern p))) AF-POST AF-NONE)]
          [(pat:integrated? p) AF-SUB]
          [(action:fail? p) AF-SUB]
          [(action:parse? p) (if (AF-nz? (pattern-AF (action:parse-pattern p))) AF-SUB AF-NONE)]
          [(action:ord? p) (pattern-AF (action:ord-pattern p))]
          [(action:post? p) (if (AF-nz? (pattern-AF (action:post-pattern p))) AF-POST AF-NONE)]
          [(head-pattern? p) AF-ANY] ;; this case should not be reachable
          [else (recur)]))
  (pattern-reduce-left p for-pattern bitwise-ior))

;; pattern-cannot-fail? : *Pattern -> Boolean
(define (pattern-cannot-fail? p)
  (= (pattern-AF p) AF-NONE))

;; pattern-can-fail? : *Pattern -> Boolean
(define (pattern-can-fail? p)
  (not (pattern-cannot-fail? p)))

;; patterns-AF-sorted? : (Listof *Pattern) -> AF/#f
;; Returns AbsFail (true) if any failure from pattern N+1 has strictly
;; greater progress than any failure from patterns 0 through N.
(define (patterns-AF-sorted? ps)
  (for/fold ([af AF-NONE]) ([p (in-list ps)])
    (define afp (pattern-AF p))
    (and af (AF<? af afp) (bitwise-ior af afp))))

;; ----

;; patterns-cannot-fail? : (Listof SinglePattern) -> Boolean
;; Returns true if the disjunction of the patterns always succeeds---and thus no
;; failure-tracking needed. Note: beware cut!
(define (patterns-cannot-fail? patterns)
  (and (not (ormap pattern-has-cut? patterns))
       (ormap pattern-cannot-fail? patterns)))

;; ============================================================

;; An AbsNullable is 'yes | 'no | 'unknown (3-valued logic)

(define (3and a b)
  (case a
    [(yes) b]
    [(no) 'no]
    [(unknown) (case b [(yes unknown) 'unknown] [(no) 'no])]))

(define (3or a b)
  (case a
    [(yes) 'yes]
    [(no) b]
    [(unknown) (case b [(yes) 'yes] [(no unknown) 'unknown])]))

(define (3andmap f xs) (foldl 3and 'yes (map f xs)))
(define (3ormap f xs) (foldl 3or 'no (map f xs)))

;; lpat-nullable : ListPattern -> AbsNullable
(define/memo (lpat-nullable lp)
  (match lp
    [(pat:datum '()) 'yes]
    [(pat:action ap lp) (lpat-nullable lp)]
    [(pat:head hp lp) (3and (hpat-nullable hp) (lpat-nullable lp))]
    [(pat:pair sp lp) 'no]
    [(pat:dots ehps lp) (3and (3andmap ehpat-nullable ehps) (lpat-nullable lp))]
    ;; For hpat:and, handle the following which are not ListPatterns
    [(pat:andu lps) (3andmap lpat-nullable (filter single-pattern? lps))]
    [(pat:and lps) (3andmap lpat-nullable lps)]
    [(pat:any) #t]
    [_ 'unknown]))

;; hpat-nullable : HeadPattern -> AbsNullable
(define/memo (hpat-nullable hp)
  (match hp
    [(hpat:single sp) 'no]
    [(hpat:seq lp) (lpat-nullable lp)]
    [(hpat:action ap hp) (hpat-nullable hp)]
    [(hpat:andu ps) (3andmap hpat-nullable (filter head-pattern? ps))]
    [(hpat:and hp sp) (3and (hpat-nullable hp) (lpat-nullable sp))]
    [(hpat:or _attrs hps _attrss) (3ormap hpat-nullable hps)]
    [(hpat:describe hp _ _ _) (hpat-nullable hp)]
    [(hpat:delimit hp) (hpat-nullable hp)]
    [(hpat:commit hp) (hpat-nullable hp)]
    [(hpat:ord hp _ _) (hpat-nullable hp)]
    [(hpat:post hp) (hpat-nullable hp)]
    [_ 'unknown]))

;; ehpat-nullable : EllipsisHeadPattern -> AbsNullable
(define (ehpat-nullable ehp)
  (match ehp
    [(ehpat _ hp repc _)
     (3or (repc-nullable repc) (hpat-nullable hp))]))

;; repc-nullable : RepConstraint -> AbsNullable
(define (repc-nullable repc)
  (cond [(rep:once? repc) 'no]
        [(and (rep:bounds? repc) (> (rep:bounds-min repc) 0)) 'no]
        [else 'yes]))

;; ============================================================

;; create-post-pattern : *Pattern -> *Pattern
(define (create-post-pattern p)
  (cond [(pattern-cannot-fail? p)
         p]
        [(pattern? p)
         (pat:post p)]
        [(head-pattern? p)
         (hpat:post p)]
        [(action-pattern? p)
         (action:post p)]
        [else (error 'syntax-parse "INTERNAL ERROR: create-post-pattern ~e" p)]))

;; create-ord-pattern : *Pattern UninternedSymbol Nat -> *Pattern
(define (create-ord-pattern p group index)
  (cond [(pattern-cannot-fail? p)
         p]
        [(single-pattern? p)
         (pat:ord p group index)]
        [(head-pattern? p)
         (hpat:ord p group index)]
        [(action-pattern? p)
         (action:ord p group index)]
        [else (error 'syntax-parse "INTERNAL ERROR: create-ord-pattern ~e" p)]))

;; ord-and-patterns : (Listof *Pattern) UninternedSymbol -> (Listof *Pattern)
;; If at most one subpattern can fail, no need to wrap.  More
;; generally, if possible failures are already consistent with and
;; ordering, no need to wrap.
(define (ord-and-patterns patterns group)
  (cond [(patterns-AF-sorted? patterns) patterns]
        [else
         (for/list ([p (in-list patterns)] [index (in-naturals)])
           (create-ord-pattern p group index))]))
