#lang racket/base
(require syntax/parse/private/residual-ct ;; keep abs. path
         "rep-attrs.rkt"
         "minimatch.rkt"
         racket/syntax)
(provide (all-defined-out))

;; Pattern parsing is done (in rep.rkt) in two passes. In pass 1, stxclass refs
;; are not required to be bound, and so patterns like `x:sc` and `(~var x sc)`
;; are left as "fixup" patterns to be resolved in pass 2.

;; Uses Arguments from kws.rkt

#|
A SinglePattern is one of
  (pat:any)
  (pat:svar id)  -- "simple" var, no stxclass
  (pat:var/p Id Id Arguments (Listof IAttr) Stx scopts) -- var with parser
  (pat:literal identifier Stx Stx)
  (pat:datum datum)
  (pat:action ActionPattern SinglePattern)
  (pat:head HeadPattern SinglePattern)
  (pat:dots (listof EllipsisHeadPattern) SinglePattern)
  (pat:and (listof SinglePattern))
  (pat:or (listof IAttr) (listof SinglePattern) (listof (listof IAttr)))
  (pat:not SinglePattern)
  (pat:pair SinglePattern SinglePattern)
  (pat:vector SinglePattern)
  (pat:box SinglePattern)
  (pat:pstruct key SinglePattern)
  (pat:describe SinglePattern stx boolean stx)
  (pat:delimit SinglePattern)
  (pat:commit SinglePattern)
  (pat:reflect stx Arguments (listof SAttr) id (listof IAttr))
  (pat:ord SinglePattern UninternedSymbol Nat)
  (pat:post SinglePattern)
  (pat:integrated id/#f id string stx)
* (pat:fixup Syntax Identifier/#f Identifier Identifier Arguments String Syntax/#f Id/#f)
* (pat:and/fixup Syntax (Listof *Pattern))

A ListPattern is a subtype of SinglePattern; one of
  (pat:datum '())
  (pat:action ActionPattern ListPattern)
  (pat:head HeadPattern ListPattern)
  (pat:pair SinglePattern ListPattern)
  (pat:dots EllipsisHeadPattern ListPattern)
|#

(define-struct pat:any () #:prefab)
(define-struct pat:svar (name) #:prefab)
(define-struct pat:var/p (name parser argu nested-attrs role opts) #:prefab)
(define-struct pat:literal (id input-phase lit-phase) #:prefab)
(define-struct pat:datum (datum) #:prefab)
(define-struct pat:action (action inner) #:prefab)
(define-struct pat:head (head tail) #:prefab)
(define-struct pat:dots (heads tail) #:prefab)
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

#|
A ActionPattern is one of
  (action:cut)
  (action:fail stx stx)
  (action:bind IAttr Stx)
  (action:and (listof ActionPattern))
  (action:parse SinglePattern stx)
  (action:do (listof stx))
  (action:undo (listof stx))
  (action:ord ActionPattern UninternedSymbol Nat)
  (action:post ActionPattern)

A BindAction is (action:bind IAttr Stx)
A SideClause is just an ActionPattern
|#

(define-struct action:cut () #:prefab)
(define-struct action:fail (when message) #:prefab)
(define-struct action:bind (attr expr) #:prefab)
(define-struct action:and (patterns) #:prefab)
(define-struct action:parse (pattern expr) #:prefab)
(define-struct action:do (stmts) #:prefab)
(define-struct action:undo (stmts) #:prefab)
(define-struct action:ord (pattern group index) #:prefab)
(define-struct action:post (pattern) #:prefab)

#|
A HeadPattern is one of 
  (hpat:var/p Id Id Arguments (Listof IAttr) Stx scopts)
  (hpat:seq ListPattern)
  (hpat:action ActionPattern HeadPattern)
  (hpat:and HeadPattern SinglePattern)
  (hpat:or (listof IAttr) (listof HeadPattern) (listof (listof IAttr)))
  (hpat:describe HeadPattern stx/#f boolean stx)
  (hpat:delimit HeadPattern)
  (hpat:commit HeadPattern)
  (hpat:reflect stx Arguments (listof SAttr) id (listof IAttr))
  (hpat:ord HeadPattern UninternedSymbol Nat)
  (hpat:post HeadPattern)
  (hpat:peek HeadPattern)
  (hpat:peek-not HeadPattern)
|#

(define-struct hpat:var/p (name parser argu nested-attrs role scopts) #:prefab)
(define-struct hpat:seq (inner) #:prefab)
(define-struct hpat:action (action inner) #:prefab)
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

#|
An EllipsisHeadPattern is
  (ehpat (Listof IAttr) HeadPattern RepConstraint Boolean)

A RepConstraint is one of
  (rep:once stx stx stx)
  (rep:optional stx stx (listof BindAction))
  (rep:bounds nat posint/+inf.0 stx stx stx)
  #f
|#

(define-struct ehpat (attrs head repc check-null?) #:prefab)
(define-struct rep:once (name under-message over-message) #:prefab)
(define-struct rep:optional (name over-message defaults) #:prefab)
(define-struct rep:bounds (min max name under-message over-message) #:prefab)

(define (pattern? x)
  (or (pat:any? x)
      (pat:svar? x)
      (pat:var/p? x)
      (pat:literal? x)
      (pat:datum? x)
      (pat:action? x)
      (pat:head? x)
      (pat:dots? x)
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
      (pat:and/fixup? x)))

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
  (or (hpat:var/p? x)
      (hpat:seq? x)
      (hpat:action? x)
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

(define single-pattern? pattern?)

(define (single-or-head-pattern? x)
  (or (single-pattern? x)
      (head-pattern? x)))

;; check-pattern : *Pattern -> *Pattern
;; Does attr computation to catch errors, but returns same pattern.
(define (check-pattern p)
  (void (pattern-attrs p))
  p)

;; pattern-attrs-table : Hasheq[*Pattern => (Listof IAttr)]
(define pattern-attrs-table (make-weak-hasheq))

;; pattern-attrs : *Pattern -> (Listof IAttr)
(define (pattern-attrs p)
  (hash-ref! pattern-attrs-table p (lambda () (pattern-attrs* p))))

(define (pattern-attrs* p)
  (match p
    ;; -- S patterns
    [(pat:any)
     null]
    [(pat:svar name)
     (list (attr name 0 #t))]
    [(pat:var/p name _ _ nested-attrs _ _)
     (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
    [(pat:reflect _ _ _ name nested-attrs)
     (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
    [(pat:datum _)
     null]
    [(pat:literal _ _ _)
     null]
    [(pat:action a sp)
     (append-iattrs (map pattern-attrs (list a sp)))]
    [(pat:head headp tailp)
     (append-iattrs (map pattern-attrs (list headp tailp)))]
    [(pat:pair headp tailp)
     (append-iattrs (map pattern-attrs (list headp tailp)))]
    [(pat:vector sp)
     (pattern-attrs sp)]
    [(pat:box sp)
     (pattern-attrs sp)]
    [(pat:pstruct key sp)
     (pattern-attrs sp)]
    [(pat:describe sp _ _ _)
     (pattern-attrs sp)]
    [(pat:and ps)
     (append-iattrs (map pattern-attrs ps))]
    [(pat:or _ ps _)
     (union-iattrs (map pattern-attrs ps))]
    [(pat:not _)
     null]
    [(pat:dots headps tailp)
     (append-iattrs (map pattern-attrs (append headps (list tailp))))]
    [(pat:delimit sp)
     (pattern-attrs sp)]
    [(pat:commit sp)
     (pattern-attrs sp)]
    [(pat:ord sp _ _)
     (pattern-attrs sp)]
    [(pat:post sp)
     (pattern-attrs sp)]
    [(pat:integrated name _ _ _)
     (if name (list (attr name 0 #t)) null)]
    [(pat:fixup _ bind _ _ _ _ _ _)
     (if bind (list (attr bind 0 #t)) null)]
    [(pat:and/fixup _ ps)
     (append-iattrs (map pattern-attrs ps))]

    ;; -- A patterns
    [(action:cut)
     null]
    [(action:fail _ _)
     null]
    [(action:bind attr expr)
     (list attr)]
    [(action:and ps)
     (append-iattrs (map pattern-attrs ps))]
    [(action:parse sp _)
     (pattern-attrs sp)]
    [(action:do _)
     null]
    [(action:undo _)
     null]
    [(action:ord sp _ _)
     (pattern-attrs sp)]
    [(action:post sp)
     (pattern-attrs sp)]

    ;; -- H patterns
    [(hpat:var/p name _ _ nested-attrs _ _)
     (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
    [(hpat:reflect _ _ _ name nested-attrs)
     (if name (cons (attr name 0 #t) nested-attrs) nested-attrs)]
    [(hpat:seq lp)
     (pattern-attrs lp)]
    [(hpat:action a hp)
     (append-iattrs (map pattern-attrs (list a hp)))]
    [(hpat:describe hp _ _ _)
     (pattern-attrs hp)]
    [(hpat:and hp sp)
     (append-iattrs (map pattern-attrs (list hp sp)))]
    [(hpat:or _ ps _)
     (union-iattrs (map pattern-attrs ps))]
    [(hpat:delimit hp)
     (pattern-attrs hp)]
    [(hpat:commit hp)
     (pattern-attrs hp)]
    [(hpat:ord hp _ _)
     (pattern-attrs hp)]
    [(hpat:post hp)
     (pattern-attrs hp)]
    [(hpat:peek hp)
     (pattern-attrs hp)]
    [(hpat:peek-not hp)
     null]

    ;; EH patterns
    [(ehpat iattrs _ _ _)
     iattrs]
    ))

;; ----

;; pattern-has-cut? : *Pattern -> Boolean
;; Returns #t if p *might* cut (~!, not within ~delimit-cut).
(define (pattern-has-cut? p)
  (match p
    ;; -- S patterns
    [(pat:any) #f]
    [(pat:svar name) #f]
    [(pat:var/p _ _ _ _ _ opts) (not (scopts-delimit-cut? opts))]
    [(pat:reflect _ _ _ name nested-attrs) #f]
    [(pat:datum _) #f]
    [(pat:literal _ _ _) #f]
    [(pat:action a sp) (or (pattern-has-cut? a) (pattern-has-cut? sp))]
    [(pat:head headp tailp) (or (pattern-has-cut? headp) (pattern-has-cut? tailp))]
    [(pat:pair headp tailp) (or (pattern-has-cut? headp) (pattern-has-cut? tailp))]
    [(pat:vector sp) (pattern-has-cut? sp)]
    [(pat:box sp) (pattern-has-cut? sp)]
    [(pat:pstruct key sp) (pattern-has-cut? sp)]
    [(pat:describe sp _ _ _) (pattern-has-cut? sp)]
    [(pat:and ps) (ormap pattern-has-cut? ps)]
    [(pat:or _ ps _) (ormap pattern-has-cut? ps)]
    [(pat:not _) #f]
    [(pat:dots headps tailp) (or (ormap pattern-has-cut? headps) (pattern-has-cut? tailp))]
    [(pat:delimit sp) #f]
    [(pat:commit sp) #f]
    [(pat:ord sp _ _) (pattern-has-cut? sp)]
    [(pat:post sp) (pattern-has-cut? sp)]
    [(pat:integrated name _ _ _) #f]
    [(pat:fixup _ _ _ _ _ _ _ _) #t]
    [(pat:and/fixup _ ps) (ormap pattern-has-cut? ps)]

    ;; -- A patterns
    [(action:cut) #t]
    [(action:fail _ _) #f]
    [(action:bind attr expr) #f]
    [(action:and ps) (ormap pattern-has-cut? ps)]
    [(action:parse sp _) (pattern-has-cut? sp)]
    [(action:do _) #f]
    [(action:undo _) #f]
    [(action:ord sp _ _) (pattern-has-cut? sp)]
    [(action:post sp) (pattern-has-cut? sp)]

    ;; -- H patterns
    [(hpat:var/p _ _ _ _ _ opts) (not (scopts-delimit-cut? opts))]
    [(hpat:reflect _ _ _ name nested-attrs) #f]
    [(hpat:seq lp) (pattern-has-cut? lp)]
    [(hpat:action a hp) (or (pattern-has-cut? a) (pattern-has-cut? hp))]
    [(hpat:describe hp _ _ _) (pattern-has-cut? hp)]
    [(hpat:and hp sp) (or (pattern-has-cut? hp) (pattern-has-cut? sp))]
    [(hpat:or _ ps _) (ormap pattern-has-cut? ps)]
    [(hpat:delimit hp) #f]
    [(hpat:commit hp) #f]
    [(hpat:ord hp _ _) (pattern-has-cut? hp)]
    [(hpat:post hp) (pattern-has-cut? hp)]
    [(hpat:peek hp) (pattern-has-cut? hp)]
    [(hpat:peek-not hp) (pattern-has-cut? hp)]

    ;; EH patterns
    [(ehpat _ hp _ _) (pattern-has-cut? hp)]
    ))

;; ----

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

;; ----

;; An AbsFail is a Nat encoding the bitvector { sub? : 1, post? : 1 }
;; Finite abstraction of failuresets based on progress bins. That is:
(define AF-NONE 0)  ;; cannot fail
(define AF-SUB  1)  ;; can fail with progress < POST
(define AF-POST 2)  ;; can fail with progress >= POST
(define AF-ANY  3)  ;; can fail with progress either < or >= POST

;; AF-nz? : AbsFail -> {0, 1}
(define (AF-nz? af) (if (= af AF-NONE) 0 1))

;; AF<? : AbsFail AbsFail -> Boolean
;; True if every failure in af1 has strictly less progress than any failure in af2.
;; Note: trivially satisfied if either side cannot fail.
(define (AF<? af1 af2)
  ;; (0, *), (*, 0), (1, 2)
  (or (= af1 AF-NONE)
      (= af2 AF-NONE)
      (and (= af1 AF-SUB) (= af2 AF-POST))))

;; pattern-absfail : *Pattern -> AbsFail
(define/memo (pattern-AF p)
  (define (patterns-AF ps)
    (for/fold ([af 0]) ([p (in-list ps)]) (bitwise-ior af (pattern-AF p))))
  (cond [(pat:any? p) AF-NONE]
        [(pat:svar? p) AF-NONE]
        [(pat:var/p? p) AF-ANY]
        [(pat:literal? p) AF-SUB]
        [(pat:datum? p) AF-SUB]
        [(pat:action? p) (bitwise-ior (pattern-AF (pat:action-action p))
                                      (pattern-AF (pat:action-inner p)))]
        [(pat:head? p) AF-ANY]
        [(pat:dots? p) AF-ANY]
        [(pat:and? p) (patterns-AF (pat:and-patterns p))]
        [(pat:or? p) (patterns-AF (pat:or-patterns p))]
        [(pat:not? p) AF-SUB]
        [(pat:pair? p) AF-SUB]
        [(pat:vector? p) AF-SUB]
        [(pat:box? p) AF-SUB]
        [(pat:pstruct? p) AF-SUB]
        [(pat:describe? p) (pattern-AF (pat:describe-pattern p))]
        [(pat:delimit? p) (pattern-AF (pat:delimit-pattern p))]
        [(pat:commit? p) (pattern-AF (pat:commit-pattern p))]
        [(pat:reflect? p) AF-ANY]
        [(pat:ord? p) (pattern-AF (pat:ord-pattern p))]
        [(pat:post? p) (if (AF-nz? (pattern-AF (pat:post-pattern p))) AF-POST AF-NONE)]
        [(pat:integrated? p) AF-SUB]
        ;; Action patterns
        [(action:cut? p) AF-NONE]
        [(action:fail? p) AF-SUB]
        [(action:bind? p) AF-NONE]
        [(action:and? p) (patterns-AF (action:and-patterns p))]
        [(action:parse? p) (if (AF-nz? (pattern-AF (action:parse-pattern p))) AF-SUB AF-NONE)]
        [(action:do? p) AF-NONE]
        [(action:undo? p) AF-SUB]
        [(action:ord? p) (pattern-AF (action:ord-pattern p))]
        [(action:post? p) (if (AF-nz? (pattern-AF (action:post-pattern p))) AF-POST AF-NONE)]
        ;; Head patterns, eh patterns, etc
        [else AF-ANY]))

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

;; ----

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
    [(pat:and lps) (3andmap lpat-nullable lps)]
    [(pat:any) #t]
    [_ 'unknown]))

;; hpat-nullable : HeadPattern -> AbsNullable
(define/memo (hpat-nullable hp)
  (match hp
    [(hpat:seq lp) (lpat-nullable lp)]
    [(hpat:action ap hp) (hpat-nullable hp)]
    [(hpat:and hp sp) (3and (hpat-nullable hp) (lpat-nullable sp))]
    [(hpat:or _attrs hps _attrss) (3ormap hpat-nullable hps)]
    [(hpat:describe hp _ _ _) (hpat-nullable hp)]
    [(hpat:delimit hp) (hpat-nullable hp)]
    [(hpat:commit hp) (hpat-nullable hp)]
    [(hpat:ord hp _ _) (hpat-nullable hp)]
    [(hpat:post hp) (hpat-nullable hp)]
    [(? pattern? hp) 'no]
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

;; ----

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
        [(pattern? p)
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

;; create-action:and : (Listof ActionPattern) -> ActionPattern
(define (create-action:and actions)
  (match actions
    [(list action) action]
    [_ (action:and actions)]))
