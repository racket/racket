#lang racket/base
(require racket/contract
         racket/match
         racket/dict
         unstable/struct
         syntax/modresolve
         syntax/stx
         syntax/id-table
         macro-debugger/model/deriv
         "util.rkt")
(provide/contract
 [check-requires (-> module-path? list?)]
 [add-disappeared-uses? (parameter/c boolean?)]
 [mpi->key (-> module-path-index? any/c)])

#|

The purpose of this script is to estimate a module's useless requires.

Usage:

  (check-requires <module-name>)

Examples:

  (check-requires 'typed-scheme)
  (check-requires 'unstable/markparam)
  (check-requires 'macro-debugger/syntax-browser/widget)

The procedure prints one line per (non-label) require in the following
format:

  KEEP <module> at <phase> <optional-comment>
    - The require must be kept because bindings defined within it are used.
    - The optional comment indicates if the require must be kept
      - only because its bindings are re-exported
      - only because the whitelist DB says so

  BYPASS <module> at <phase>
    - The require is used, but only for bindings that could be more directly
      obtained via another module. For example, 'racket' can be bypassed in favor
      of some subset of 'racket/base', 'racket/contract', etc.

  DROP <module> at <phase>
    - The require appears to be unused. Unless it must be kept for side
      effects or for bindings of a very unusual macro, it can be dropped
      entirely.

Notes:

  BYPASS recommendations should often be disregarded, because the
  required module is expressly intended as an aggregation module and the
  only way to bypass it would be to require private modules
  directly. See TODO for plans to improve BYPASS recommendations.

  Ignore recommendations to DROP or BYPASS modules with side
  effects. Read the section below (How it works) and also see the docs
  for 'module-db' for whitelisting side-effectful modules.

  The script is not intelligent about the language, which causes
  certain spurious recommendations to appear frequently. For example,

    DROP scheme/mzscheme at 1

  means that the module's language is mzscheme, which automatically
  inserts (require-for-syntax scheme/mzscheme). It's infeasible to
  remove it except by rewriting the module in scheme/base or
  racket/base.

========

How it works

Determining whether a require is actually useless is impossible: a
module may be required for compile-time side effect only, and there's
no way to monitor that.

Here are some approximations that are feasible to calculate:

NOM-USES = A require R is "used" by a module M if, during the
compilation of M, a reference is resolved to a binding exported by R.

DEF-USES = A require R is "used" by a module M if, during the
compilation of M, a reference is resolved to a binding defined in R.

The limitations:
 - misses side-effects
 - misses identifiers recognized via 'free-identifier=?'
   (But those should be recorded as 'disappeared-use anyway.)

|#

;; ========

(define add-disappeared-uses? (make-parameter #t))

;; ========

;; A RefTable = hash[Phase => free-id-table[bool]]
;; Phase = nat

#|

For the calculations at the end, we only want to consider identifiers
from the expanded module (ie, syntax-source-module = here.)

That means that instead of a free-id-table, we really want a dict/set
that distinguishes between identifiers imported different ways.  eg,
hash keyed on (nom-name, nom-mod). The reason is that a not-from-here
identifier can block/clobber a from-here identifier if they happen to
refer to the same binding. That messes up the analysis.

Temporary solution: only add from-here identifiers to the reftable.

|#

;; new-reftable : -> RefTable
(define (new-reftable)
  (make-hash))

;; reftable-get-phase : RefTable Phase -> free-id-table[bool]
(define (reftable-get-phase refs phase)
  (hash-ref! refs phase (lambda () (make-free-id-table #:phase phase))))

;; reftable-add-all! : RefTable Phase (listof identifier) -> void
(define (reftable-add-all! refs phase ids)
  (let ([id-table (reftable-get-phase refs phase)])
    (for ([id (in-list ids)]
          #:when (here-mpi? (syntax-source-module id)))
      (free-id-table-set! id-table id #t))))

;; reftable-add! : RefTable Phase identifier -> void
(define (reftable-add! refs phase id)
  (reftable-add-all! refs phase (list id)))

;; ========

;; phase : (parameterof nat)
(define phase (make-parameter 0))

;; ========

;; analyze : *Deriv* RefTable -> void
;; *Deriv* = Deriv | LDeriv | BRule | ModRule | ... (anything from deriv.rkt)
(define (analyze deriv refs)
  (define (recur . args)
    (let check ([arg args])
      (cond [(syntax? arg) (error 'whoops "arg = ~s" arg)]
            [(list? arg) (for-each check arg)]
            [else (void)]))
    (for ([arg (in-list args)])
      (if (list? arg)
          (apply recur arg)
          (analyze arg refs))))
  (define (add! ids)
    (reftable-add-all! refs (phase) ids))

  ;; (printf "analyze ~.s\n" deriv)

  ;; Handle common base (ie, resolves) part of derivs, if applicable
  (match deriv
    [(base z1 z2 resolves ?1)
     (add! resolves)
     (when (and (syntax? z2) (add-disappeared-uses?))
       (let ([uses (syntax-property z2 'disappeared-use)])
         (add! (let loop ([x uses] [onto null])
                 (cond [(identifier? x) (cons x onto)]
                       [(pair? x) (loop (car x) (loop (cdr x) onto))]
                       [else onto])))))]
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
     (parameterize ((phase (+ (phase) (if for-stx? 1 0))))
       (recur inner))]
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
    [(local-value name ?1 resolves bound?)
     (when (and bound? resolves)
       (add! (cons name resolves)))]
    [(track-origin before after)
     (void)]
    [(local-remark contents)
     (void)]

    [(p:variable z1 z2 rs ?1)
     (void)]
    [(p:module z1 z2 rs ?1 locals tag rename check tag2 ?3 body shift)
     (recur locals check body)]
    [(p:#%module-begin z1 z2 rs ?1 me pass1 pass2 ?2)
     (recur pass1 pass2)]
    [(p:define-syntaxes z1 z2 rs ?1 rhs locals)
     (parameterize ((phase (+ (phase) 1)))
       (recur rhs locals))]
    [(p:define-values z1 z2 rs ?1 rhs)
     (recur rhs)]

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
    [(p:letrec-syntaxes+values _ _ _ _ srenames sbindrhss vrenames vrhss body tag)
     (recur sbindrhss vrhss body)]

    [(p:provide _ _ _ _ inners ?2)
     (recur inners)]

    [(p:require _ _ _ _ locals)
     (recur locals)]

    [(p:#%stratified-body _ _ _ _ bderiv)
     (recur bderiv)]

    [(p:stop _ _ _ _) (void)]
    [(p:unknown _ _ _ _) (void)]
    [(p:#%top _ _ _ _)
     (void)]
    [(p:#%datum _ _ _ _) (void)]
    [(p:quote _ _ _ _) (void)]
    [(p:quote-syntax z1 z2 _ _)
     (when z2 (analyze/quote-syntax z2 refs))]
    [(p:#%variable-reference _ _ _ _)
     ;; FIXME
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
    [(b:defstx _ head ?1 rename ?2 bindrhs)
     (recur head bindrhs)]

    [(bind-syntaxes rhs locals)
     (parameterize ((phase (+ 1 (phase))))
       (recur rhs locals))]

    [(clc ?1 renames body)
     (recur body)]

    [(mod:prim head rename prim)
     (recur head prim)]
    [(mod:splice head rename ?1 tail)
     (recur head)]
    [(mod:lift head renames tail)
     (recur head)]
    [(mod:lift-end tail)
     (void)]
    [(mod:cons head)
     (recur head)]
    [(mod:skip)
     (void)]

    [(ecte _ _ locals first second locals2)
     (recur locals first second locals2)]

    [#f
     (void)]))

;; analyze/quote-syntax : stx RefTable -> void
;; Current approach: estimate that an identifier in a syntax template
;; may be used at (sub1 (phase)) or (phase).
;; FIXME: Allow for more conservative choices, too.
;; FIXME: #%top, #%app, #%datum, etc?
;; FIXME: Track tentative (in quote-syntax) references separately?
(define (analyze/quote-syntax qs-stx refs)
  (let ([phases (list (phase) (sub1 (phase)))]
        [stx (syntax-case qs-stx ()
               [(_quote-syntax x) #'x])])
    (define (add! id)
      (for ([phase (in-list phases)])
        (reftable-add! refs phase id)))
    (let loop ([stx stx])
      (let ([d (if (syntax? stx) (syntax-e stx) stx)])
        (cond [(identifier? stx) (add! stx)]
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

;; ========

;; sMPI = S-expr form of mpi (see mpi->key)
;; Using MPIs doesn't work. I conjecture that the final module shift means that
;; all during-expansion MPIs are different from all compiled-expr MPIs.

;; A UsedTable = hash[(list int sMPI) => list]

;; calculate-used-approximations : RefTable -> (values UsedTable UsedTable)
(define (calculate-used-approximations refs)
  (let ([NOM-USES (make-hash)]
        [DEF-USES (make-hash)])
    (for* ([(use-phase id-table) (in-hash refs)]
           [id (in-dict-keys id-table)])
      ;; Only look at identifiers written in module being examined.
      ;; (Otherwise, nom-mod & nom-phase aren't enough info (???)
      (when (here-mpi? (syntax-source-module id)) ;; REDUNDANT
        (let ([b (identifier-binding id use-phase)])
          (match b
            [(list def-mod def-sym nom-mod nom-sym
                   def-phase nom-imp-phase nom-exp-phase)
             ;; use-phase = def-phase + required-phase
             ;; thus required-phase = use-phase - def-phase
             (let* ([required-phase (- use-phase def-phase)]
                    [key (list required-phase (mpi->key def-mod))])
               (hash-set! DEF-USES key
                          (cons id (hash-ref DEF-USES key null))))
             ;; use-phase = nom-imp-phase + nom-exp-phase ?????
             ;; We just care about nom-imp-phase, since importing into *here*
             #|
             ;; FIXME: This check goes wrong on defined-for-syntax ids
             (unless (equal? use-phase (+ nom-imp-phase nom-exp-phase))
               (error 'calculate
                      "internal error: phases wrong in ~s @ ~s, binding = ~s"
                      id use-phase b))
             |#
             (let ([key (list nom-imp-phase (mpi->key nom-mod))])
               (hash-set! NOM-USES key
                          (cons id (hash-ref NOM-USES key null))))]
            [_
             (void)]))))
    (values NOM-USES DEF-USES)))

;; ========

;; get-requires : compiled-module-expr -> (listof (list int MPI))
(define (get-requires compiled)
  (let ([phase+mods-list (module-compiled-imports compiled)])
    (for*/list ([phase+mods (in-list phase+mods-list)]
                #:when (car phase+mods) ;; Skip for-label requires
                [mod (cdr phase+mods)])
      (list (car phase+mods) mod))))

;; add-provides! : compiled-module-expr UsedTable UsedTable -> void
(define (add-provides! compiled NOM-USES DEF-USES)
  (define (add! mpi phase)
    (let ([key (list phase (mpi->key mpi))])
      (hash-set! NOM-USES key (cons 'export (hash-ref NOM-USES key null)))
      (hash-set! DEF-USES key (cons 'export (hash-ref DEF-USES key null)))))
  (let-values ([(vprov sprov) (module-compiled-exports compiled)])
    (for* ([phase+exps (in-list (append vprov sprov))]
           #:when (car phase+exps) ;; Skip for-label provides
           [name+srcs (in-list (cdr phase+exps))]
           [src (in-list (cadr name+srcs))])
      (let ([name (car name+srcs)])
        (match src
          [(? module-path-index?)
           (add! src 0)]
          [(list imp-mod imp-phase-shift imp-name imp-phase-???)
           (add! imp-mod imp-phase-shift)])))))

;; ========

;; A ModuleDB = hash[path/symbol => (U 'no-drop 'no-bypass)]
;;   'no-drop = must not be dropped or bypassed because of, eg, side effects
;;   'no-bypass = don't bypass in favor of private component modules
;;                but if the module is unused, can drop it
;;                (FIXME: replace with component module calculation and checking)

(define (make-module-db mod+config-list)
  (for/hash ([mod+config (in-list mod+config-list)])
    (values (resolve-module-path (car mod+config) #f) (cadr mod+config))))

;; module-db : ModuleDB
(define module-db
  (make-module-db
   '([racket/base                            no-bypass]
     [racket/contract/base                   no-bypass]
     [racket/gui                             no-bypass]
     [racket/match                           no-bypass]
     ['#%builtin                             no-drop]

     [typed-scheme/private/base-env          no-drop]
     [typed-scheme/private/base-special-env  no-drop]
     [typed-scheme/private/base-env-numeric  no-drop]
     [typed-scheme/private/base-env-indexing no-drop])))

;; ========

#|
A recommendation is one of
  (list 'keep   module-path-index phase string/#f)
  (list 'bypass module-path-index phase)
  (list 'drop   module-path-index phase)
|#

;; check-requires : module-path -> (listof recommendation)
(define (check-requires mod-path)
  (let-values ([(compiled deriv) (get-module-code/trace mod-path)])
    (let ([refs (new-reftable)])
      (analyze deriv refs)
      (let-values ([(NOM-USES DEF-USES) (calculate-used-approximations refs)])
        (add-provides! compiled NOM-USES DEF-USES)
        (report NOM-USES DEF-USES (get-requires compiled))))))

;; report : UseTable UseTable (listof (list int mpi)) -> (listof recommendation)
(define (report NOM-USES DEF-USES phase+mod-list)
  (for/list ([phase+mod (in-list phase+mod-list)])
    (let* ([key (list (car phase+mod) (mpi->key (cadr phase+mod)))]
           [db-config
            (hash-ref module-db
                      (resolved-module-path-name
                       (module-path-index-resolve (cadr phase+mod)))
                      #f)]
           [nom-ids (hash-ref NOM-USES key null)]
           [def-ids (hash-ref DEF-USES key null)]
           [phase (car phase+mod)]
           [mod (cadr phase+mod)]
           [name (format "~s at ~s" (mpi->key mod) phase)])
      (cond [(and (pair? nom-ids) (pair? def-ids))
             (list 'keep mod phase (if (ormap identifier? nom-ids) #f "for exports"))]
            [(pair? nom-ids)
             (if (memq db-config '(no-bypass no-drop))
                 (list 'keep mod phase "db says no-bypass")
                 (list 'bypass mod phase))]
            [else
             (if (memq db-config '(no-drop))
                 (list 'keep mod phase "db says no-drop")
                 (list 'drop mod phase))]))))

(define (mpi->key x)
  (let ([l (mpi->list x)])
    (if (and (pair? l) (null? (cdr l)))
        (car l)
        l)))

(define (mpi->list x)
  (cond [(module-path-index? x)
         (let-values ([(rel base) (module-path-index-split x)])
           (if rel
               (cons rel (mpi->list base))
               null))]
        [(eq? x #f)
         null]
        [else
         (list x)]))

#|
TODO
====

Elaborate on BYPASS recommendations by finding the necessary modules
further up the require chain to require directly.
  - don't recommend private modules, though... heuristic

Let user provide database of modules that should never be dropped, eg
because they have side effects.
  - wouldn't it be awesome if this db could be a datalog program?
  - start simpler, though

Verbose mode should show identifiers used by a module (for KEEP).
For example, if only one used, good candidate to split out, if possible.

Ambitious mode could analyze module and recommend ways to split module
into independent submodules.

More options for quote-syntax handling & explain current heuristic better.

Handle for-label.
|#
