#lang racket/base
(require racket/contract
         racket/match
         racket/dict
         unstable/struct
         syntax/modresolve
         syntax/stx
         syntax/id-table
         macro-debugger/model/deriv
         "private/reftable.rkt"
         "private/nom-use-alg.rkt"
         "private/util.rkt")
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
      (nom-use-alg refs compiled))))

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
