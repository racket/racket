#lang racket/base
(require "../common/set.rkt"
         "../compile/serialize-property.rkt"
         "../compile/serialize-state.rkt"
         "../common/memo.rkt"
         "syntax.rkt"
         "property.rkt"
         "scope.rkt"
         "../common/phase.rkt"
         "full-binding.rkt"
         "module-binding.rkt"
         "local-binding.rkt"
         "datum-map.rkt"
         "../expand/rename-trans.rkt"
         "../common/module-path.rkt"
         "cache.rkt")

(provide
 binding-frame-id
 binding-free=id
 (all-from-out "module-binding.rkt")
 (all-from-out "local-binding.rkt")

 free-identifier=?
 same-binding?
 same-binding-nominals?
 identifier-binding
 identifier-binding-symbol
 
 maybe-install-free=id!
 binding-set-free=id

 resolve+shift
 syntax-module-path-index-shift

 apply-syntax-shifts
 syntax-apply-shifts
 binding-module-path-index-shift
 syntax-transfer-shifts
 syntax-add-shifts
 
 syntax-source-module
 identifier-prune-to-source-module)

;; ----------------------------------------

(define (free-identifier=? a b a-phase b-phase)
  (define ab (toplevel-as-symbol (resolve+shift a a-phase #:unbound-sym? #t)))
  (define bb (toplevel-as-symbol (resolve+shift b b-phase #:unbound-sym? #t)))
  (cond
   [(or (symbol? ab) (symbol? bb))
    (eq? ab bb)]
   [else
    (same-binding? ab bb)]))

;; By tradition, equate "unbound" with "bound at the top level"
(define (toplevel-as-symbol b)
  (if (and (module-binding? b)
           (top-level-module-path-index? (module-binding-module b)))
      (module-binding-sym b)
      b))

(define (same-binding? ab bb)
  (cond
   [(module-binding? ab)
    (and (module-binding? bb)
         (eq? (module-binding-sym ab)
              (module-binding-sym bb))
         (eqv? (module-binding-phase ab)
               (module-binding-phase bb))
         (eq? (module-path-index-resolve (module-binding-module ab))
              (module-path-index-resolve (module-binding-module bb))))]
   [(local-binding? ab)
    (and (local-binding? bb)
         (eq? (local-binding-key ab)
              (local-binding-key bb)))]
   [else (error "bad binding" ab)]))

;; Check whether two bindings that are `same-binding?` also provide
;; the same nominal info (i.e., claim to be required through the same
;; immediate path):
(define (same-binding-nominals? ab bb)
  (and (eq? (module-path-index-resolve (module-binding-nominal-module ab))
            (module-path-index-resolve (module-binding-nominal-module bb)))
       (eqv? (module-binding-nominal-require-phase ab)
             (module-binding-nominal-require-phase bb))
       (eqv? (module-binding-nominal-sym ab)
             (module-binding-nominal-sym bb))))

(define (identifier-binding-symbol id phase)
  (define b (resolve+shift id phase #:unbound-sym? #t))
  (cond
   [(symbol? b) b]
   [(module-binding? b)
    (module-binding-sym b)]
   [(local-binding? b)
    (local-binding-key b)]
   [else (syntax-e id)]))

(define (identifier-binding id phase [top-level-symbol? #f])
  (define b (resolve+shift id phase))
  (cond
   [(module-binding? b)
    (if (top-level-module-path-index? (module-binding-module b))
        (if top-level-symbol?
            (list (module-binding-nominal-sym b))
            #f)
        (list (module-binding-module b)
              (module-binding-sym b)
              (module-binding-nominal-module b)
              (module-binding-nominal-sym b)
              (module-binding-phase b)
              (module-binding-nominal-require-phase b)
              (module-binding-nominal-phase b)))]
   [(local-binding? b)
    'lexical]
   [else #f]))

;; ----------------------------------------

(define (maybe-install-free=id! val id phase)
  (when (rename-transformer? val)
    (define free=id (rename-transformer-target val))
    (unless (syntax-property free=id 'not-free-identifier=?)
      (define b (resolve+shift id phase #:exactly? #t #:immediate? #t))
      (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) (binding-set-free=id b free=id)))))

;; Helper to add a `free-identifier=?` equivance to a binding
(define (binding-set-free=id b free=id)
  (cond
   [(module-binding? b) (module-binding-update b #:free=id free=id)]
   [(local-binding? b) (local-binding-update b #:free=id free=id)]
   [else (error "bad binding for free=id:" b)]))

; ----------------------------------------

;; To tag shifts that should not count as a module source
;; in the sense of `syntax-source-module`:
(struct non-source-shift (from to) #:prefab)
(define (shift-from s)
  (if (pair? s) (car s) (non-source-shift-from s)))
(define (shift-to s)
  (if (pair? s) (cdr s) (non-source-shift-to s)))

;; Adjust `s` (recursively) so that if `resolve+shift` would
;; report `form-mpi`, the same operation on the result will
;; report `to-mpi`. A non-#f `inspector` is provided when shifting
;; syntax literals in a module to match the module's declaration-time
;; inspector.
(define (syntax-module-path-index-shift s from-mpi to-mpi [inspector #f]
                                        #:non-source? [non-source? #f])
  (cond
   [(eq? from-mpi to-mpi)
    (if inspector
        (syntax-set-inspector s inspector)
        s)]
   [else
    (define shift (if non-source?
                      (non-source-shift from-mpi to-mpi)
                      (cons from-mpi to-mpi)))
    (struct-copy syntax s
                 [mpi-shifts (shift-cons shift (syntax-mpi-shifts s))]
                 [inspector (or (syntax-inspector s)
                                inspector)]
                 [scope-propagations+tamper (if (datum-has-elements? (syntax-content s))
                                                (propagation-mpi-shift (syntax-scope-propagations+tamper s)
                                                                       (lambda (s) (shift-cons shift s))
                                                                       inspector
                                                                       (syntax-scopes s)
                                                                       (syntax-shifted-multi-scopes s)
                                                                       (syntax-mpi-shifts s))
                                                (syntax-scope-propagations+tamper s))])]))

(define (shift-cons shift shifts)
  (cond
    [(and (pair? shifts)
          (eq? (shift-from shift) (shift-from (car shifts))))
     ;; Adding `shift` is not useful
     shifts]
    [else (cons shift shifts)]))

;; Use `resolve+shift` instead of `resolve` when the module of a
;; module binding is relevant or when `free-identifier=?` equivalences
;; (as installed by a binding to a rename transfomer) are relevant;
;; module path index shifts attached to `s` are taken into account in
;; the result
(define (resolve+shift s phase
                       #:ambiguous-value [ambiguous-value #f]
                       #:exactly? [exactly? #f]
                       #:immediate? [immediate? exactly?]
                       #:unbound-sym? [unbound-sym? #f]
                       ;; For resolving bulk bindings in `free-identifier=?` chains:
                       #:extra-shifts [extra-shifts null])
  (define can-cache? (and (not exactly?) (not immediate?) (null? extra-shifts)))
  (cond
    [(and can-cache?
          (resolve+shift-cache-get s phase))
     => (lambda (b)
          (if (eq? b '#:none)
              (and unbound-sym? (syntax-content s))
              b))]
    [else
     (define immediate-b (resolve s phase
                                  #:ambiguous-value ambiguous-value
                                  #:exactly? exactly?
                                  #:extra-shifts extra-shifts))
     (define b (if (and immediate-b
                        (not immediate?)
                        (binding-free=id immediate-b))
                   (resolve+shift (binding-free=id immediate-b) phase
                                  #:extra-shifts (append extra-shifts (syntax-mpi-shifts s))
                                  #:ambiguous-value ambiguous-value
                                  #:exactly? exactly?
                                  #:unbound-sym? unbound-sym?)
                   immediate-b))
     (cond
       [(module-binding? b)
        (define mpi-shifts (syntax-mpi-shifts s))
        (cond
          [(null? mpi-shifts)
           b]
          [else
           (define mod (module-binding-module b))
           (define shifted-mod (apply-syntax-shifts mod mpi-shifts))
           (define nominal-mod (module-binding-nominal-module b))
           (define shifted-nominal-mod (if (eq? mod nominal-mod)
                                           shifted-mod
                                           (apply-syntax-shifts nominal-mod mpi-shifts)))
           (define result-b
             (if (and (eq? mod shifted-mod)
                      (eq? nominal-mod shifted-nominal-mod)
                      (not (binding-free=id b))
                      (null? (module-binding-extra-nominal-bindings b)))
                 b
                 (module-binding-update b
                                        #:module shifted-mod
                                        #:nominal-module shifted-nominal-mod
                                        #:free=id (and (binding-free=id b)
                                                       (syntax-transfer-shifts (binding-free=id b) s))
                                        #:extra-nominal-bindings
                                        (for/list ([b (in-list (module-binding-extra-nominal-bindings b))])
                                          (apply-syntax-shifts-to-binding b mpi-shifts)))))
           (when can-cache?
             (resolve+shift-cache-set! s phase result-b))
           result-b])]
       [else
        (when can-cache?
          (resolve+shift-cache-set! s phase (or b '#:none)))
        (or b
            (and unbound-sym?
                 (syntax-content s)))])]))

;; Apply accumulated module path index shifts
(define (apply-syntax-shifts mpi shifts)
  (cond
   [(null? shifts) mpi]
   [else
    (define shifted-mpi (apply-syntax-shifts mpi (cdr shifts)))
    (define shift (car shifts))
    (module-path-index-shift shifted-mpi (shift-from shift) (shift-to shift))]))

;; Apply accumulated module path index shifts to a module binding
(define (apply-syntax-shifts-to-binding b shifts)
  (cond
   [(null? shifts) b]
   [else
    (define shifted-b (apply-syntax-shifts-to-binding b (cdr shifts)))
    (define shift (car shifts))
    (binding-module-path-index-shift shifted-b (shift-from shift) (shift-to shift))]))
        

;; Apply a syntax object's shifts to a given module path index
(define (syntax-apply-shifts s mpi)
  (apply-syntax-shifts mpi (syntax-mpi-shifts s)))

;; Apply a single shift to a single binding
(define (binding-module-path-index-shift b from-mpi to-mpi)
  (cond
   [(module-binding? b)
    (module-binding-update b
                           #:module (module-path-index-shift (module-binding-module b)
                                                             from-mpi
                                                             to-mpi)
                           #:nominal-module (module-path-index-shift (module-binding-nominal-module b)
                                                                     from-mpi
                                                                     to-mpi)
                           #:extra-nominal-bindings (for/list ([b (in-list (module-binding-extra-nominal-bindings b))])
                                                      (binding-module-path-index-shift b from-mpi to-mpi)))]
   [else b]))

(define (syntax-transfer-shifts to-s from-s [inspector #f] #:non-source? [non-source? #f])
  (syntax-add-shifts to-s (syntax-mpi-shifts from-s) inspector #:non-source? non-source?))

(define (syntax-add-shifts to-s shifts [inspector #f] #:non-source? [non-source? #f])
  (cond
   [(and (null? shifts) inspector)
    (syntax-set-inspector to-s inspector)]
   [else
    (for/fold ([s to-s]) ([shift (in-list (reverse shifts))]
                          [i (in-naturals)])
      (syntax-module-path-index-shift s (shift-from shift) (shift-to shift) (and (zero? i) inspector)
                                      #:non-source? non-source?))]))

(define (syntax-set-inspector s insp)
  ;; This inspector merging is also implemented via propagations in "syntax.rkt"
  (struct-copy syntax s
               [inspector (or (syntax-inspector s)
                              insp)]
               [scope-propagations+tamper (if (datum-has-elements? (syntax-content s))
                                              (propagation-mpi-shift (syntax-scope-propagations+tamper s)
                                                                     #f
                                                                     insp
                                                                     (syntax-scopes s)
                                                                     (syntax-shifted-multi-scopes s)
                                                                     (syntax-mpi-shifts s))
                                              (syntax-scope-propagations+tamper s))]))

;; ----------------------------------------

;; We can imagine that a syntax object's source module is determined
;; by adding a module's path index as it is expanded to everything
;; that starts out in the module. It turns out that we're already
;; adding a module path index like that in the form of a shift. So, we
;; infer a source module from the module-path-index shifts that are
;; attached to the syntax object by starting with the initial shift
;; and working our way back.
;;
;; Shifts added for a `module->namespace` context shouldn't count
;; toward a module source, so those are added as `non-source-shift`
;; records, and we skip them here.
(define (syntax-source-module s [source? #f])
  (unless (syntax? s)
    (raise-argument-error 'syntax-track-origin "syntax?" s))
  (for/or ([shift (in-list (reverse (syntax-mpi-shifts s)))]
           #:unless (non-source-shift? shift))
    (define from-mpi (car shift))
    (define-values (path base) (module-path-index-split from-mpi))
    (and (not path)
         (module-path-index-resolved from-mpi)
         (let ([mpi (apply-syntax-shifts from-mpi (syntax-mpi-shifts s))])
           (if source?
               (resolved-module-path-name (module-path-index-resolve mpi #f))
               mpi)))))

(define (identifier-prune-to-source-module id)
  (unless (identifier? id)
    (raise-argument-error 'identifier-prune-to-source-module "identifier?" id))
  (struct-copy syntax (datum->syntax #f (syntax-e id) id id)
               [mpi-shifts (syntax-mpi-shifts id)]))
