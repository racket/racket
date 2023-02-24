#lang racket/base

;; This module defines variants of the core unit forms that support inferred linking.

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse/pre
                     syntax/context
                     syntax/private/id-table
                     syntax/intdef
                     syntax/kerncase
                     syntax/name
                     syntax/stx
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt"
                     "exptime/unit-infer.rkt"
                     "exptime/syntax.rkt"
                     "contract-syntax.rkt")
         racket/contract/base
         racket/contract/region
         syntax/location
         "contract.rkt"
         "keywords.rkt"
         "runtime.rkt"
         "unit-core.rkt"
         "util.rkt")

(provide define-unit-binding
         define-unit
         define-unit/new-import-export
         define-compound-unit
         define-unit-from-context
         define-unit/contract

         define-unit/s unit/s
         define-compound-unit/infer compound-unit/infer
         define-values/invoke-unit/infer invoke-unit/infer)

;; -----------------------------------------------------------------------------
;; `define-unit` and related forms

(begin-for-syntax
  (define (tagged-sigid->tagged-siginfo x)
    (cons (car x)
          (signature-siginfo (lookup-signature (cdr x)))))

  ;; build-define-unit : syntax-object
  ;;                     (syntax-object -> (values syntax-object (listof identifier) (listof identifier) (listof identifier))
  ;;                     string ->
  ;;                     syntax-object
  (define (build-define-unit stx build err-msg #:contracted? contracted?)
    (syntax-case stx ()
      ((_ name . rest)
       (begin
         (check-id #'name)
         (let-values (((exp i e d) (parameterize ([current-syntax-context (syntax-property (current-syntax-context) 'inferred-name (syntax-e #'name))])
                                     (build #'rest))))
           (with-syntax ((((itag . isig) ...) i)
                         (((etag . esig) ...) e)
                         (((deptag . depsig) ...) d)
                         (contracted? contracted?))
             (syntax-protect
              (quasisyntax/loc (current-syntax-context)
                (begin
                  (define u #,(syntax-property exp 'inferred-name (syntax-e #'name)))
                  (define-syntax name
                    (make-set!-transformer
                     (make-unit-info (quote-syntax u)
                                     (list (cons 'itag (quote-syntax isig)) ...)
                                     (list (cons 'etag (quote-syntax esig)) ...)
                                     (list (cons 'deptag (quote-syntax depsig)) ...)
                                     (quote-syntax name)
                                     contracted?))))))))))
      ((_)
       (raise-stx-err err-msg))))

  ;; A marker used when the result of invoking a unit should not be contracted
  (define no-invoke-contract (gensym))
  (define (build-unit/contract stx)
    (syntax-parse stx
      #:context (current-syntax-context)
      [(i:import-clause/c
        e:export-clause/c
        d:opt-init-depends
        {~optional {~seq #:invoke/contract ~! b:body-clause/c}}
        . bexps)

       (define-values [exp isigs esigs deps]
         (build-unit
          (check-unit-syntax
           (syntax/loc stx
             ((import i.i.s ...) (export e.e.s ...) {~? {~@ . d}} . bexps)))))

       (define/syntax-parse name (syntax-local-infer-name (current-syntax-context)))
       (define/syntax-parse new-unit exp)
       (define/syntax-parse unit-contract (unit/c/core
                                           #'name
                                           (quasisyntax/loc stx
                                             (i e {~? {~@ . d}} {~? b}))))
       (values
        (syntax-protect
         (syntax/loc stx
           (contract unit-contract new-unit '(unit name) (current-contract-region) (quote name) (quote-srcloc name))))
        isigs esigs deps)])))

(define-for-syntax (build-define-unit-binding stx)
  (define (check-helper tagged-info)
    (cons (car (siginfo-names (cdr tagged-info)))
          (siginfo->key-exprs (cdr tagged-info) (car tagged-info))))
  
  (syntax-case stx (import export init-depend)
    ((unit-exp (import i ...) (export e ...) (init-depend idep ...))
     (let* ([ti (syntax->list #'(i ...))]
            [te (syntax->list #'(e ...))]
            [tidep (syntax->list #'(idep ...))]
            [tagged-import-sigids (map check-tagged-id ti)]
            [tagged-export-sigids (map check-tagged-id te)]
            [tagged-dep-sigids (map check-tagged-id tidep)]
            [tagged-import-infos (map tagged-sigid->tagged-siginfo tagged-import-sigids)]
            [tagged-export-infos (map tagged-sigid->tagged-siginfo tagged-export-sigids)]
            [tagged-dep-siginfos (map tagged-sigid->tagged-siginfo tagged-dep-sigids)])
       (check-duplicate-sigs tagged-import-infos ti tagged-dep-siginfos tidep)         
       (check-duplicate-subs tagged-export-infos te)
       (with-syntax ((((import-name . (import-keys ...)) ...)
                      (map check-helper tagged-import-infos))
                     (((export-name . (export-keys ...)) ...)
                      (map check-helper tagged-export-infos))
                     (form (stx-car (current-syntax-context))))
         (values
          (syntax-protect
           #`(let ([unit-tmp unit-exp])
               #,(syntax/loc #'unit-exp
                   (check-unit unit-tmp 'form))
               #,(syntax/loc #'unit-exp
                   (check-sigs unit-tmp 
                               (vector-immutable
                                (cons 'import-name
                                      (vector-immutable import-keys ...))
                                ...)
                               (vector-immutable
                                (cons 'export-name
                                      (vector-immutable export-keys ...))
                                ...)
                               'form))
               unit-tmp))
          tagged-import-sigids
          tagged-export-sigids
          tagged-dep-sigids))))))

(define-syntax/err-param (define-unit-binding stx)
  (build-define-unit
   stx #:contracted? #f
   (lambda (unit)
     (build-define-unit-binding (check-unit-body-syntax unit)))
   "missing unit name, unit expression, import clause, and export clause"))

(define-syntax/err-param (define-unit stx)
  (build-define-unit
   stx #:contracted? #f
   (lambda (unit)
     (build-unit (check-unit-syntax unit)))
   "missing unit name, import clause, and export clause"))

(define-syntax/err-param (define-unit/new-import-export stx)
  (build-define-unit
   stx #:contracted? #f
   (lambda (unit)
     (build-unit/new-import-export (check-unit-syntax unit)))
   "missing unit name, import clause, and export clause"))

(define-syntax/err-param (define-compound-unit stx)
  (build-define-unit
   stx #:contracted? #f
   build-compound-unit
   "missing unit name"))

(define-for-syntax (check-ufc-syntax stx)
  (syntax-case stx ()
    ((export-spec) (void))
    (()
     (raise-stx-err "missing export-spec"))
    (_
     (raise-stx-err "nothing is permitted after export-spec"))))

(define-syntax/err-param (define-unit-from-context stx)
  (build-define-unit
   stx #:contracted? #f
   (lambda (sig)
     (check-ufc-syntax sig)
     (build-unit-from-context sig))
   "missing unit name and signature"))

(define-syntax/err-param (define-unit/contract stx)
  (build-define-unit
   stx #:contracted? #t
   (λ (stx)
     (build-unit/contract stx))
   "missing unit name"))

;; -----------------------------------------------------------------------------
;; `unit/s`

(define-for-syntax (build-unit/s stx)
  (syntax-case stx (import export init-depend)
    [((import i ...) (export e ...) (init-depend d ...) u)
     (let* ([ui (lookup-def-unit #'u)]
            [unprocess (let ([i (make-syntax-delta-introducer #'u (unit-info-orig-binder ui))])
                         (lambda (p)
                           (unprocess-tagged-id (cons (car p) (i (cdr p))))))])
       (with-syntax ([(isig ...) (map unprocess (unit-info-import-sig-ids ui))]
                     [(esig ...) (map unprocess (unit-info-export-sig-ids ui))])
         (build-unit/new-import-export
          (syntax/loc stx
            ((import i ...) (export e ...) (init-depend d ...) ((esig ...) u isig ...))))))]))

(define-syntax/err-param (define-unit/s stx)
  (build-define-unit
   stx #:contracted? #f
   (λ (stx) (build-unit/s (check-unit-syntax stx)))
   "missing unit name"))

(define-syntax/err-param (unit/s stx)
  (syntax-case stx ()
    [(_ . stx)
     (let-values ([(u x y z) (build-unit/s (check-unit-syntax #'stx))])
       u)]))

;; -----------------------------------------------------------------------------
;; `compound-unit/infer`

(define-for-syntax (unprocess-tagged-id ti)
  (if (car ti)
      #`(tag #,(car ti) #,(cdr ti))
      (cdr ti)))

;; build-compound-unit/infer : syntax-object  -> 
;;                      (values syntax-object (listof identifier) (listof identifier))
;; constructs the code for a compound-unit/infer expression.  stx match the return of 
;; check-compound-syntax
;; The two additional values are the identifiers of the compound-unit's import and export
;; signatures
(define-for-syntax (build-compound-unit/infer stx)
  (define (lookup-tagged tid)
    (cons (car tid) (lookup-signature (cdr tid))))
  
  (define (process-signature s)
    (define l
      ((check-tagged 
        (lambda (b)
          (syntax-case* b (:) (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
            ((x : y)
             (and (identifier? #'x) (identifier? #'y))
             (list #'x #'y (signature-siginfo (lookup-signature #'y))))
            (x
             (identifier? #'x)
             (list (car (generate-temporaries (list #'x)))
                   #'x
                   (signature-siginfo (lookup-signature #'x))))
            (_
             (raise-stx-err "expected syntax matching <identifier> or (<identifier> : <identifier>)"
                            b)))))
       s))
    (apply make-link-record l))
  
  (define ((process-tagged-sigid introducer) sid)
    (make-link-record (car sid) #f (introducer (cdr sid)) (signature-siginfo (lookup-signature (cdr sid)))))
  
  (syntax-case stx ()
    (((import-clause ...) 
      (export-clause ...)
      (((out ...) u l ...) ...))
     (let* ([us (syntax->list #'(u ...))]
            [units (map lookup-def-unit us)]
            [import-sigs (map process-signature 
                              (syntax->list #'(import-clause ...)))]
            [sig-introducers (map (lambda (unit u) values) units us)]
            [sub-outs
             (map
              (lambda (outs unit sig-introducer)
                (define o
                  (map
                   (lambda (clause)
                     (define c (check-tagged-:-clause clause))
                     (make-link-record (car c) (cadr c) (cddr c)
                                       (signature-siginfo (lookup-signature (cddr c)))))
                   (syntax->list outs)))
                (complete-exports (map (process-tagged-sigid sig-introducer) (unit-info-export-sig-ids unit))
                                  o))
              (syntax->list #'((out ...) ...))
              units
              sig-introducers)]
            [link-defs (append import-sigs (apply append sub-outs))])
       
       (define lnk-table (make-bound-id-table))
       (define sig-table (make-hasheq))
       
       (let ([dup (check-duplicate-identifier (map link-record-linkid link-defs))])
         (when dup
           (raise-stx-err "duplicate identifier" dup)))
       
       (for-each
        (lambda (b)
          (bound-id-table-set! lnk-table (link-record-linkid b) b))
        link-defs)
       
       (for-each
        (lambda (b)
          (for-each
           (lambda (cid)
             (define there? (hash-ref sig-table cid #f))
             (hash-set! sig-table cid (if there? 'duplicate (link-record-linkid b))))
           (siginfo-ctime-ids (link-record-siginfo b))))
        link-defs)
       
       (let ([sub-ins
              (map
               (lambda (ins unit sig-introducer unit-stx)
                 (define is (syntax->list ins))
                 (define lrs
                   (map
                    (lambda (i)
                      (define tagged-lnkid (check-tagged-id i))
                      (define sig
                        (bound-id-table-ref lnk-table (cdr tagged-lnkid) #f))
                      (unless sig
                        (raise-stx-err "unknown linking identifier" i))
                      (make-link-record (car tagged-lnkid)
                                        (cdr tagged-lnkid)
                                        (link-record-sigid sig)
                                        (link-record-siginfo sig)))
                    is))
                 (check-duplicate-subs
                  (map 
                   (lambda (lr) (cons (link-record-tag lr) (link-record-siginfo lr)))
                   lrs)
                  is)
                 (complete-imports sig-table 
                                   lrs
                                   (map (process-tagged-sigid sig-introducer)
                                        (unit-info-import-sig-ids unit))
                                   unit-stx))
               (syntax->list #'((l ...) ...))
               units
               sig-introducers
               us)]
             [exports
              (map 
               (lambda (e)
                 (define tid (check-tagged-spec-syntax e #f identifier?))
                 (define lookup (bound-id-table-ref  lnk-table (cdr tid) #f))
                 (cond
                   [lookup (unprocess-tagged-id tid)]
                   [else
                    (let ([lnkid (hash-ref
                                  sig-table
                                  (car (siginfo-ctime-ids (signature-siginfo (lookup-signature (cdr tid)))))
                                  #f)])
                      (cond
                        [(not lnkid)
                         (raise-stx-err "no sub unit exports this signature" (cdr tid))]
                        [(eq? lnkid 'duplicate)
                         (raise-stx-err "multiple sub units export this signature" (cdr tid))]
                        [else 
                         (unprocess-tagged-id
                          (cons (car tid) lnkid))]))]))
               (syntax->list #'(export-clause ...)))])
         
         (define init-deps
           (for/fold ([init-deps '()]) ([u (in-list units)]
                                        [sub-in (in-list sub-ins)]
                                        [u-pos (in-naturals)])
             (for/fold ([init-deps init-deps]) ([dep (in-list (unit-info-deps u))])
               ;; Find the link for this dependency:
               (define lr
                 (for/or ([lr (in-list sub-in)])
                   (and (eq? (link-record-tag lr)
                             (car dep))
                        (siginfo-subtype (signature-siginfo (lookup-signature (link-record-sigid lr)))
                                         (signature-siginfo (lookup-signature (cdr dep))))
                        lr)))
               ;; If `lr` refers to an import, then propoagate the dependency.
               ;; If it refers to a linked unit, make sure that unit is earlier.
               (cond
                [(for/or ([import-sig (in-list import-sigs)])
                   (and (free-identifier=? (link-record-linkid import-sig)
                                           (link-record-linkid lr))
                        import-sig))
                 ;; imported
                 => (lambda (import-sig)
                      (cons (cons (link-record-tag import-sig)
                                  (link-record-sigid import-sig))
                            init-deps))]
                [(for/or ([sub-out (in-list sub-outs)]
                          [i-pos (in-naturals)])
                   (for/or ([olr (in-list sub-out)])
                     (and (free-identifier=? (link-record-linkid olr)
                                             (link-record-linkid lr))
                          i-pos)))
                 => (lambda (i-pos)
                      (unless (i-pos . < . u-pos)
                        (raise-stx-err "unit depends on initialization of later unit" 
                                       (link-record-linkid lr)))
                      init-deps)]
                [else
                 (error "internal error: cannot find link source for init-dependency check")]))))
         
         (with-syntax (((import-clause ...)
                        (map unprocess-link-record-bind import-sigs))
                       (((out ...) ...)
                        (map
                         (lambda (out) 
                           (map unprocess-link-record-bind out))
                         sub-outs))
                       (((in ...) ...)
                        (map
                         (lambda (ins)
                           (map unprocess-link-record-use ins))
                         sub-ins))
                       ((unit-id ...) (map 
                                       (lambda (u stx)
                                         (quasisyntax/loc stx #,(syntax-local-introduce (unit-info-unit-id u))))
                                       units (syntax->list #'(u ...)))))
           (build-compound-unit #`((import import-clause ...)
                                   (export #,@exports)
                                   (link ((out ...) unit-id in ...) ...))
                                init-deps)))))
    (((i ...) (e ...) (l ...))
     (for-each check-link-line-syntax (syntax->list #'(l ...))))))


(define-for-syntax (check-compound/infer-syntax stx)
  (syntax-case (check-compound-syntax stx) ()
    ((i e (b ...))
     (with-syntax (((b ...)
                    (map
                     (lambda (b)
                       (if (identifier? b)
                           #`(() #,b)
                           b))
                     (syntax->list #'(b ...)))))
       #'(i e (b ...))))))

(define-syntax/err-param (compound-unit/infer stx)
  (let-values (((u i e d)
                (build-compound-unit/infer
                 (check-compound/infer-syntax 
                  (syntax-case stx () ((_ . x) #'x))))))
    u))

(define-for-syntax (do-define-compound-unit/infer stx)
  (build-define-unit
   stx #:contracted? #f
   (lambda (clause)
     (build-compound-unit/infer (check-compound/infer-syntax clause)))
   "missing unit name"))

(define-syntax/err-param (define-compound-unit/infer stx)
  (do-define-compound-unit/infer stx))

;; -----------------------------------------------------------------------------
;; `invoke-unit/infer`

(begin-for-syntax
  (define (parse-tagged-import-spec stx)
    (syntax-parse stx
      #:context (current-syntax-context)
      [spec:tagged-import-spec
       (attribute spec.value)]))

  (define (parse-tagged-export-spec stx)
    (syntax-parse stx
      #:context (current-syntax-context)
      [spec:tagged-export-spec
       (attribute spec.value)]))

  (define (signature-ie-subsumes? a b)
    (and (eq? (signature-ie-tag-sym a) (signature-ie-tag-sym b))
         (siginfo-subtype (signature-ie-siginfo a) (signature-ie-siginfo b))))

  (define (any-signature-ie-subsumes? as b)
    (ormap (λ (a) (signature-ie-subsumes? a b)) as)))

;; (syntax or listof[syntax]) boolean (boolean or listof[syntax]) -> syntax
(define-for-syntax (build-invoke-unit/infer units define? exports values-clause)
  (define (imps/exps-from-unit u)
    (define ui (lookup-def-unit u))
    (define (unprocess p)
      #`(bind-at #,u #,(unprocess-tagged-id p)))
    (define unprocess-export (if exports unprocess-tagged-id unprocess))
    (values (for/list ([id (in-list (unit-info-import-sig-ids ui))])
              (parse-tagged-import-spec (unprocess id)))
            (for/list ([id (in-list (unit-info-export-sig-ids ui))])
              (parse-tagged-export-spec (unprocess-export id)))))
  
  (define (imps/exps-from-units units exports)
    (define-values [ispecs especs]
      (for/fold ([ispecs '()] [especs '()])
                ([unit (in-list units)])
        (define-values [is es] (imps/exps-from-unit unit))
        (values (append is ispecs) (append es especs))))

    (define kept-imports
      (let loop ([left ispecs]
                 [kept '()])
        (cond
          [(null? left)
           kept]
          [else
           (define ispec (car left))
           (define left* (cdr left))
           (if (or (any-signature-ie-subsumes? left* ispec)
                   (any-signature-ie-subsumes? especs ispec))
               (loop left* kept)
               (loop left* (cons ispec kept)))])))

    (define kept-exports
      (cond
        [(list? exports)
         (define given-especs (map parse-tagged-import-spec exports))
         (for ([export (in-list exports)])
           (define given-espec (parse-tagged-import-spec export))
           (unless (any-signature-ie-subsumes? especs given-espec)
             (wrong-syntax (signature-ie-sig-id given-espec) "no subunit exports signature")))
         given-especs]
        [else
         especs]))

    (values kept-imports kept-exports))

  (when (and (not define?) exports)
    (error 'build-invoke-unit/infer 
           "internal error: exports for invoke-unit/infer"))
  (when (null? units)
    (raise-stx-err "no units in link clause"))

  (cond
    [(identifier? units)
     (let-values ([(isigs esigs) (imps/exps-from-units (list units) exports)])
       (with-syntax ([u units]
                     [(espec ...) (map signature-ie-src-stx esigs)]
                     [(ispec ...) (map signature-ie-src-stx isigs)])
         (syntax-protect
          (if define?
              (quasisyntax/loc (current-syntax-context)
                (define-values/invoke-unit u
                  (import ispec ...)
                  (export espec ...)
                  #,@(if values-clause (list values-clause) '())))
              (syntax/loc (current-syntax-context)
                (invoke-unit u (import ispec ...)))))))]

    [(list? units)
     (let-values ([(isigs esigs) (imps/exps-from-units units exports)])
       (with-syntax ([(new-unit) (generate-temporaries '(new-unit))]
                     [(unit ...) units]
                     [(espec ...) (map signature-ie-src-stx esigs)]
                     [(ispec ...) (map signature-ie-src-stx isigs)]
                     [(esig ...) (map signature-ie->tagged-sig-id esigs)]
                     [(isig ...) (map signature-ie->tagged-sig-id isigs)])
         (with-syntax ([u (let-values ([(u i e d)
                                        (build-compound-unit/infer
                                         (check-compound/infer-syntax
                                          #'((import isig ...)
                                             (export esig ...)
                                             (link unit ...))))])
                            u)])
           (syntax-protect
            (if define?
                (quasisyntax/loc (current-syntax-context)
                  (define-values/invoke-unit u
                    (import ispec ...)
                    (export espec ...)
                    #,@(if values-clause (list values-clause) '())))
                (syntax/loc (current-syntax-context)
                  (invoke-unit u (import isig ...))))))))]

    ;; just for error handling
    [else
     (lookup-def-unit units)]))

(begin-for-syntax
  (define-syntax-class invoke-units-clause
    #:description "unit clause"
    #:attributes [units]
    #:commit
    #:literals [link]
    (pattern (link ~! unit:id ...)
      #:attr units (attribute unit))
    (pattern unit:id
      #:attr units #'unit)))

(define-syntax define-values/invoke-unit/infer
  (syntax-parser
    #:track-literals
    #:literals [export]
    ;; We allow the export clause to appear before the unit
    ;; clause for backward compatibility.
    [(_ {~describe "export clause" (export ~! e ...)}
        units:invoke-units-clause)
     (build-invoke-unit/infer (attribute units.units)
                              #t
                              (attribute e)
                              #f)]
    [(_ units:invoke-units-clause
        {~optional {~describe "export clause" (export ~! e ...)}}
        {~optional results:invoke-results-clause})
     (build-invoke-unit/infer (attribute units.units)
                              #t
                              (attribute e)
                              (attribute results))]))

(define-syntax/err-param (invoke-unit/infer stx)
  (syntax-case stx ()
    [(_ (link unit ...))
     (build-invoke-unit/infer (syntax->list #'(unit ...)) #f #f #f)]
    [(_ u) (build-invoke-unit/infer #'u #f #f #f)]
    [(_)
     (raise-stx-err "missing unit" stx)]
    [(_ . b)
     (raise-stx-err
      (format "expected syntax matching (~a <define-unit-identifier>) or (~a (link <define-unit-identifier> ...))"
              (syntax-e (stx-car stx)) (syntax-e (stx-car stx))))]))
