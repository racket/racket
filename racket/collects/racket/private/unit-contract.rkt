#lang racket/base

(require (for-syntax racket/base
                     syntax/boundmap
                     syntax/name
                     syntax/parse
                     (only-in racket/syntax generate-temporary)
                     "unit-compiletime.rkt"
                     "unit-contract-syntax.rkt"
                     "unit-syntax.rkt")
         (for-meta 2 racket/base)
         racket/contract/base
         racket/contract/combinator
         "unit-utils.rkt"
         "unit-runtime.rkt")

(provide (for-syntax unit/c/core) unit/c)

(define-for-syntax (contract-imports/exports import?)
  (λ (table-stx import-tagged-infos import-sigs ctc-table blame-id)
    (define def-table (make-bound-identifier-mapping))
    
    (define (convert-reference var vref ctc sig-ctc rename-bindings)
      (let ([wrap-with-proj
             (λ (ctc stx)
               ;; If contract coersion ends up being a large overhead, we can
               ;; store the result in a local box, then just check the box to
               ;; see if we need to coerce.
               #`(let ([ctc (coerce-contract 'unit/c (letrec-syntax #,rename-bindings #,ctc))])
                   (((contract-projection ctc)
                     #,(if import? #`(blame-swap #,blame-id) blame-id))
                    #,stx)))])
        (if ctc
            #`(λ ()
                #,(if sig-ctc
                      #`(cons #,(wrap-with-proj 
                                 ctc 
                                 (with-syntax ([sig-ctc-stx
                                                (syntax-property sig-ctc
                                                                 'inferred-name
                                                                 var)])
                                   #`(let ([old-v/c (#,vref)])
                                       (contract sig-ctc-stx (car old-v/c)
                                                 (cdr old-v/c) (blame-positive #,blame-id)
                                                 (quote #,var) (quote-syntax #,var)))))
                              (blame-negative #,blame-id))
                      (wrap-with-proj ctc #`(#,vref))))
            vref)))
    (for ([tagged-info (in-list import-tagged-infos)]
          [sig         (in-list import-sigs)])
         (let ([v #`(hash-ref #,table-stx #,(car (tagged-info->keys tagged-info)))])
           (for ([int/ext-name (in-list (car sig))]
                 [index        (in-list (build-list (length (car sig)) values))])
                (bound-identifier-mapping-put! def-table (car int/ext-name)
                                               #`(vector-ref #,v #,index)))))
    (with-syntax ((((eloc ...) ...)
                   (for/list ([target-sig import-sigs])
                     (let ([rename-bindings (get-member-bindings def-table target-sig #`(blame-positive #,blame-id) #f)])
                       (for/list ([target-int/ext-name (in-list (car target-sig))]
                                  [sig-ctc (in-list (cadddr target-sig))])
                         (let* ([var (car target-int/ext-name)]
                                [vref (bound-identifier-mapping-get def-table var)]
                                [ctc (bound-identifier-mapping-get ctc-table var (λ () #f))])
                           (convert-reference var vref ctc sig-ctc rename-bindings))))))
                  (((export-keys ...) ...) 
                   (map tagged-info->keys import-tagged-infos)))
      #'(unit-export ((export-keys ...)
                      (vector-immutable eloc ...)) ...))))

(define-for-syntax contract-imports (contract-imports/exports #t))
(define-for-syntax contract-exports (contract-imports/exports #f))
;; This is copied from the unit implementation, but can't be required
;; from there since unit.rkt also requires this file
(define-for-syntax (tagged-sigid->tagged-siginfo x)
  (cons (car x)
        (signature-siginfo (lookup-signature (cdr x)))))

(define-for-syntax (unit/c/core name stx)
  (syntax-parse stx
    [(:import-clause/c
      :export-clause/c
      (~optional d:dep-clause #:defaults ([(d.s 1) null]))
      b:body-clause/c)
     (begin
       (define-values (isig tagged-import-sigs import-tagged-infos 
                            import-tagged-sigids import-sigs)
         (process-unit-import #'(i.s ...)))
       
       (define-values (esig tagged-export-sigs export-tagged-infos 
                            export-tagged-sigids export-sigs)
         (process-unit-export #'(e.s ...)))

       (define deps (syntax->list #'(d.s ...)))
       (define dep-tagged-siginfos
         (map tagged-sigid->tagged-siginfo
              (map check-tagged-id deps)))

       (define apply-body-contract (attribute b.apply-invoke-ctcs))
       (define make-define-ctcs/blame (attribute b.make-define-ctcs/blame))

       (define contract-table
         (make-bound-identifier-mapping))
       
       (define (process-sig name sig xs cs)
         (define xs-list (syntax->list xs))
         (let ([dup (check-duplicate-identifier xs-list)])
           (when dup
             (raise-stx-err (format "duplicate identifier found for signature ~a"
                                    (syntax->datum name))
                            dup)))
         (let ([ids (map car (car sig))])
           (for-each (λ (id)
                       (unless (memf (λ (i) (bound-identifier=? id i)) ids)
                         (raise-stx-err (format "identifier not member of signature ~a" 
                                                (syntax-e name))
                                        id)))
                     xs-list))
         (for ([x (in-list xs-list)]
               [c (in-list (syntax->list cs))])
              (bound-identifier-mapping-put! contract-table x c)))
       
       (check-duplicate-sigs import-tagged-infos isig dep-tagged-siginfos deps)
       (check-duplicate-subs export-tagged-infos esig)
       
       (for-each process-sig
                 isig
                 import-sigs
                 (syntax->list #'((i.x ...) ...))
                 (syntax->list #'((i.c ...) ...)))
       (for-each process-sig
                 esig
                 export-sigs
                 (syntax->list #'((e.x ...) ...))
                 (syntax->list #'((e.c ...) ...)))
       
       (with-syntax ([((dept . depr) ...)
                      (map
                       (lambda (tinfo)
                         (cons (car tinfo)
                               (syntax-local-introduce (car (siginfo-rtime-ids (cdr tinfo))))))
                       dep-tagged-siginfos)]
                     [(isig ...) isig]
                     [(esig ...) esig]
                     [((import-key ...) ...)
                      (map tagged-info->keys import-tagged-infos)]
                     [((export-key ...) ...)
                      (map tagged-info->keys export-tagged-infos)]
                     [(import-name ...)
                      (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                           import-tagged-infos)]
                     [(export-name ...)
                      (map (lambda (tag/info) (car (siginfo-names (cdr tag/info))))
                           export-tagged-infos)]
                     [ctcs/blame (generate-temporary 'ctcs/blame)])
         (quasisyntax/loc stx
           (begin
             (make-contract
              #:name
              (list 'unit/c
                    (cons 'import 
                          (list (cons 'isig
                                      (map list (list 'i.x ...) 
                                           (build-compound-type-name 'i.c ...)))
                                ...))
                    (cons 'export 
                          (list (cons 'esig
                                      (map list (list 'e.x ...)
                                           (build-compound-type-name 'e.c ...)))
                                ...))
                    (cons 'init-depend
                          (list 'd.s ...))
                    #,@(attribute b.name))
              #:projection
              (λ (blame)
                #,@(make-define-ctcs/blame #'ctcs/blame #'blame)
                (λ (unit-tmp)
                  (unit/c-first-order-check 
                   unit-tmp
                   (vector-immutable
                    (cons 'import-name
                          (vector-immutable import-key ...)) ...)
                   (vector-immutable 
                    (cons 'export-name 
                          (vector-immutable export-key ...)) ...)
                   (list (cons 'dept depr) ...)
                   blame)
                  (make-unit
                   '#,name
                   (vector-immutable (cons 'import-name
                                           (vector-immutable import-key ...)) ...)
                   (vector-immutable (cons 'export-name 
                                           (vector-immutable export-key ...)) ...)
                   (list (cons 'dept depr) ...)
                   (λ ()
                     (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                       (values (lambda (import-table)
                                 #,(apply-body-contract
                                    #`(unit-fn #,(contract-imports
                                                  #'import-table
                                                  import-tagged-infos
                                                  import-sigs
                                                  contract-table
                                                  #'blame))
                                    #'blame
                                   #'ctcs/blame))
                               #,(contract-exports 
                                  #'export-table
                                  export-tagged-infos
                                  export-sigs
                                  contract-table
                                  #'blame)))))))
              #:first-order
              (λ (v)
                (unit/c-first-order-check 
                 v
                 (vector-immutable
                  (cons 'import-name
                        (vector-immutable import-key ...)) ...)
                 (vector-immutable 
                  (cons 'export-name 
                        (vector-immutable export-key ...)) ...)
                 (list (cons 'dept depr) ...)
                 #f)))))))]))

(define-syntax/err-param (unit/c stx)
  (syntax-case stx ()
    [(_ . sstx)
     (let ([name (syntax-local-infer-name stx)])
       (unit/c/core name #'sstx))]))

(define (unit/c-first-order-check val expected-imports expected-exports expected-deps blame)
  (let/ec return
    (define (failed str . args)
      (if blame
          (apply raise-blame-error blame val str args)
          (return #f)))
    (define (check-sig-subset sub-sig super-sig import?)
      (define t (make-hash))
      (let loop ([i (sub1 (vector-length sub-sig))])
        (when (>= i 0)
          (let ([v (cdr (vector-ref sub-sig i))])
            (let loop ([j (sub1 (vector-length v))])
              (when (>= j 0)
                (let ([vj (vector-ref v j)])
                  (hash-set! t vj
                             (if (hash-ref t vj #f)
                                 'amb
                                 #t)))
                (loop (sub1 j)))))
          (loop (sub1 i))))
      (let loop ([i (sub1 (vector-length super-sig))])
        (when (>= i 0)
          (let* ([v0 (vector-ref (cdr (vector-ref super-sig i)) 0)]
                 [r (hash-ref t v0 #f)])
            (when (not r)
              (let ([sub-name (car (vector-ref super-sig i))])
                (define tag-part (vector-ref (cdr (vector-ref super-sig i)) 0))
                (define tag (and (pair? tag-part) (car tag-part)))
                (failed
                 (string-append
                  (if import?
                      (format "contract does not list import ~a" sub-name)
                      (format "unit must export signature ~a" sub-name))
                  (if tag
                      (format " with tag ~a" tag)
                      ""))))))
          (loop (sub1 i)))))
    ;; check that the dependencies of the given unit are consistent with the
    ;; dependencies specified by the contract. Ensures that the given dependencies
    ;; are a subset of the expected dependencies otherwise raises a contract error.
    (define (check-dependencies expected given imports)
      (define (lookup dep lst)
        (member dep lst (lambda (p1 p2)
                          (and (eq? (car p1) (car p2))
                               (eq? (cdr p1) (cdr p2))))))
      ;; Normalize dependencies to be symbols or pairs of tags and symbols
      (define (normalize-deps deps)
        (map (lambda (dep) (if (car dep) dep (cdr dep))) deps))
      (define t (for*/hash ([i (in-vector imports)]
                            [v (in-value (cdr i))]
                            [im (in-value (vector-ref v 0))]
                            #:when (member im (normalize-deps expected))
                            [vj (in-vector v)])
                  (values vj #t)))
      ;; use the imports to get the name and tag of dependency
      (define (get-name dep-tag)
        (define tag-table
          (for/hash ([e (in-vector imports)])
            (define name (car e))
            (define v (vector-ref (cdr e) 0))
            (define tag (if (pair? v) (cdr v) v))
            (values tag name)))
        (hash-ref tag-table dep-tag #f))
     (for ([dep (in-list (normalize-deps given))])
       (unless (hash-ref t dep #f)
         (define tag (and (pair? dep) (car dep)))
         (define sig-tag (or (and (pair? dep) (cdr dep)) dep))
         (failed
               (string-append
                (format "contract does not list initialization dependency ~a"
                        (get-name sig-tag))
                (if tag
                    (format " with tag ~a" tag)
                    ""))))))
    (unless (unit? val)
      (failed "not a unit"))
    (check-sig-subset expected-imports (unit-import-sigs val) #t)
    (check-sig-subset (unit-export-sigs val) expected-exports #f)
    (check-dependencies expected-deps (unit-deps val) expected-imports)
    #t))
