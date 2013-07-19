#lang racket/base

(require (for-syntax racket/base
                     syntax/boundmap
                     syntax/name
                     syntax/parse
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
                     (let ([rename-bindings (get-member-bindings def-table target-sig #`(blame-positive #,blame-id))])
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

(define-for-syntax (unit/c/core name stx)
  (syntax-parse stx
    [(:import-clause/c :export-clause/c)
     (begin
       (define-values (isig tagged-import-sigs import-tagged-infos 
                            import-tagged-sigids import-sigs)
         (process-unit-import #'(i.s ...)))
       
       (define-values (esig tagged-export-sigs export-tagged-infos 
                            export-tagged-sigids export-sigs)
         (process-unit-export #'(e.s ...)))
       
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
       
       (check-duplicate-sigs import-tagged-infos isig null null)
       
       (check-duplicate-subs export-tagged-infos esig)
       
       (check-unit-ie-sigs import-sigs export-sigs)
       
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
       
       (with-syntax ([(isig ...) isig]
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
                           export-tagged-infos)])
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
                                ...)))
              #:projection
              (λ (blame)
                (λ (unit-tmp)
                  (unit/c-first-order-check 
                   unit-tmp
                   (vector-immutable
                    (cons 'import-name
                          (vector-immutable import-key ...)) ...)
                   (vector-immutable 
                    (cons 'export-name 
                          (vector-immutable export-key ...)) ...)
                   blame)
                  (make-unit
                   '#,name
                   (vector-immutable (cons 'import-name
                                           (vector-immutable import-key ...)) ...)
                   (vector-immutable (cons 'export-name 
                                           (vector-immutable export-key ...)) ...)
                   (unit-deps unit-tmp)
                   (λ ()
                     (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                       (values (lambda (import-table)
                                 (unit-fn #,(contract-imports
                                             #'import-table
                                             import-tagged-infos
                                             import-sigs
                                             contract-table
                                             #'blame)))
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
                 #f)))))))]))

(define-syntax/err-param (unit/c stx)
  (syntax-case stx ()
    [(_ . sstx)
     (let ([name (syntax-local-infer-name stx)])
       (unit/c/core name #'sstx))]))

(define (unit/c-first-order-check val expected-imports expected-exports blame)
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
                (if import?
                    (failed "contract does not list import ~a" sub-name)
                    (failed "unit must export signature ~a" sub-name)))))
          (loop (sub1 i)))))
    (unless (unit? val)
      (failed "not a unit"))
    (check-sig-subset expected-imports (unit-import-sigs val) #t)
    (check-sig-subset (unit-export-sigs val) expected-exports #f)
    #t))
