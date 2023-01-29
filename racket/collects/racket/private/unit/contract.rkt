#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/id-table
                     syntax/name
                     syntax/parse
                     "exptime/import-export.rkt"
                     "exptime/signature.rkt"
                     "contract-syntax.rkt")
         (for-meta 2 racket/base)
         racket/contract/base
         racket/contract/combinator
         "runtime.rkt"
         "util.rkt")

(provide (for-syntax unit/c/core) unit/c)

(define-for-syntax (contract-imports/exports import?)
  (λ (table-stx sigs tags ctc-table blame-id)
    (define def-tables (make-hasheq)) ; tag (or #f) =>  bound-identifier-mapping
    
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
    (for ([sig (in-list sigs)]
          [tag (in-list tags)])
      (define def-table (hash-ref! def-tables tag make-bound-id-table))
      (define v #`(hash-ref #,table-stx #,(siginfo->key-expr (signature-siginfo sig) tag)))
      (for ([(int/ext-name index) (in-indexed (in-list (signature-vars sig)))])
        (bound-id-table-set! def-table (car int/ext-name) #`(vector-ref #,v #,index))))
    (with-syntax ((((eloc ...) ...)
                   (for/list ([target-sig (in-list sigs)]
                              [target-tag (in-list tags)])
                     (define def-table (hash-ref def-tables target-tag))
                     (let ([rename-bindings (get-member-bindings def-table target-sig #`(blame-positive #,blame-id) #f)])
                       (for/list ([target-int/ext-name (in-list (signature-vars target-sig))]
                                  [sig-ctc (in-list (signature-ctcs target-sig))])
                         (let* ([var (car target-int/ext-name)]
                                [vref (bound-id-table-ref def-table var)]
                                [ctc (bound-id-table-ref ctc-table var #f)])
                           (convert-reference var vref ctc sig-ctc rename-bindings))))))
                  (((export-keys ...) ...)
                   (for/list ([sig (in-list sigs)]
                              [tag (in-list tags)])
                     (siginfo->key-exprs (signature-siginfo sig) tag))))
      #'(unit-export ((export-keys ...)
                      (vector-immutable eloc ...)) ...))))

(define-for-syntax contract-imports (contract-imports/exports #t))
(define-for-syntax contract-exports (contract-imports/exports #f))

(define-for-syntax (unit/c/core name stx)
  (syntax-parse stx
    #:context (current-syntax-context)
    [(:import-clause/c
      :export-clause/c
      d:optional-dep-clause
      b:body-clause/c)

     ;; The input syntax is restricted to `tagged-signature-id`, since
     ;; `unit/c` doesn’t allow the full grammar of imports and exports.
     ;; However, we still want to reparse each signature as an
     ;; import/export spec so that we split the signature’s variables
     ;; into internal/external pairs (see Note [Parsed signature
     ;; imports and exports] for details).
     ;;
     ;; Even though the internal/external ids will always have the same
     ;; symbolic names, doing this split is still important because the
     ;; internal ids are given the local lexical context, and we need
     ;; to be able to match them up with the provided contract
     ;; specifications using `bound-identifier=?`.
     (define/syntax-parse [in:tagged-import-spec ...] #'[i.s ...])
     (define/syntax-parse [out:tagged-export-spec ...] #'[e.s ...])

     (define apply-body-contract (attribute b.apply-invoke-ctcs))
     (define make-define-ctcs/blame (attribute b.make-define-ctcs/blame))

     (define contract-table (make-bound-id-table))
       
     (define (process-sig sig-stx sig-var-ids xs cs)
       (let ([dup (check-duplicate-identifier xs)])
         (when dup
           (raise-stx-err (format "duplicate identifier found for signature ~a"
                                  (syntax->datum sig-stx))
                          dup)))
       (for ([x (in-list xs)]
             [c (in-list cs)])
         (unless (memf (λ (i) (bound-identifier=? x i)) sig-var-ids)
           (raise-stx-err (format "identifier not member of signature ~a" 
                                  (syntax-e sig-stx))
                          x))
         (bound-id-table-set! contract-table x c)))

     (check-duplicate-sigs (map cons (attribute i.s.tag-sym) (attribute i.s.info))
                           (attribute i.s)
                           (map cons (attribute d.s.tag-sym) (attribute d.s.info))
                           (attribute d.s))
     (check-duplicate-subs (map cons (attribute e.s.tag-sym) (attribute e.s.info))
                           (attribute e.s))
       
     (for-each process-sig
               (attribute i.s)
               (attribute in.var.int-id)
               (attribute i.x)
               (attribute i.c))
     (for-each process-sig
               (attribute e.s)
               (attribute out.var.int-id)
               (attribute e.x)
               (attribute e.c))

     (define/syntax-parse [imports/exports/deps ...]
       #'[(vector-immutable (cons 'i.s.info.self-id (vector-immutable i.s.key ...)) ...)
          (vector-immutable (cons 'e.s.info.self-id (vector-immutable e.s.key ...)) ...)
          (list d.s.self-key ...)])

     (define/syntax-parse ctcs/blame (generate-temporary 'ctcs/blame))
       
     (quasisyntax/loc stx
       (make-contract
        #:name
        (list 'unit/c
              (cons 'import 
                    (list (cons 'i.s
                                (map list (list 'i.x ...) 
                                     (build-compound-type-name 'i.c ...)))
                          ...))
              (cons 'export 
                    (list (cons 'e.s
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
             imports/exports/deps ...
             blame)
            (make-unit
             '#,name
             imports/exports/deps ...
             (λ ()
               (let-values ([(unit-fn export-table) ((unit-go unit-tmp))])
                 (values (lambda (import-table)
                           #,(apply-body-contract
                              #`(unit-fn #,(contract-imports
                                            #'import-table
                                            (attribute in.value)
                                            (attribute in.tag-sym)
                                            contract-table
                                            #'blame))
                              #'blame
                              #'ctcs/blame))
                         #,(contract-exports 
                            #'export-table
                            (attribute out.value)
                            (attribute out.tag-sym)
                            contract-table
                            #'blame)))))))
        #:first-order
        (λ (v)
          (unit/c-first-order-check 
           v
           imports/exports/deps ...
           #f))))]))

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
      (define t (for*/hash ([i (in-vector imports)]
                            [v (in-value (cdr i))]
                            [im (in-value (vector-ref v 0))]
                            #:when (member im expected)
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
     (for ([dep (in-list given)])
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
