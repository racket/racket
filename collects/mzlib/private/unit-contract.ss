#lang scheme/base

(require (for-syntax scheme/base
                     stxclass
                     syntax/boundmap
                     "unit-compiletime.ss")
         scheme/contract
         scheme/pretty
         "unit-keywords.ss"
         "unit-utils.ss"
         "unit-runtime.ss")

(provide unit/c)

(define-for-syntax (contract-imports/exports import?)
  (λ (table-stx import-tagged-infos import-sigs ctc-table pos neg src-info name)
    (define def-table (make-bound-identifier-mapping))
    
    (define (convert-reference vref ctc sig-ctc rename-bindings)
      (let ([wrap-with-proj
             (λ (ctc stx)
               ;; If contract coersion ends up being a large overhead, we can
               ;; store the result in a local box, then just check the box to
               ;; see if we need to coerce.
               #`(let ([ctc (coerce-contract 'unit/c (letrec-syntax #,rename-bindings #,ctc))])
                   ((((proj-get ctc) ctc)
                     #,(if import? neg pos)
                     #,(if import? pos neg)
                     #,src-info
                     #,name)
                    #,stx)))])
        (if ctc
            #`(cons
               (λ ()
                 (let* ([old-v 
                         #,(if sig-ctc
                               #`(let ([old-v/c ((car #,vref))])
                                   (cons #,(wrap-with-proj ctc #'(car old-v/c))
                                         (cdr old-v/c)))
                               (wrap-with-proj ctc #`((car #,vref))))])
                   old-v))
               (λ (v) 
                 (let* ([new-v 
                         #,(if sig-ctc
                               #`(cons #,(wrap-with-proj ctc #'(car v))
                                       (cdr v))
                               (wrap-with-proj ctc #'v))])
                   ((cdr #,vref) new-v))))
            vref)))
    (for ([tagged-info (in-list import-tagged-infos)]
          [sig         (in-list import-sigs)])
         (let ([v #`(hash-ref #,table-stx #,(car (tagged-info->keys tagged-info)))])
           (for ([int/ext-name (in-list (car sig))]
                 [index        (in-list (build-list (length (car sig)) values))])
                (bound-identifier-mapping-put! def-table
                                               (car int/ext-name)
                                               #`(vector-ref #,v #,index)))))
    (with-syntax ((((eloc ...) ...)
                   (for/list ([target-sig import-sigs])
                     (let ([rename-bindings 
                            (get-member-bindings def-table target-sig pos)])
                       (for/list ([target-int/ext-name (in-list (car target-sig))]
                                  [sig-ctc (in-list (cadddr target-sig))])
                         (let* ([vref
                                 (bound-identifier-mapping-get
                                  def-table
                                  (car target-int/ext-name))]
                                [ctc
                                 (bound-identifier-mapping-get
                                  ctc-table
                                  (car target-int/ext-name)
                                  (λ () #f))])
                           (convert-reference vref ctc sig-ctc rename-bindings))))))
                  (((export-keys ...) ...) 
                   (map tagged-info->keys import-tagged-infos)))
      #'(unit-export ((export-keys ...)
                      (vector-immutable eloc ...)) ...))))

(define-for-syntax contract-imports (contract-imports/exports #t))
(define-for-syntax contract-exports (contract-imports/exports #f))

(define-syntax/err-param (unit/c stx)
  (begin
    (define-syntax-class sig-id
      (pattern x
               #:declare x (static-of 'signature 
                                      (λ (x)
                                        (signature? (set!-trans-extract x))))))
    (define-syntax-class unit/c-clause
      #:transparent
      (pattern (s:sig-id [x:identifier c:expr] ...))
      (pattern s:sig-id ;; allow a non-wrapped sig-id, which is the same as (sig-id)
               #:with (x ...) null
               #:with (c ...) null))
    (define-syntax-class import-clause #:literals (import)
      #:transparent
      (pattern (import i:unit/c-clause ...)))
    (define-syntax-class export-clause #:literals (export)
      #:transparent
      (pattern (export e:unit/c-clause ...)))
    (syntax-parse stx
      [(_ (import i:unit/c-clause ...)
          (export e:unit/c-clause ...) bad-expr . rest)
       (raise-syntax-error 'unit/c
                           "extra form"
                           #'bad-expr)]
      [(_ :import-clause :export-clause)
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
               (raise-syntax-error 'unit/c
                                   (format "duplicate identifier found for signature ~a"
                                           (syntax->datum name))
                                   dup)))
           (let ([ids (map car (car sig))])
             (for-each (λ (id)
                         (unless (memf (λ (i) (bound-identifier=? id i)) ids)
                           (raise-syntax-error 'unit/c
                                               (format "identifier not member of signature ~a" 
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
         
         (with-syntax ([((import-key ...) ...)
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
               (make-proj-contract
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
                                  ...)))
                (λ (pos neg src-info name)
                  (λ (unit-tmp)
                    (unless (unit? unit-tmp)
                      (raise-contract-error unit-tmp src-info pos name
                                            "value is not a unit"))
                    (contract-check-sigs 
                     unit-tmp
                     (vector-immutable
                      (cons 'import-name
                            (vector-immutable import-key ...)) ...)
                     (vector-immutable 
                      (cons 'export-name 
                            (vector-immutable export-key ...)) ...)
                     src-info pos name)
                    (make-unit
                     #f
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
                                               #'pos
                                               #'neg
                                               #'src-info
                                               #'name)))
                                 #,(contract-exports 
                                    #'export-table
                                    export-tagged-infos
                                    export-sigs
                                    contract-table
                                    #'pos
                                    #'neg
                                    #'src-info
                                    #'name)))))))
                (λ (v)
                  (and (unit? v)
                       (with-handlers ([exn:fail:contract? (λ () #f)])
                         (contract-check-sigs 
                          v
                          (vector-immutable
                           (cons 'import-name
                                 (vector-immutable import-key ...)) ...)
                          (vector-immutable 
                           (cons 'export-name 
                                 (vector-immutable export-key ...)) ...)
                          (list #f "not-used") 'not-used null))
                       #t)))))))]
      [(_ (import i:unit/c-clause ...) bad-e . body)
       (raise-syntax-error 'unit/c
                           "expected an export description"
                           #'bad-e)]
      [(_ (import i:unit/c-clause ...))
       (raise-syntax-error 'unit/c
                           "expected an export description"
                           stx)]
      [(_ bad-i . rest)
       (raise-syntax-error 'unit/c
                           "expected an import description"
                           #'bad-i)]
      [(_)
       (raise-syntax-error 'unit/c
                           "expected an import description"
                           stx)])))

(define (contract-check-helper sub-sig super-sig import? val src-info blame ctc)
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
            (raise-contract-error
             val src-info blame ctc
             (cond
               [import?
                (format "contract does not list import ~a" sub-name)]
              [else
                (format "unit must export signature ~a" sub-name)])))))
      (loop (sub1 i)))))

(define (contract-check-sigs unit expected-imports expected-exports src-info blame ctc)
    (contract-check-helper expected-imports (unit-import-sigs unit) #t unit src-info blame ctc)
    (contract-check-helper (unit-export-sigs unit) expected-exports #f unit src-info blame ctc))
