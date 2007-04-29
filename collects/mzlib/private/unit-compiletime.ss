(module unit-compiletime mzscheme
  (require-for-syntax (lib "struct.ss" "syntax"))
  (require (lib "boundmap.ss" "syntax")
           (lib "list.ss")
           "unit-runtime.ss"
           "unit-syntax.ss")
  (require-for-template mzscheme
                        "unit-keywords.ss"
                        "unit-runtime.ss")
  
  (provide (struct var-info (syntax? exported? id))
           (struct signature (siginfo vars val-defs stx-defs))
           (rename build-siginfo make-siginfo)
           siginfo-names siginfo-ctime-ids siginfo-rtime-ids siginfo-subtype
           (struct signature-form (f))
           (struct unit-info (unit-id import-sig-ids export-sig-ids))
           (struct link-record (linkid tag sigid siginfo))
           unprocess-link-record-bind unprocess-link-record-use
           set!-trans-extract do-identifier
           process-tagged-import process-tagged-export
           lookup-signature lookup-def-unit make-id-mapper make-id-mappers sig-names sig-int-names sig-ext-names
           map-sig split-requires apply-mac complete-exports complete-imports check-duplicate-subs
           process-spec process-spec2)
  
  
  (define-syntax (apply-mac stx)
    (syntax-case stx ()
      ((_ f x) ((syntax-e #'f) #'x))))

  ;; split-requires : (listof syntax-object) -> (values (listof syntax-object) (listof syntax-object))
  (define (split-requires l)
    (let loop ((l l)
               (requires null))
      (cond
        ((null? l) (cons (reverse requires) l))
        (else
         (syntax-case (car l) ()
           ((r . x)
            (or (module-identifier=? #'r #'require)
                (module-identifier=? #'r #'require-for-syntax)
                (module-identifier=? #'r #'require-for-template))
            (loop (cdr l) (cons (car l) requires)))
           (_
            (cons (reverse requires) l)))))))
                 
        
  ;; (make-var-info bool bool identifier)
  (define-struct var-info (syntax? exported? id))
  
  (define-syntax (define-struct/proc stx)
    (syntax-case stx ()
      ((_ name (field ...) p)
       (and (identifier? #'name)
            (andmap identifier? (syntax->list #'(field ...))))
       (generate-struct-declaration
        stx #'name #f (syntax->list #'(field ...)) (syntax-local-context)
        (lambda (orig-stx name-stx defined-name-stxes super-info)
          #`(make-struct-type '#,name-stx 
                              #,(and super-info (list-ref super-info 0))
                              #,(/ (- (length defined-name-stxes) 3) 2)
                              0 #f null (current-inspector)
                              p))))))
  ;; An int/ext is
  ;; - (cons identifier identifier)
  ;; A def is
  ;; - (listof (cons (listof int/ext) syntax-object))
  ;; A sig is
  ;; - (list (listof int/ext) (listof def) (listof def))
  ;; A tagged-sig is 
  ;; - (listof (cons #f siginfo) (cons #f identifier) sig)
  ;; - (listof (cons symbol siginfo) (cons symbol identifier) sig)
  
  ;; A siginfo is
  ;; - (make-siginfo (listof symbol) (listof symbol) (listof identifier) (hash-tableof symbol bool))
  ;; where the car of each list represents the signature, and the cdr represents
  ;; its super signatures.  All lists are non-empty and the same length.
  (define-struct siginfo (names ctime-ids rtime-ids super-table))
  
  ;; build-siginfo : (listof symbol) (listof symbol) (listof identifier) -> siginfo
  (define (build-siginfo names rtime-ids)
    (define ctime-ids 
      (cons (gensym)
            (if (null? (cdr names))
                null
                (siginfo-ctime-ids 
                 (signature-siginfo
                  (lookup-signature (cadr names)))))))
    (make-siginfo names
                  ctime-ids
                  rtime-ids 
                  (make-immutable-hash-table (map (λ (x) `(,x . #t)) ctime-ids))))
  
  ;; siginfo-subtype : siginfo siginfo -> bool
  (define (siginfo-subtype s1 s2)
    (hash-table-get (siginfo-super-table s1)
                    (car (siginfo-ctime-ids s2))
                    (λ () #f)))
  
  
  ;; A signature is 
  ;; (make-signature siginfo
  ;;                 (listof identifier)
  ;;                 (listof (cons (listof identifier) syntax-object))
  ;;                 (listof (cons (listof identifier) syntax-object)))
  (define-struct/proc signature (siginfo vars val-defs stx-defs)
    (lambda (_ stx)
      (parameterize ((error-syntax stx))
        (raise-stx-err "illegal use of signature name"))))

  ;; (make-signature-form (syntax-object -> any))
  (define-struct/proc signature-form (f)
    (lambda (_ stx)
      (parameterize ((error-syntax stx))
        (raise-stx-err "illegal use of signature form"))))
    
  ;; (make-unit-info identifier (listof (cons symbol identifier)) (listof (cons symbol identifier)))
  (define-struct/proc unit-info (unit-id import-sig-ids export-sig-ids deps)
    (lambda (struct stx) 
      (with-syntax ((u (unit-info-unit-id struct)))
        (syntax-case stx (set!)
          ((set! x y)
           #`(begin 
               #,(syntax/loc #'y (check-unit y 'set!))
               #,(syntax/loc #'y (check-sigs y (unit-import-sigs u) (unit-export-sigs u) 'set!))
               (set! u y)))
          ((_ . y)
           (syntax/loc stx (u . y)))
          (x
           (identifier? #'x)
           (quasisyntax/loc stx (values u))))))) ;; The apparently superfluous values is so the certificates aren't
                                                 ;; too permissive
    
  (define (lookup id err-msg)
    (check-id id)
    (let ((s (set!-trans-extract
              (syntax-local-value
               (syntax-local-introduce id)
               (lambda ()
                 (raise-stx-err err-msg id))))))
      s))
  
  ;; lookup-signature : syntax-object -> signature
  (define (lookup-signature id)
    (let ((s (lookup id "unknown signature")))
      (unless (signature? s)
        (raise-stx-err "not a signature" id))
      s))

  (define (set!-trans-extract x)
    (if (set!-transformer? x)
        (set!-transformer-procedure x)
        x))
  
  (define (lookup-def-unit id)
    (let ((u (lookup id "unknown unit definition")))
      (unless (unit-info? u)
        (raise-stx-err "not a unit definition" id))
      u))

  ;; check-module-id-subset : (listof syntax-object) (listof identifier) syntax-object -> 
  ;; ensures each element of i1 is an identifier module-identifier=? to an identifier in i2
  (define (check-module-id-subset i1 i2)
    (let ((ht (make-module-identifier-mapping)))
      (for-each (lambda (id)
                  (module-identifier-mapping-put! ht id #t))
                i2)
      (for-each
       (lambda (id)
         (check-id id)
         (unless (module-identifier-mapping-get ht id (lambda () #f))
           (raise-stx-err "listed identifier not present in signature specification" id)))
       i1)))
        
  ;; do-rename : sig syntax-object syntax-object -> sig
  ;; internals and externals must both be of the form (x ...)
  ;; ensures that each x above is an identifier
  (define (do-rename sig internals externals)
    (check-module-id-subset (syntax->list externals)
                            (sig-int-names sig))
    (let ((ht (make-module-identifier-mapping)))
      (for-each
       (lambda (int ext)
         (check-id int)
         (when (module-identifier-mapping-get ht ext (lambda () #f))
           (raise-stx-err "duplicate renamings" ext))
         (module-identifier-mapping-put! ht ext int))
       (syntax->list internals)
       (syntax->list externals))
      (map-sig
       (lambda (id)
         (module-identifier-mapping-get ht id (lambda () id)))
       (lambda (x) x)
       sig)))
  
  ;; do-prefix : sig syntax-object -> sig
  ;; ensures that pid is an identifier
  (define (do-prefix sig pid)
    (check-id pid)
    (let ((p (syntax-e pid)))
      (map-sig
       (lambda (id)
         (datum->syntax-object
          id
          (string->symbol (format "~a~a" p (syntax-e id)))))
       (lambda (x) x)
       sig)))

  ;; do-only : sig (listof identifier) -> sig
  ;; ensures that only-ids are identifiers and are mentioned in the signature
  (define (do-only/except sig only/except-ids put get)
    (check-module-id-subset only/except-ids
                            (sig-int-names sig))
    (let ((ht (make-module-identifier-mapping)))
      (for-each (lambda (id)
                  (module-identifier-mapping-put! ht id (put id)))
                only/except-ids)
      (map-sig
       (lambda (id)
         (module-identifier-mapping-get ht id
                                        (lambda () 
                                          (get id))))
       (lambda (x) x)
       sig)))
  
  ;; do-identifier : identifier (box (cons identifier siginfo)) -> sig
  (define (do-identifier spec res)
    (let* ((sig (lookup-signature spec))
           (vars (signature-vars sig))
           (vals (signature-val-defs sig))
           (stxs (signature-stx-defs sig)))
      (set-box! res (cons spec (signature-siginfo sig)))
      (map-sig intro-o-shadow
               syntax-local-introduce
               (list (map cons vars vars)
                     (map 
                      (λ (val)
                        (cons (map (λ (id) (cons id id))
                                   (car val))
                              (cdr val)))
                      vals)
                     (map 
                      (λ (stx)
                        (cons (map (λ (id) (cons id id))
                                   (car stx))
                              (cdr stx)))
                      stxs)))))
  
  (define (sig-names sig)
    (append (car sig)
            (apply append (map car (cadr sig)))
            (apply append (map car (caddr sig)))))
    
  
  (define (sig-int-names sig)
    (map car (sig-names sig)))

  (define (sig-ext-names sig)
    (map cdr (sig-names sig)))
  
  ;; intro-o-shadow : identifier -> identifier
  (define (intro-o-shadow id)
    (syntax-local-introduce (syntax-local-get-shadower id)))
  
  ;; map-def : (identifier -> identifier) (syntax-object -> syntax-object) def -> def
  (define (map-def f g def)
    (cons (map (lambda (x)
                 (cons (f (car x)) (g (cdr x))))
               (car def))
          (g (cdr def))))
  
  ;; map-sig : (identifier -> identifier) (sytnax-object -> syntax-object)  sig -> sig
  ;; applies f to the internal parts, and g to the external parts.
  (define (map-sig f g sig)
    (list (map (lambda (x) (cons (f (car x)) (g (cdr x)))) (car sig))
          (map (lambda (x) (map-def f g x)) (cadr sig))
          (map (lambda (x) (map-def f g x)) (caddr sig))))
  
  ;; An import-spec is one of
  ;; - signature-name
  ;; - (only import-spec identifier ...)
  ;; - (except import-spec identifier ...)
  ;; - (prefix prefix-identifier import-spec)
  ;; - (rename import-spec (local-identifier signature-identifier) ...)  
  
  ;; An export-spec is one of
  ;; - signature-name
  ;; - (prefix prefix-identifier export-spec)
  ;; - (rename export-spec (local-identifier signature-identifier) ...)
  
  ;; A tagged-import-spec is one of
  ;; - import-spec
  ;; - (tag symbol import-spec)

  ;; A tagged-export-spec is one of
  ;; - export-spec
  ;; - (tag symbol export-spec)

  
  ;; process-tagged-import/export : syntax-object boolean -> tagged-sig
  (define (process-tagged-import/export spec import?)
    (define res (box #f))
    (check-tagged-spec-syntax spec import? identifier?)
    (syntax-case spec (tag)
      ((tag sym spec)
       (let ([s (process-import/export #'spec res)])
         (list (cons (syntax-e #'sym) (cdr (unbox res)))
               (cons (syntax-e #'sym) (car (unbox res)))
               s)))
      ((tag . _)
       (raise-stx-err "expected (tag symbol <import/export-spec>)" spec))
      (_ (let ([s (process-import/export spec res)])
           (list (cons #f (cdr (unbox res)))
                 (cons #f (car (unbox res)))
                 s)))))
 
 
  ;; process-import/export : syntax-object (box (cons identifier) siginfo) -> sig
  (define (process-import/export spec res)
    (syntax-case spec (only except prefix rename)
      (_
       (identifier? spec)
       (do-identifier spec res))
      ((only sub-spec id ...)
       (do-only/except (process-import/export #'sub-spec res)
                       (syntax->list #'(id ...))
                       (lambda (x) x)
                       (lambda (id)
                         (car (generate-temporaries #`(#,id))))))
      ((except sub-spec id ...)
       (do-only/except (process-import/export #'sub-spec res)
                       (syntax->list #'(id ...))
                       (lambda (id)
                         (car (generate-temporaries #`(#,id))))
                       (lambda (x) x)))
      ((prefix pid sub-spec)
       (do-prefix (process-import/export #'sub-spec res) #'pid))
      ((rename sub-spec (internal external) ...)
       (let* ((sig-res
               (do-rename (process-import/export #'sub-spec res)
                          #'(internal ...)
                          #'(external ...)))
              (dup (check-duplicate-identifier (sig-int-names sig-res))))
         (when dup
           (raise-stx-err
            (format "rename created duplicate identifier ~a" (syntax-e dup))
            spec))
         sig-res))))

  (define (process-tagged-import spec)
    (process-tagged-import/export spec #t))
  (define (process-tagged-export spec)
    (process-tagged-import/export spec #f))
  
  ;; process-spec : syntax-object -> sig
  (define (process-spec spec)
    (check-tagged-spec-syntax spec #f identifier?)
    (process-import/export spec (box #f)))
      
  ;; process-spec2 : syntax-object -> identifier?
  (define (process-spec2 spec)
    (define b (box #f))
    (check-tagged-spec-syntax spec #t identifier?)
    (process-import/export spec b)
    (car (unbox b)))
  
  
;  ;; extract-siginfo : (union import-spec export-spec) -> ???
;  ;; extracts the identifier that refers to the signature
;  (define (extract-siginfo spec)
;    (syntax-case spec (only except prefix rename)
;      ((only sub-spec . x)
;       (extract-siginfo #'sub-spec))
;      ((except sub-spec . x)
;       (extract-siginfo #'sub-spec))
;      ((prefix pid sub-spec)
;       (extract-siginfo #'sub-spec))
;      ((rename sub-spec . x)
;       (extract-siginfo #'sub-spec))
;      (_ spec)))

  
  
  ;; check-duplicate-subs : (listof (cons symbol siginfo)) (listof syntax-object) ->
  (define (check-duplicate-subs tagged-siginfos sources)
    (for-each
     (λ (tinfo1 s1)
       (for-each 
        (λ (tinfo2 s2)
          (unless (eq? tinfo1 tinfo2)
            (when (and (eq? (car tinfo1) (car tinfo2))
                       (siginfo-subtype (cdr tinfo1) (cdr tinfo2)))
              (raise-stx-err (format "the signature of ~a extends this signature"
                                     (syntax-object->datum s1))
                             s2))))
        tagged-siginfos
        sources))
     tagged-siginfos
     sources))
    
  
  ;; A link-record is
  ;; (make-link-record (or symbol #f) (or identifier #f) identifier siginfo)
  (define-struct link-record (tag linkid sigid siginfo))
  
  ;; complete-exports : (listof link-record) (listof link-record) -> (listof link-record)
  ;; The export-bindings should not contain two bindings that are related as subsignatures.
  (define (complete-exports unit-exports given-bindings)
    (define binding-table (make-hash-table 'equal))
    (define used-binding-table (make-hash-table 'equal))
    
    (check-duplicate-subs 
     (map (λ (ts) (cons (link-record-tag ts) (link-record-siginfo ts))) given-bindings)
     (map link-record-sigid given-bindings))
    
    (for-each 
     (λ (b)
       (hash-table-put! binding-table 
                        (cons (link-record-tag b)
                              (car (siginfo-ctime-ids (link-record-siginfo b))))
                        (link-record-linkid b)))
     given-bindings)
    
    (begin0
      (map
       (λ (export)
         (define r
           (ormap 
            (λ (ctime-id)
              (define key (cons (link-record-tag export) ctime-id))
              (define used (hash-table-get used-binding-table key (λ () #f)))
              (when used
                (raise-stx-err "this export is supplied multiple times by the given unit" used))
              (let ([r (hash-table-get binding-table key (λ () #f))])
                (when r
                  (hash-table-put! used-binding-table key r))
                r))
            (siginfo-ctime-ids (link-record-siginfo export))))
         (make-link-record
          (link-record-tag export)
          (cond
            [r r]
            [else (car (generate-temporaries (list (link-record-linkid export))))])
          (link-record-sigid export)
          (link-record-siginfo export)))
       unit-exports)
      
      (hash-table-for-each
       binding-table
       (λ (k v)
         (unless (hash-table-get used-binding-table k (λ () #f))
           (raise-stx-err "this export is not supplied by the given unit" v))))))

  (define (name-form n) (syntax-object->datum n))
  
  ;; complete-imports : (hash-tableof symbol (or identifier 'duplicate))
  ;;                    (listof link-record)
  ;;                    (listof (list symbol identifier siginfo)) ->
  ;;                    (listof (cons symbol identifier))
  (define (complete-imports sig-table given-links unit-imports src)
    (define linked-sigs-table (make-hash-table 'equal))
    (for-each
     (λ (link)
       (define tag (link-record-tag link))
       (for-each
        (λ (cid)
          (define there? (hash-table-get linked-sigs-table (cons tag cid) (λ () #f)))
          (hash-table-put! linked-sigs-table (cons tag cid) (if there? 'duplicate #t)))
        (siginfo-ctime-ids (link-record-siginfo link))))
     given-links)
    
    (append
     given-links
     (let loop ([unit-imports unit-imports])
       (cond
         [(null? unit-imports) null]
         [else
          (let* ([import (car unit-imports)]
                 [ctime-ids (siginfo-ctime-ids (link-record-siginfo import))]
                 [tag (link-record-tag import)]
                 [there?
                  (hash-table-get linked-sigs-table
                                  (cons tag (car ctime-ids))
                                  (λ () #f))])
            (cond
              [(eq? 'duplicate there?)
               (raise-stx-err
                (if tag
                    (format "specified linkages satisfy (tag ~a ~a) import multiple times"
                            tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                    (format "specified linkages satisfy untagged ~a import multiple times"
                            (name-form (car (siginfo-names (link-record-siginfo import))))))
                src)]
              [there?
               (loop (cdr unit-imports))]
              [else
               (let ([there?2 (hash-table-get sig-table
                                              (car ctime-ids)
                                              (λ () #f))])
                 (cond
                   [(eq? 'duplicate there?2)
                    (raise-stx-err 
                     (if tag
                         (format "multiple linkages satisfy (tag ~a ~a) import"
                                 tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                         (format "multiple linkages satisfy untagged ~a import"
                                 (name-form (car (siginfo-names (link-record-siginfo import))))))
                     src)]
                   [there?2
                    (for-each
                     (λ (cid)
                       (hash-table-put! linked-sigs-table
                                        (cons tag cid)
                                        #t))
                     ctime-ids)
                    (cons (make-link-record (link-record-tag import)
                                            there?2
                                            (link-record-sigid import)
                                            (link-record-siginfo import))
                          (loop (cdr unit-imports)))]
                   [else
                    (raise-stx-err
                     (if tag
                         (format "no linkages satisfy (tag ~a ~a) import"
                                 tag (name-form (car (siginfo-names (link-record-siginfo import)))))
                         (format "no linkages satisfy untagged ~a import"
                                 (name-form (car (siginfo-names (link-record-siginfo import))))))
                     src)]))]))]))))

  (define (unprocess-link-record-bind lr)
    (if (link-record-tag lr)
        #`(#,(link-record-linkid lr) : (tag #,(link-record-tag lr) #,(link-record-sigid lr)))
        #`(#,(link-record-linkid lr) : #,(link-record-sigid lr))))
  
  (define (unprocess-link-record-use lr)
    (if (link-record-tag lr)
        #`(tag #,(link-record-tag lr) #,(link-record-linkid lr))
        (link-record-linkid lr)))

  (define (make-id-mappers . unbox-stxes)
    (apply values (map make-id-mapper unbox-stxes)))
  
  (define (make-id-mapper unbox-stx)
    (make-set!-transformer
     (lambda (sstx)
       (syntax-case sstx (set!)
         [x
          (identifier? #'x) 
          unbox-stx]
         [(set! . x)
          (raise-syntax-error
           'unit
           "cannot set! imported or exported variables"
           sstx)]
         [(_ . x)
          (datum->syntax-object
           sstx
           (cons unbox-stx #'x)
           sstx)])))))
