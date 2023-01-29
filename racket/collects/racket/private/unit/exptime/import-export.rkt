#lang racket/base

;; This module implements parsing and processing of signature imports
;; and exports.

(require racket/list
         syntax/id-table
         syntax/parse
         syntax/parse/experimental/specialize
         syntax/transformer
         "signature.rkt"
         "util.rkt"
         (for-template racket/base
                       racket/contract/base
                       "../keywords.rkt"))

(provide signature-ie-names
         signature-ie-int-names
         signature-ie-ext-names
         map-signature-ie

         import-spec
         export-spec
         tagged-import-spec
         tagged-export-spec

         check-duplicate-subs
         check-duplicate-sigs
         check-unit-ie-sigs
         build-val+macro-defs
         build-post-val-defs+ctcs
         get-member-bindings)

;; -----------------------------------------------------------------------------
;; signature-ie

;; Note [Parsed signature imports and exports]
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; When we parse a signature import or export in a unit body, we
;; return a `signature?` value, but we adjust the shape of some of
;; its fields:
;;
;;     int/ext? = (cons/c identifier? identifier?)
;;     vars     : (listof int/ext?)
;;     val-defs : (listof (listof (cons/c (listof int/ext?) syntax?))
;;     stx-defs : (listof (listof (cons/c (listof int/ext?) syntax?))
;;
;; These split `int/ext?` identifier pairs track both the internal and
;; external name of each binding, since they may be renamed in the
;; unit body using the `rename` or `prefix` forms. Additionally, we
;; adjust the scopes on some of the fields.
;;
;; In comments, we refer to these modified signature structures using
;; the pseudo-contract `signature-ie?` instead of just `signature?` to
;; avoid potential confusion.

;; lookup-signature-ie : identifier? syntax? -> signature-ie?
;; See Note [Parsed signature imports and exports].
(define (lookup-signature-ie sig-id
                             #:bind-lctx bind-lctx
                             #:add-prefix add-prefix)
  (define sig (lookup-signature sig-id))
  (define delta-introduce (make-relative-introducer bind-lctx
                                                    (car (siginfo-names (signature-siginfo sig)))))

  (define (split-id id)
    (cons id id))
  (define (split-def def)
    (cons (map split-id (car def)) (cdr def)))

  (map-signature-ie
   (lambda (id) (add-prefix (delta-introduce id)))
   syntax-local-introduce
   (make-signature
    (signature-siginfo sig)
    (map split-id (signature-vars sig))
    (map split-def (signature-val-defs sig))
    (map split-def (signature-stx-defs sig))
    (signature-post-val-defs sig)
    (signature-ctcs sig)
    (signature-orig-binder sig))))

(define (signature-ie-names sig)
  (append (signature-vars sig)
          (apply append (map car (signature-val-defs sig)))
          (apply append (map car (signature-stx-defs sig)))))

(define (signature-ie-int-names sig)
  (map car (signature-ie-names sig)))
(define (signature-ie-ext-names sig)
  (map cdr (signature-ie-names sig)))

;; map-signature-ie : (identifier? -> identifier?) (syntax? -> syntax?) signature-ie? -> signature-ie?
;; applies f to the internal parts, and g to the external parts.
(define (map-signature-ie f g sig)
  (define (map-def f g def)
    (cons (map (lambda (x)
                 (cons (f (car x)) (g (cdr x))))
               (car def))
          (g (cdr def))))

  (make-signature
   (signature-siginfo sig)
   (map (lambda (x) (cons (f (car x)) (g (cdr x)))) (signature-vars sig))
   (map (lambda (x) (map-def f g x)) (signature-val-defs sig))
   (map (lambda (x) (map-def f g x)) (signature-stx-defs sig))
   (map (lambda (x) (cons (map f (car x)) (g (cdr x)))) (signature-post-val-defs sig))
   (map (lambda (x) (if x (g x) x)) (signature-ctcs sig))
   (signature-orig-binder sig)))

;; -----------------------------------------------------------------------------
;; parsing imports and exports

;; check-bound-id-subset : (listof syntax-object) (listof identifier) syntax-object -> 
;; ensures each element of i1 is an identifier bound-identifier=? to an identifier in i2
(define (check-bound-id-subset i1 i2)
  (define ht (make-bound-id-table))
  (for ([id (in-list i2)])
    (bound-id-table-set! ht id #t))
  (for ([id (in-list i1)])
    (check-id id)
    (unless (bound-id-table-ref ht id #f)
      (raise-stx-err "listed identifier not present in signature specification" id))))

;; do-rename : sig syntax-object syntax-object -> sig
;; internals and externals must both be of the form (x ...)
;; ensures that each x above is an identifier
(define (do-rename sig internals externals)
  (check-bound-id-subset (syntax->list externals)
                         (signature-ie-int-names sig))

  (define ht (make-bound-id-table))
  (for ([int (in-list (syntax->list internals))]
        [ext (in-list (syntax->list externals))])
    (check-id int)
    (when (bound-id-table-ref ht ext #f)
      (raise-stx-err "duplicate renamings" ext))
    (bound-id-table-set! ht ext int))

  (map-signature-ie
   (lambda (id)
     (bound-id-table-ref ht id id))
   (lambda (x) x)
   sig))

;; do-prefix : id id -> id
;; ensures that pid is an identifier
(define (do-prefix stx pid)
  (if (identifier? stx)
      (datum->syntax
       stx
       (string->symbol (format "~a~a" (syntax-e pid) (syntax-e stx)))
       stx)
      stx))

;; do-only/except : sig (listof identifier) -> sig
;; ensures that only-ids are identifiers and are mentioned in the signature
(define (do-only/except sig only/except-ids put get)
  (check-bound-id-subset only/except-ids
                         (signature-ie-int-names sig))

  (define ht (make-bound-id-table))
  (for ([id (in-list only/except-ids)])
    (bound-id-table-set! ht id (put id)))

  (map-signature-ie
   (lambda (id)
     (bound-id-table-ref ht id (λ () (get id))))
   (lambda (x) x)
   sig))

(define (add-prefixes add-prefix l)
  (map add-prefix (syntax->list l)))

(define-syntax-class (import/export import?
                                    #:bind-lctx bind-lctx
                                    #:add-prefix add-prefix)
  #:description (format "~a spec" (if import? "import" "export"))
  #:attributes [sig-id value]
  #:commit
  #:literals [only except prefix rename bind-at]

  (pattern sig-id:id
    #:attr value (lookup-signature-ie #'sig-id
                                      #:bind-lctx (or bind-lctx #'sig-id)
                                      #:add-prefix add-prefix))

  (pattern (bind-at ~! bind-lctx {~var || (import/export import?
                                                         #:bind-lctx #'bind-lctx
                                                         #:add-prefix add-prefix)}))

  (pattern (only ~! sub-spec id:id ...)
    #:declare sub-spec (import/export import?
                                      #:bind-lctx bind-lctx
                                      #:add-prefix add-prefix)
    #:fail-unless import? "bad export-spec keyword"
    #:attr sig-id (attribute sub-spec.sig-id)
    #:attr value (do-only/except (attribute sub-spec.value)
                                 (add-prefixes add-prefix #'(id ...))
                                 (lambda (id) id)
                                 (lambda (id)
                                   (car (generate-temporaries #`(#,id))))))

  (pattern (except ~! sub-spec id:id ...)
    #:declare sub-spec (import/export import?
                                      #:bind-lctx bind-lctx
                                      #:add-prefix add-prefix)
    #:fail-unless import? "bad export-spec keyword"
    #:attr sig-id (attribute sub-spec.sig-id)
    #:attr value (do-only/except (attribute sub-spec.value)
                                 (add-prefixes add-prefix #'(id ...))
                                 (lambda (id)
                                   (car (generate-temporaries #`(#,id))))
                                 (lambda (id) id)))

  (pattern (prefix ~! pid:id
                   {~var || (import/export import?
                                           #:bind-lctx bind-lctx
                                           #:add-prefix (λ (id) (add-prefix (do-prefix id #'pid))))}))

  (pattern (rename ~! sub-spec [internal:id external:id] ...)
    #:declare sub-spec (import/export import? #:bind-lctx bind-lctx #:add-prefix add-prefix)
    #:attr sig-id (attribute sub-spec.sig-id)
    #:attr value (do-rename (attribute sub-spec.value)
                            #'(internal ...)
                            (datum->syntax #f (add-prefixes add-prefix #'(external ...))))
    #:fail-when (check-duplicate-identifier (signature-ie-int-names (attribute value)))
                "rename created duplicate identifier"))

;; Defined as a wrapper around `import/export` so the clauses of `import/export`
;; don’t have to bother with binding all the attributes.
(define-syntax-class (import/export* import?
                                     #:bind-lctx bind-lctx
                                     #:add-prefix add-prefix)
  #:description #f
  #:attributes [sig-id value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var.int-id 1} {var.ext-id 1}
                {val-def.int-id 2} {val-def.ext-id 2} {val-def.rhs 1}
                {stx-def.int-id 2} {stx-def.ext-id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}]
  #:commit
  (pattern {~var || (import/export import?
                                   #:bind-lctx bind-lctx
                                   #:add-prefix add-prefix)}
    #:and {~bind-signature-ie || (attribute value)}))

(define-syntax-class (tagged-import/export import? #:bind-lctx bind-lctx)
  #:description (format "tagged ~a spec" (if import? "import" "export"))
  #:attributes [tag-id tag-sym sig-id tagged-sig-id value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var.int-id 1} {var.ext-id 1}
                {val-def.int-id 2} {val-def.ext-id 2} {val-def.rhs 1}
                {stx-def.int-id 2} {stx-def.ext-id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}
                {key 1} self-key {super-key 1}]
  #:commit
  #:literals [bind-at tag]
  (pattern (bind-at ~! bind-lctx {~var || (tagged-import/export import? #:bind-lctx #'bind-lctx)}))

  (pattern (tag ~! tag-id:id {~var || (import/export* import?
                                                      #:bind-lctx bind-lctx
                                                      #:add-prefix values)})
    #:attr tag-sym (syntax-e #'tag-id)
    #:attr tagged-sig-id (syntax/loc this-syntax
                           (tag tag-id sig-id))
    #:and {~bind-keys || (attribute tag-sym) (attribute info)})

  (pattern {~var || (import/export* import?
                                    #:bind-lctx bind-lctx
                                    #:add-prefix values)}
    #:attr tag-id #f
    #:attr tag-sym #f
    #:attr tagged-sig-id #'sig-id
    #:and {~bind-keys || (attribute tag-sym) (attribute info)}))

;; Used by `provide-signature-elements`.
(define-syntax-class/specialize import-spec
  (import/export* #t #:bind-lctx #f #:add-prefix values))
;; Used by `open`.
(define-syntax-class/specialize export-spec
  (import/export* #f #:bind-lctx #f #:add-prefix values))

(define-syntax-class/specialize tagged-import-spec
  (tagged-import/export #t #:bind-lctx #f))
(define-syntax-class/specialize tagged-export-spec
  (tagged-import/export #f #:bind-lctx #f))

;; -----------------------------------------------------------------------------
;; helpers

;; check-duplicate-subs : (listof (cons/c symbol? siginfo?)) (listof syntax?) -> void?
(define (check-duplicate-subs tagged-siginfos sources)
  (for ([tinfo1 (in-list tagged-siginfos)]
        [s1 (in-list sources)])
    (for ([tinfo2 (in-list tagged-siginfos)]
          [s2 (in-list sources)])
      (unless (eq? tinfo1 tinfo2)
        (when (and (eq? (car tinfo1) (car tinfo2))
                   (siginfo-subtype (cdr tinfo1) (cdr tinfo2)))
          (raise-stx-err (format "the signature of ~a extends this signature"
                                 (syntax->datum s1))
                         s2))))))

;; check-duplicate-sigs : (listof (cons symbol siginfo)) (listof syntax-object)
;;                        (listof (cons symbol siginfo)) (listof syntax-object) -> void?
(define (check-duplicate-sigs tagged-siginfos sources tagged-deps dsources)
  (define import-idx (make-hash))

  (for ([tinfo (in-list tagged-siginfos)]
        [s (in-list sources)])
    (define key (cons (car tinfo)
                      (car (siginfo-ctime-ids (cdr tinfo)))))
    (when (hash-ref import-idx key #f)
      (raise-stx-err "duplicate import signature" s))
    (hash-set! import-idx key #t))

  (for ([dep (in-list tagged-deps)]
        [s (in-list dsources)])
    (unless (hash-ref import-idx
                      (cons (car dep)
                            (car (siginfo-ctime-ids (cdr dep))))
                      #f)
      (raise-stx-err "initialization dependency on unknown import" s))))
  
(define (check-unit-ie-sigs import-sigs export-sigs)
  (define import-names (append-map signature-ie-int-names import-sigs))
  (define export-names (append-map signature-ie-int-names export-sigs))

  (let ([dup (check-duplicate-identifier import-names)])
    (when dup
      (raise-stx-err
       (format "~a is imported by multiple signatures" (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier export-names)])
    (when dup
      (raise-stx-err (format "~a is exported by multiple signatures"
                             (syntax-e dup)))))
  
  (let ([dup (check-duplicate-identifier (append import-names export-names))])
    (when dup
      (raise-stx-err (format "import ~a is exported" (syntax-e dup))))))

;; build-val+macro-defs : (syntax? -> syntax?)
;;                     -> (signature-ie? -> (list/c syntax? syntax? syntax?))
(define ((build-val+macro-defs intro) sig)
  (cond
    [(and (null? (signature-val-defs sig))
          (null? (signature-stx-defs sig)))
     ;; No renames needed; this shortcut avoids
     ;; an explosion of renamings, especially with chains
     ;; of `open':
     (list #'(() (values)) #'() #'())]
    [else
     ;; Renames and macros needed:
     (define intro* (make-syntax-introducer))
     (define introduced-sig (map-signature-ie values (λ (id) (intro (intro* id))) sig))
     (define/syntax-parse ([int-ivar . ext-ivar] ...) (signature-vars introduced-sig))
     (define/syntax-parse ([([int-vid . ext-vid] ...) . vbody] ...) (signature-val-defs introduced-sig))
     (define/syntax-parse ([([int-sid . ext-sid] ...) . sbody] ...) (signature-stx-defs introduced-sig))
     (list #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
              (make-rename-transformers
               (quote-syntax
                (int-ivar ...
                 int-vid ... ...
                 int-sid ... ...))))
           #'(((int-sid ...) sbody) ...)
           #'(((int-vid ...) vbody) ...))]))

;; build-post-val-defs+ctcs : signature-ie? -> (list/c stx-list? (listof syntax?))
(define (build-post-val-defs+ctcs sig)
  (define introduced-sig (map-signature-ie values
                                           (make-syntax-introducer)
                                           sig))
  (define/syntax-parse ([int-ivar . ext-ivar] ...) (signature-vars introduced-sig))
  (define/syntax-parse ([([int-vid . ext-vid] ...) . _] ...) (signature-val-defs introduced-sig))
  (define/syntax-parse ([([int-sid . ext-sid] ...) . _] ...) (signature-stx-defs introduced-sig))
  (define/syntax-parse [post-rhs ...] (map cdr (signature-post-val-defs introduced-sig)))

  (define/syntax-parse post-renames
    #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
       (make-rename-transformers
        (quote-syntax
         (int-ivar ...
          int-vid ... ...
          int-sid ... ...)))))

  (list
   #'((let-syntaxes (post-renames) post-rhs) ...)
   (for/list ([ctc (in-list (signature-ctcs introduced-sig))])
     ; Absence of a contract is represented by #f, but a contract can be present
     ; and be #'f, so we have to be extra careful not to coerce to syntax objects
     ; too early. After wrapping in (contract . _), converting to syntax is okay.
     (and ctc (cons 'contract #`(let-syntaxes (post-renames) #,ctc))))))

;; Using `make-rename-transformers' helps improve sharing in
;; a syntax-quoted list of identifiers, although it risks
;; losting certificates as the list is broken apart; since the
;; identifiers are bound at the same point that the rename
;; transformer is introduced, certificate loss should be ok.
(define (make-rename-transformers ids)
  (apply values
         (map 
          make-rename-transformer
          (syntax->list ids))))

;; get-member-bindings : bound-id-table? signature-ie? syntax? any/c -> (listof syntax?)
(define (get-member-bindings member-table sig pos bind?)
  (for/list ([int-id (in-list (map car (signature-vars sig)))]
             [ext-id (in-list (map cdr (signature-vars sig)))]
             [ctc (in-list (signature-ctcs sig))])

    (define (add-ctc name-id val-expr)
      (if ctc
          (with-syntax ([ctc* (syntax-property ctc 'inferred-name name-id)])
            #`(let ([v/c (#,val-expr)])
                (contract ctc* (car v/c) (cdr v/c) #,pos
                          (quote #,name-id) (quote-syntax #,name-id))))
          #`(#,val-expr)))

    #`[#,(if bind? ext-id int-id)
       (make-variable-like-transformer
        (quote-syntax #,(add-ctc int-id (bound-id-table-ref member-table int-id))))]))
