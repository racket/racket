#lang racket/base

;; This module implements parsing and processing of signature imports
;; and exports.

(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/parse/private/pattern-expander)
         racket/list
         racket/syntax
         syntax/private/id-table
         syntax/parse/pre
         syntax/parse/experimental/specialize
         syntax/transformer
         "signature.rkt"
         "util.rkt"
         (for-template racket/base
                       racket/contract/base
                       "../keywords.rkt"))

(provide (struct-out signature-ie)
         signature-ie-siginfo
         signature-ie-tag-sym
         signature-ie-names
         signature-ie-int-names
         signature-ie-ext-names
         map-signature-ie
         signature-ie->tagged-sig-id
         ~bind-signature-ie

         import-spec ; Used by `provide-signature-elements`.
         export-spec ; Used by `open`.
         tagged-ie-spec
         tagged-import-spec
         tagged-export-spec

         rename-ie-spec

         check-duplicate-subs
         check-duplicate-sigs
         check-unit-ie-sigs
         build-val+macro-defs
         build-post-val-defs+ctcs)

;; -----------------------------------------------------------------------------
;; signature-ie

#| Note [Parsed signature imports and exports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we parse a signature import or export in a unit body, we return a
`signature-ie?` value. The value contains a reference to the original
`signature?` binding it imports or exports, but most of the signature
binding’s fields are also included in the `signature-ie?` value, in a
slightly modified form:

  * Fields of `signature-ie?` have adjusted scopes based on the lexical
    context of the signature identifier in the import/export spec.

  * Fields containing imports come in a slightly different shape,
    with each binding identifier split into an `int/ext?` pair:

      int/ext? = (cons/c identifier? identifier?)
      vars     : (listof int/ext?)
      val-defs : (listof (listof (cons/c (listof int/ext?) syntax?))
      stx-defs : (listof (listof (cons/c (listof int/ext?) syntax?))

    These split identifier pairs track both the internal and external
    name of each binding, since they may be renamed in the unit body
    using the `rename`, `prefix`, `only`, or `except` forms.

The parsing of import/export specs themselves is handled by the
`ie-spec` syntax class, which binds a `value` attribute containing the
parsed `signature-ie?` value. |#

;; Represents a parsed (possibly tagged) import or export spec.
;; See Note [Parsed signature imports and exports] for details.
(struct signature-ie
  (src-stx       ; syntax? - the original import/export spec, for error reporting
   import?       ; boolean? - #t if this is an import, #f if it’s an export
   sig           ; signature?
   sig-id        ; identifier?
   tag-id        ; (or/c identifier? #f)
   vars          ; (listof int/ext?)
   val-defs      ; (listof (cons/c (listof int/ext?) syntax?))
   stx-defs      ; (listof (cons/c (listof int/ext?) syntax?))
   post-val-defs ; (listof (cons/c (listof identifier?) syntax?))
   ctcs)         ; (listof (or/c syntax? #f))
  #:transparent)

;; signature-ie-siginfo : signature-ie? -> siginfo?
(define (signature-ie-siginfo sig)
  (signature-siginfo (signature-ie-sig sig)))

;; signature-ie-tag-sym : signature-ie? -> (or/c symbol? #f)
(define (signature-ie-tag-sym sig)
  (and (signature-ie-tag-id sig)
       (syntax-e (signature-ie-tag-id sig))))

;; lookup-signature-ie : identifier? syntax? -> signature-ie?
;; See Note [Parsed signature imports and exports].
(define (signature->signature-ie sig sig-id
                                 #:import? import?
                                 #:bind-lctx bind-lctx
                                 #:add-prefix add-prefix)
  (define delta-introduce (make-signature-member-introducer sig bind-lctx))
  (define (split-id id)
    (cons id id))
  (define (split-def def)
    (cons (map split-id (car def)) (cdr def)))

  (map-signature-ie
   (lambda (id) (add-prefix (delta-introduce id)))
   syntax-local-introduce
   (signature-ie
    sig-id
    import?
    sig
    sig-id
    #f
    (map split-id (signature-vars sig))
    (map split-def (signature-val-defs sig))
    (map split-def (signature-stx-defs sig))
    (signature-post-val-defs sig)
    (signature-ctcs sig))))

(define (signature-ie-names sig)
  (append (signature-ie-vars sig)
          (apply append (map car (signature-ie-val-defs sig)))
          (apply append (map car (signature-ie-stx-defs sig)))))

(define (signature-ie-int-names sig)
  (map car (signature-ie-names sig)))
(define (signature-ie-ext-names sig)
  (map cdr (signature-ie-names sig)))

;; map-signature-ie : (identifier? -> identifier?) (syntax? -> syntax?) signature-ie? -> signature-ie?
;; applies f to the internal parts, and g to the external parts.
(define (map-signature-ie f g sig)
  (define (map-def def)
    (cons (map (lambda (x)
                 (cons (f (car x)) (g (cdr x))))
               (car def))
          (g (cdr def))))

  (struct-copy
   signature-ie sig
   [vars          (map (lambda (x) (cons (f (car x)) (g (cdr x)))) (signature-ie-vars sig))]
   [val-defs      (map map-def (signature-ie-val-defs sig))]
   [stx-defs      (map map-def (signature-ie-stx-defs sig))]
   [post-val-defs (map (lambda (x) (cons (map f (car x)) (g (cdr x)))) (signature-ie-post-val-defs sig))]
   [ctcs          (map (lambda (x) (if x (g x) x)) (signature-ie-ctcs sig))]))

;; signature-ie->tagged-sig-id : signature-ie? -> syntax?
(define (signature-ie->tagged-sig-id sig)
  (if (signature-ie-tag-id sig)
      (quasisyntax/loc (signature-ie-src-stx sig)
        (tag #,(signature-ie-tag-id sig) #,(signature-ie-sig-id sig)))
      (signature-ie-sig-id sig)))

(define (get-int-ids defs)
  (map (λ (def) (map car (car def))) defs))
(define (get-ext-ids defs)
  (map (λ (def) (map car (car def))) defs))

;; Helpers for bulk-binding attributes for signature-ie values.
(define-syntax ~bind-signature-ie
  (pattern-expander
   (syntax-parser
     [(_ x:attr-decl e:expr)
      #`{~and
         {~do (define tmp e)}
         {~bind/nested
          [x tmp] tmp
          ([src-stx (signature-ie-src-stx tmp)]
           [sig (signature-ie-sig tmp)]
           [sig-id (signature-ie-sig-id tmp)]
           [tag-id (signature-ie-tag-id tmp)]
           [tag-sym (signature-ie-tag-sym tmp)]
           [tagged-sig-id (signature-ie->tagged-sig-id tmp)]
           [{var.int-id 1} (map car (signature-ie-vars tmp))]
           [{var.ext-id 1} (map cdr (signature-ie-vars tmp))]
           [{val-def.int-id 2} (get-int-ids (signature-ie-val-defs tmp))]
           [{val-def.ext-id 2} (get-ext-ids (signature-ie-val-defs tmp))]
           [{val-def.rhs 1} (map cdr (signature-ie-val-defs tmp))]
           [{stx-def.int-id 2} (get-int-ids (signature-ie-stx-defs tmp))]
           [{stx-def.ext-id 2} (get-ext-ids (signature-ie-stx-defs tmp))]
           [{stx-def.rhs 1} (map cdr (signature-ie-stx-defs tmp))]
           [{post-def.id 2} (map car (signature-ie-post-val-defs tmp))]
           [{post-def.rhs 1} (map cdr (signature-ie-post-val-defs tmp))]
           [{ctc 1} (signature-ie-ctcs tmp)])}

         {~bind-siginfo {#,(dotted-id #'x.id #'info) x.depth}
                        (attribute-map signature-ie-siginfo 'x.depth tmp)}
         {~bind-keys x
                     (attribute-map signature-ie-siginfo 'x.depth tmp)
                     (attribute-map signature-ie-tag-sym 'x.depth tmp)}}])))

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

(define-syntax-class (ie-spec* import?
                               #:bind-lctx bind-lctx
                               #:add-prefix add-prefix)
  #:description (format "~a spec" (if import? "import" "export"))
  #:attributes [value]
  #:commit
  #:literals [only except prefix rename bind-at]

  (pattern sig-id:signature-id
    #:attr value (signature->signature-ie (attribute sig-id.value)
                                          #'sig-id
                                          #:import? import?
                                          #:bind-lctx (or bind-lctx #'sig-id)
                                          #:add-prefix add-prefix))

  (pattern (bind-at ~! bind-lctx {~var || (ie-spec* import?
                                                    #:bind-lctx #'bind-lctx
                                                    #:add-prefix add-prefix)}))

  (pattern (only ~! sub-spec id:id ...)
    #:declare sub-spec (ie-spec* import?
                                 #:bind-lctx bind-lctx
                                 #:add-prefix add-prefix)
    #:fail-unless import? "bad export-spec keyword"
    #:attr value (do-only/except (attribute sub-spec.value)
                                 (add-prefixes add-prefix #'(id ...))
                                 (lambda (id) id)
                                 (lambda (id)
                                   (car (generate-temporaries #`(#,id))))))

  (pattern (except ~! sub-spec id:id ...)
    #:declare sub-spec (ie-spec* import?
                                 #:bind-lctx bind-lctx
                                 #:add-prefix add-prefix)
    #:fail-unless import? "bad export-spec keyword"
    #:attr value (do-only/except (attribute sub-spec.value)
                                 (add-prefixes add-prefix #'(id ...))
                                 (lambda (id)
                                   (car (generate-temporaries #`(#,id))))
                                 (lambda (id) id)))

  (pattern (prefix ~! pid:id
                   {~var || (ie-spec* import?
                                      #:bind-lctx bind-lctx
                                      #:add-prefix (λ (id) (add-prefix (do-prefix id #'pid))))}))

  (pattern (rename ~! sub-spec [internal:id external:id] ...)
    #:declare sub-spec (ie-spec* import? #:bind-lctx bind-lctx #:add-prefix add-prefix)
    #:attr value (do-rename (attribute sub-spec.value)
                            #'(internal ...)
                            (datum->syntax #f (add-prefixes add-prefix #'(external ...))))
    #:fail-when (check-duplicate-identifier (signature-ie-int-names (attribute value)))
    "rename created duplicate identifier"))

;; Defined as a wrapper around `ie-spec*` so the clauses of `ie-spec*`
;; don’t have to bother with binding all the attributes.
(define-syntax-class (ie-spec import? #:bind-lctx [bind-lctx #f])
  #:description #f
  #:attributes [value sig sig-id info
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
  (pattern {~var spec (ie-spec* import?
                                #:bind-lctx bind-lctx
                                #:add-prefix values)}
    #:attr value (struct-copy signature-ie (attribute spec.value) [src-stx #'spec])
    #:and {~bind-signature-ie || (attribute value)}))

(define-syntax-class (tagged-ie-spec* import? #:bind-lctx bind-lctx)
  #:description (format "tagged ~a spec" (if import? "import" "export"))
  #:attributes [value]
  #:commit
  #:literals [bind-at tag]
  (pattern (bind-at ~! bind-lctx {~var || (tagged-ie-spec* import? #:bind-lctx #'bind-lctx)}))
  (pattern (tag ~! tag-id:id {~var spec (ie-spec import? #:bind-lctx bind-lctx)})
    #:attr value (struct-copy signature-ie (attribute spec.value) [tag-id #'tag-id]))
  (pattern {~var || (ie-spec import? #:bind-lctx bind-lctx)}))

;; Defined as a wrapper around `tagged-ie-spec*` so the clauses of `tagged-ie-spec*`
;; don’t have to bother with binding all the attributes.
(define-syntax-class (tagged-ie-spec import?)
  #:description #f
  #:attributes [value sig sig-id tag-id tag-sym tagged-sig-id info
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
  (pattern {~var spec (tagged-ie-spec* import? #:bind-lctx #f)}
    #:attr value (struct-copy signature-ie (attribute spec.value) [src-stx #'spec])
    #:and {~bind-signature-ie || (attribute value)}))

(define-syntax-class/specialize import-spec
  (ie-spec #t))
(define-syntax-class/specialize export-spec
  (ie-spec #f))

(define-syntax-class/specialize tagged-import-spec
  (tagged-ie-spec #t))
(define-syntax-class/specialize tagged-export-spec
  (tagged-ie-spec #f))

;; -----------------------------------------------------------------------------
;; helpers

;; rename-ie-spec : syntax? (listof (cons/c identifier? identifier?)) -> syntax?
;; Adds a new `rename` wrapper around the given import or export spec,
;; pushing it underneath any `tag` or `bind-at` wrappers.
(define (rename-ie-spec spec-stx renamings)
  (if (empty? renamings)
      spec-stx
      (let loop ([spec-stx spec-stx])
        (syntax-parse spec-stx
          #:context 'rename-ie-spec
          #:literals [tag bind-at]
          [(head:tag ~! tag-id spec)
           (datum->syntax spec-stx (list #'head #'tag-id (loop #'spec)) spec-stx spec-stx)]
          [(head:bind-at ~! lctx spec)
           (datum->syntax spec-stx (list #'head #'lctx (loop #'spec)) spec-stx spec-stx)]
          [spec
           #`(rename spec #,@(for/list ([renaming (in-list renamings)])
                               #`[#,(car renaming) #,(cdr renaming)]))]))))

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
    [(and (null? (signature-ie-val-defs sig))
          (null? (signature-ie-stx-defs sig)))
     ;; No renames needed; this shortcut avoids
     ;; an explosion of renamings, especially with chains
     ;; of `open':
     (list #'(() (values)) #'() #'())]
    [else
     ;; Renames and macros needed:
     (define intro* (make-syntax-introducer))
     (define introduced-sig (map-signature-ie values (λ (id) (intro (intro* id))) sig))
     (define/syntax-parse ([int-ivar . ext-ivar] ...) (signature-ie-vars introduced-sig))
     (define/syntax-parse ([([int-vid . ext-vid] ...) . vbody] ...) (signature-ie-val-defs introduced-sig))
     (define/syntax-parse ([([int-sid . ext-sid] ...) . sbody] ...) (signature-ie-stx-defs introduced-sig))
     (list #'((ext-ivar ... ext-vid ... ... ext-sid ... ...)
              (make-rename-transformers
               (quote-syntax
                (int-ivar ...
                 int-vid ... ...
                 int-sid ... ...))))
           #'(((int-sid ...) sbody) ...)
           #'(((int-vid ...) vbody) ...))]))

;; build-post-val-defs+ctcs : signature-ie? -> (values (listof syntax?) (listof (or/c syntax? #f)))
(define (build-post-val-defs+ctcs sig)
  (define introduced-sig (map-signature-ie values
                                           (make-syntax-introducer)
                                           sig))
  (define/syntax-parse ([int-ivar . ext-ivar] ...) (signature-ie-vars introduced-sig))
  (define/syntax-parse ([([int-vid . ext-vid] ...) . _] ...) (signature-ie-val-defs introduced-sig))
  (define/syntax-parse ([([int-sid . ext-sid] ...) . _] ...) (signature-ie-stx-defs introduced-sig))
  (define/syntax-parse post-renames
    #'[(ext-ivar ... ext-vid ... ... ext-sid ... ...)
       (make-rename-transformers
        (quote-syntax
         (int-ivar ...
          int-vid ... ...
          int-sid ... ...)))])

  (values (for/list ([post-def (in-list (signature-ie-post-val-defs introduced-sig))])
            #`(let-syntaxes (post-renames) #,(cdr post-def)))
          (for/list ([ctc (in-list (signature-ie-ctcs introduced-sig))])
            (and ctc #`(let-syntaxes (post-renames) #,ctc)))))

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
