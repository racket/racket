#lang racket/base

(require racket/syntax
         syntax/boundmap
         syntax/parse
         syntax/parse/experimental/specialize
         "unit-syntax.rkt")
(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))
(require (for-template racket/base
                       "unit-keywords.rkt"
                       "unit-runtime.rkt"))

(provide (struct-out var-info)
         (struct-out signature)
         (struct-out signature-form)
         (struct-out unit-info)
         (struct-out link-record)

         (rename-out [build-siginfo make-siginfo])
         siginfo-names siginfo-ctime-ids siginfo-rtime-ids siginfo-subtype
         unprocess-link-record-bind unprocess-link-record-use
         set!-trans-extract
         lookup-signature lookup-def-unit make-id-mapper make-id-mappers signature-ie-names signature-ie-int-names signature-ie-ext-names
         map-signature-ie split-requires split-requires* apply-mac complete-exports complete-imports check-duplicate-subs
         make-relative-introducer
         (for-template bind-at)
         build-init-depend-property

         build-key siginfo->key-expr siginfo->key-exprs
         signature-id tagged-signature-id
         import-spec export-spec tagged-import-spec tagged-export-spec)

(define-syntax (apply-mac stx)
  (syntax-case stx ()
    ((_ f x) ((syntax-e #'f) #'x))))

;; split-requires* : (listof identifier) -> (listof syntax) -> (values (listof syntax) (listof syntax))
;; Parameterized over identifiers for require forms.
(define ((split-requires* req-forms) l)
  (let loop ((l l)
             (requires null))
    (cond
      ((null? l) (cons (reverse requires) l))
      (else
       (syntax-case (car l) ()
         ((r . x)
          (ormap (lambda (req) (free-identifier=? #'r req))
                 req-forms)
          (loop (cdr l) (cons (car l) requires)))
         (_
          (cons (reverse requires) l)))))))

;; split-requires : (listof syntax) -> (values (listof syntax) (listof syntax))
;; Recognizes mzscheme require forms.
(define split-requires
  (split-requires*
   (list #'require #'require-for-syntax #'require-for-template)))

;; (make-var-info bool bool identifier (U #f syntax-object))
(define-struct var-info (syntax? [exported? #:mutable] id [ctc #:mutable]))

(define-syntax define-struct/proc
  (syntax-rules ()
    ((_ name (field ...) p)
     (define-struct name (field ...) #:property prop:procedure p))))

(begin-for-syntax
  (define (empty-id? id)
    (eq? (syntax-e id) '||))

  (define (dotted-id base-id sub-id)
    ;; Behave like `~var` when the base identifier is the empty
    ;; identifier: don’t do any prefixing, but still alter the
    ;; result’s lexical context.
    (if (empty-id? base-id)
        (datum->syntax base-id (syntax-e sub-id) base-id base-id)
        (format-id base-id "~a.~a" base-id sub-id #:subs? #t))))

;; Like `~bind`, but binds all the attributes as nested attributes of
;; a given base identifier.
(define-syntax ~bind/nested
  (pattern-expander
   (λ (stx)
     (define-syntax-class attr-decl
       #:attributes [id depth]
       #:commit
       (pattern id:id
         #:attr depth #'0)
       (pattern {id:id depth:nat}))

     (syntax-parse stx
       [(_ base-id:id [attr-d:attr-decl attr-e:expr] ...)
        (define/syntax-parse [attr-id ...]
          (for/list ([sub-id (in-list (attribute attr-d.id))])
            (dotted-id #'base-id sub-id)))
        #'{~bind [{attr-id attr-d.depth} attr-e] ...}]))))

;; An int/ext is
;; - (cons identifier identifier)
;; A def is
;; - (listof (cons (listof int/ext) syntax-object))
;; A ctc is
;; - syntax-object
;; - #f
;; A sig is
;; - (list (listof int/ext) (listof def) (listof def) (listof ctc))
;; A tagged-sig is 
;; - (listof (cons #f siginfo) (cons #f identifier) sig)
;; - (listof (cons symbol siginfo) (cons symbol identifier) sig)

;; A siginfo contains information about the identity of a signature
;; and its super-signatures. Each of the three list fields are always
;; non-empty and the same length; the first element of each list
;; corresponds to the child signature, and each subsequent element
;; corresponds to the next super-signature in the inheritance chain.
(define-struct siginfo
  (names         ; (listof identifier?) - the identifiers bound by `define-signature`
   ctime-ids     ; (listof symbol?) - gensyms that uniquely identify the signature
                 ;   in the transformer environment
   rtime-ids     ; (listof identifier?) - identifiers bound to a gensym that
                 ;   uniquely identifies the signature at runtime; see
                 ;   Note [Signature runtime representation] in "unit-runtime.rkt"
   super-table)) ; (hash/c symbol? #t) - a hash that maps the elements of ctime-ids,
                 ;   to #t, used for efficient subtyping checks

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
                (make-immutable-hasheq (map (λ (x) `(,x . #t)) ctime-ids))))

;; siginfo-subtype : siginfo siginfo -> bool
(define (siginfo-subtype s1 s2)
  (hash-ref (siginfo-super-table s1)
            (car (siginfo-ctime-ids s2))
            (λ () #f)))

;; The compile-time value of a signature binding.
;; Note that a slightly modified variant of this structure is
;; sometimes used when processing imports and exports in a unit body,
;; see Note [Parsed signature imports and exports] for details.
(define-struct/proc signature
  (siginfo       ; siginfo?
   vars          ; (listof identifier?)
   val-defs      ; (listof (cons/c (listof identifier?) syntax?))
   stx-defs      ; (listof (cons/c (listof identifier?) syntax?))
   post-val-defs ; (listof (cons/c (listof identifier?) syntax?))
   ctcs          ; (listof (or/c syntax? #f))
   orig-binder)  ; identifier?
  (lambda (_ stx)
    (parameterize ((current-syntax-context stx))
      (raise-stx-err "illegal use of signature name"))))

;; (make-signature-form (syntax-object -> any))
(define-struct/proc signature-form (f)
  (lambda (_ stx)
    (parameterize ((current-syntax-context stx))
      (raise-stx-err "illegal use of signature form"))))

;; (make-unit-info identifier (listof (cons symbol identifier)) (listof (cons symbol identifier)) identifier boolean)
(define-struct/proc unit-info (unit-id import-sig-ids export-sig-ids deps orig-binder contracted?)
  (lambda (struct stx) 
    (with-syntax ((u (syntax-local-introduce (unit-info-unit-id struct))))
      (syntax-case stx (set!)
        ((set! x y)
         (if (unit-info-contracted? struct)
             (raise-syntax-error 'set!
                                 "cannot set! a contracted unit"
                                 stx
                                 (syntax x))
             #`(begin 
                 #,(syntax/loc #'y (check-unit y 'set!))
                 #,(syntax/loc #'y (check-sigs y (unit-import-sigs u) (unit-export-sigs u) 'set!))
                 (set! u y))))
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

;; build-key : (or/c symbol? #f) identifier? -> signature-key?
(define (build-key tag i)
  (if tag
      #`(cons '#,tag #,i)
      i))

;; siginfo->key-expr : siginfo? (or/c symbol? #f) -> syntax?
;; Builds an expression that evaluates to this signature’s runtime key;
;; see Note [Signature runtime representation] in "unit-runtime.rkt".
(define (siginfo->key-expr info tag)
  (build-key tag (car (siginfo-rtime-ids info))))

;; siginfo->key-exprs : siginfo? (or/c symbol? #f) -> (listof syntax?)
;; Builds a list of expressions that evaluate to runtime keys for this
;; signature and each of its super-signatures; see Note [Signature
;; runtime representation] in "unit-runtime.rkt".
(define (siginfo->key-exprs info tag)
  (map (λ (id) (build-key tag id)) (siginfo-rtime-ids info)))

;; Helper for bulk-binding attributes for siginfo values.
(define-syntax ~bind-siginfo
  (pattern-expander
   (syntax-parser
     [(_ x:id e:expr)
      #`{~and
         {~do (define tmp e)}
         #,@(if (empty-id? #'x) '()
                (list #'{~bind [x tmp]}))
         {~bind/nested
          x
          [{id 1} (siginfo-names tmp)]
          [self-id (car (siginfo-names tmp))]
          [{super-id 1} (cdr (siginfo-names tmp))]
          [{ctime-id 1} (siginfo-ctime-ids tmp)]
          [{rtime-id 1} (siginfo-rtime-ids tmp)]}}])))

(define (get-int-ids defs)
  (map (λ (def) (map car (car def))) defs))
(define (get-ext-ids defs)
  (map (λ (def) (map car (car def))) defs))

;; Helpers for bulk-binding attributes for signature and signature-ie values.
(define-syntaxes [~bind-signature ~bind-signature-ie]
  (let ()
    (define (make ie?)
      (pattern-expander
       (syntax-parser
         [(_ x:id e:expr)
          #:with x-info (dotted-id #'x #'info)
          #`{~and
             {~do (define tmp e)}
             #,@(if (empty-id? #'x) '()
                    (list #'{~bind [x tmp]}))
             {~bind/nested
              x
              [{post-def.id 2} (map car (signature-post-val-defs tmp))]
              [{post-def.rhs 1} (map cdr (signature-post-val-defs tmp))]
              [{ctc 1} (signature-ctcs tmp)]
              #,@(if ie?

                     #'([{var.int-id 1} (map car (signature-vars tmp))]
                        [{var.ext-id 1} (map cdr (signature-vars tmp))]
                        [{val-def.int-id 2} (get-int-ids (signature-val-defs tmp))]
                        [{val-def.ext-id 2} (get-ext-ids (signature-val-defs tmp))]
                        [{val-def.rhs 1} (map cdr (signature-val-defs tmp))]
                        [{stx-def.int-id 2} (get-int-ids (signature-stx-defs tmp))]
                        [{stx-def.ext-id 2} (get-ext-ids (signature-stx-defs tmp))]
                        [{stx-def.rhs 1} (map cdr (signature-stx-defs tmp))])

                     #'([{var-id 1} (signature-vars tmp)]
                        [{val-def.id 2} (map car (signature-val-defs tmp))]
                        [{val-def.rhs 1} (map cdr (signature-val-defs tmp))]
                        [{stx-def.id 2} (map car (signature-stx-defs tmp))]
                        [{stx-def.rhs 1} (map cdr (signature-stx-defs tmp))]))}

             {~bind-siginfo #,(dotted-id #'x #'info) (signature-siginfo tmp)}}])))

    (values (make #f) (make #t))))

;; Helper for bulk-binding attributes for signature key expressions.
(define-syntax ~bind-keys
  (pattern-expander
   (syntax-parser
     [(_ x:id tag-e:expr info-e:expr)
      #`{~and
         {~do (define keys (siginfo->key-exprs info-e tag-e))}
         {~bind/nested
          x
          [{key 1} keys]
          [self-key (car keys)]
          [{super-key 1} (cdr keys)]}}])))

(define-syntax-class (static/extract predicate description)
  #:description description
  #:attributes [value]
  #:commit
  (pattern {~var x (static values #f)}
    #:attr value (set!-trans-extract (attribute x.value))
    #:fail-unless (predicate (attribute value)) #f))

(define-syntax-class signature-id
  #:description #f
  #:attributes [value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var-id 1}
                {val-def.id 2} {val-def.rhs 1}
                {stx-def.id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}]
  #:commit
  (pattern {~var || (static/extract signature? "identifier bound to a signature")}
    #:and {~bind-signature || (attribute value)}))

(define-syntax-class tagged-signature-id
  #:description "tagged signature identifier"
  #:attributes [tag-id tag-sym sig-id value info
                {info.id 1} info.self-id {info.super-id 1}
                {info.ctime-id 1}
                {info.rtime-id 1}
                {var-id 1}
                {val-def.id 2} {val-def.rhs 1}
                {stx-def.id 2} {stx-def.rhs 1}
                {post-def.id 2} {post-def.rhs 1}
                {ctc 1}
                {key 1} self-key {super-key 1}]
  #:commit
  #:literals [tag]
  (pattern (tag ~! tag-id:id {~and sig-id :signature-id})
    #:attr tag-sym (syntax-e #'tag-id)
    #:and {~bind-keys || (attribute tag-sym) (attribute info)})
  (pattern {~and sig-id :signature-id}
    #:attr tag-id #f
    #:attr tag-sym #f
    #:and {~bind-keys || (attribute tag-sym) (attribute info)}))

(define (set!-trans-extract x)
  (if (set!-transformer? x)
      (set!-transformer-procedure x)
      x))

(define (lookup-def-unit id)
  (let ((u (lookup id "unknown unit definition")))
    (unless (unit-info? u)
      (raise-stx-err "not a unit definition" id))
    u))

;; check-bound-id-subset : (listof syntax-object) (listof identifier) syntax-object -> 
;; ensures each element of i1 is an identifier bound-identifier=? to an identifier in i2
(define (check-bound-id-subset i1 i2)
  (let ((ht (make-bound-identifier-mapping)))
    (for-each (lambda (id)
                (bound-identifier-mapping-put! ht id #t))
              i2)
    (for-each
     (lambda (id)
       (check-id id)
       (unless (bound-identifier-mapping-get ht id (lambda () #f))
         (raise-stx-err "listed identifier not present in signature specification" id)))
     i1)))

;; do-rename : sig syntax-object syntax-object -> sig
;; internals and externals must both be of the form (x ...)
;; ensures that each x above is an identifier
(define (do-rename sig internals externals)
  (check-bound-id-subset (syntax->list externals)
                         (signature-ie-int-names sig))
  (let ((ht (make-bound-identifier-mapping)))
    (for-each
     (lambda (int ext)
       (check-id int)
       (when (bound-identifier-mapping-get ht ext (lambda () #f))
         (raise-stx-err "duplicate renamings" ext))
       (bound-identifier-mapping-put! ht ext int))
     (syntax->list internals)
     (syntax->list externals))
    (map-signature-ie
     (lambda (id)
       (bound-identifier-mapping-get ht id (lambda () id)))
     (lambda (x) x)
     sig)))

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
  (let ((ht (make-bound-identifier-mapping)))
    (for-each (lambda (id)
                (bound-identifier-mapping-put! ht id (put id)))
              only/except-ids)
    (map-signature-ie
     (lambda (id)
       (bound-identifier-mapping-get ht id
                                     (lambda () 
                                       (get id))))
     (lambda (x) x)
     sig)))

(define (make-relative-introducer ref-id orig-id)
  (lambda (id)
    ((make-syntax-delta-introducer id orig-id)
     (datum->syntax ref-id
                    (syntax-e id)
                    id
                    id))))

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

(define (signature-ie->list sig)
  (list (signature-vars sig)
        (signature-val-defs sig)
        (signature-stx-defs sig)
        (signature-ctcs sig)
        (signature-post-val-defs sig)))

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

;; process-import/export : syntax-object syntax-object (box (cons identifier) siginfo) -> sig
(define (process-import/export spec spec-bind bind? add-prefix)
  (syntax-case spec (only except prefix rename bind-at)
    (_
     (identifier? spec)
     (lookup-signature-ie spec #:bind-lctx (or spec-bind spec) #:add-prefix add-prefix))
    ((bind-at spec-bind spec)
     (process-import/export #'spec #'spec-bind bind? add-prefix))
    ((only sub-spec id ...)
     (do-only/except (process-import/export #'sub-spec spec-bind bind? add-prefix)
                     (add-prefixes add-prefix #'(id ...))
                     (lambda (id) id)
                     (lambda (id)
                       (car (generate-temporaries #`(#,id))))))
    ((except sub-spec id ...)
     (do-only/except (process-import/export #'sub-spec spec-bind bind? add-prefix)
                     (add-prefixes add-prefix #'(id ...))
                     (lambda (id)
                       (car (generate-temporaries #`(#,id))))
                     (lambda (id) id)))
    ((prefix pid sub-spec)
     (process-import/export #'sub-spec spec-bind bind?
                            (lambda (id)
                              (add-prefix (do-prefix id #'pid)))))
    ((rename sub-spec (internal external) ...)
     (let* ((sig-res
             (do-rename (process-import/export #'sub-spec spec-bind bind? add-prefix)
                        #'(internal ...)
                        (datum->syntax #f (add-prefixes add-prefix #'(external ...)))))
            (dup (check-duplicate-identifier (signature-ie-int-names sig-res))))
       (when dup
         (raise-stx-err
          (format "rename created duplicate identifier ~a" (syntax-e dup))
          spec))
       sig-res))))

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
                                   (syntax->datum s1))
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
  (define binding-table (make-hash))
  (define used-binding-table (make-hash))
  
  (check-duplicate-subs 
   (map (λ (ts) (cons (link-record-tag ts) (link-record-siginfo ts))) given-bindings)
   (map link-record-sigid given-bindings))
  
  (for-each 
   (λ (b)
     (hash-set! binding-table 
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
            (define used (hash-ref used-binding-table key (λ () #f)))
            (when used
              (raise-stx-err "this export is supplied multiple times by the given unit" used))
            (let ([r (hash-ref binding-table key (λ () #f))])
              (when r
                (hash-set! used-binding-table key r))
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
    
    (hash-for-each
     binding-table
     (λ (k v)
       (unless (hash-ref used-binding-table k (λ () #f))
         (raise-stx-err "this export is not supplied by the given unit" v))))))

(define (name-form n) (syntax->datum n))

;; complete-imports : (hash-tableof symbol (or identifier 'duplicate))
;;                    (listof link-record)
;;                    (listof (list symbol identifier siginfo)) ->
;;                    (listof (cons symbol identifier))
(define (complete-imports sig-table given-links unit-imports src)
  (define linked-sigs-table (make-hash))
  (for-each
   (λ (link)
     (define tag (link-record-tag link))
     (for-each
      (λ (cid)
        (define there? (hash-ref linked-sigs-table (cons tag cid) (λ () #f)))
        (hash-set! linked-sigs-table (cons tag cid) (if there? 'duplicate #t)))
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
                (hash-ref linked-sigs-table
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
             (let ([there?2 (hash-ref sig-table
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
                     (hash-set! linked-sigs-table
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

(define (make-id-mappers . make-unbox-stxes)
  (apply values (map make-id-mapper make-unbox-stxes)))

(define (make-id-mapper make-unbox-stx)
  (make-set!-transformer
   (lambda (sstx)
     (syntax-case sstx (set!)
       [x
        (identifier? #'x) 
        (make-unbox-stx sstx)]
       [(set! . x)
        (raise-syntax-error
         'unit
         "cannot set! imported or exported variables"
         sstx)]
       [(_ . x)
        (datum->syntax
         sstx
         (cons (make-unbox-stx sstx) #'x)
         sstx)]))))

;; This utility function returns a list of natural numbers for use as a syntax
;; property needed to support units in Typed Racket
;; Each number in the list is an index into a unit's list of imports signifying
;; that the import at that index is also an init-dependency of the unit
(define (build-init-depend-property init-depends imports)
  (define (sig=? s1 s2)
    (and (eq? (syntax-e (car s1)) (car s2))
         (free-identifier=? (cdr s1) (cdr s2))))
  (for/list ([import (in-list imports)]
             [index (in-naturals)]
             #:when (member import init-depends sig=?))
    index))
