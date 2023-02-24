#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse/pre
                     syntax/parse/private/pattern-expander)
         racket/syntax
         syntax/name
         syntax/parse/pre
         syntax/parse/experimental/template
         syntax/stx)

(provide raise-stx-err
         check-id
         checked-syntax->list

         set!-trans-extract
         lookup
         static/extract
         make-relative-introducer
         add-inferred-name

         split-requires
         split-requires*

         attribute-map
         attributes-map
         (for-syntax empty-id?
                     dotted-id
                     attr-decl)
         ~bind/nested
         with-loc
         with-loc+name
         with-inferred-name)

;; -----------------------------------------------------------------------------
;; error messages

(define (raise-stx-err msg [stx #f])
  (raise-syntax-error #f msg (current-syntax-context) stx))

;; check-id: syntax-object -> identifier
(define (check-id id)
  (unless (identifier? id)
    (raise-stx-err "not an identifier" id))
  id)

;; checked-syntax->list : syntax-object -> (listof syntax-object)
(define (checked-syntax->list s)
  (define l (syntax->list s))
  (unless (or (stx-pair? s) (stx-null? s))
    (raise-stx-err "bad syntax (not a list)" s))
  (unless l
    (raise-stx-err "bad syntax (illegal use of `.')" s))
  l)

;; -----------------------------------------------------------------------------

(define (set!-trans-extract x)
  (if (set!-transformer? x)
      (set!-transformer-procedure x)
      x))

(define (lookup id err-msg)
  (check-id id)
  (set!-trans-extract
   (syntax-local-value
    (syntax-local-introduce id)
    (lambda ()
      (raise-stx-err err-msg id)))))

(define-syntax-class (static/extract predicate description)
  #:description description
  #:attributes [value]
  #:commit
  (pattern {~var x (static values #f)}
    #:attr value (set!-trans-extract (attribute x.value))
    #:fail-unless (predicate (attribute value)) #f))

;; make-relative-introducer : syntax? syntax? -> (syntax -> syntax)
(define (make-relative-introducer ref-stx orig-stx)
  (lambda (stx)
    ((make-syntax-delta-introducer stx orig-stx)
     (datum->syntax ref-stx
                    (syntax-e stx)
                    stx
                    stx))))

;; add-inferred-name : syntax? symbol? -> syntax?
(define (add-inferred-name stx name)
  ;; Don’t overwrite an existing 'inferred-name property, since that
  ;; represents a “more local” name.
  (if (syntax-property stx 'inferred-name)
      stx
      (syntax-property stx 'inferred-name name)))

;; -----------------------------------------------------------------------------
;; splitting requires

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

;; -----------------------------------------------------------------------------
;; syntax/parse helpers

;; Maps over a the value of a syntax/parse attribute with the given
;; ellipsis depth. Any `#f` values in the spine are always left
;; untouched, but if `skip-false?` is `#t`, then `f` is applied to
;; `#f` values in the leaves (since those may represent legitimate
;; attribute values rather than placeholders representing the
;; absence of a value).
(define (attribute-map f depth val #:skip-false? [skip-false? #t])
  (let loop ([val val]
             [depth depth])
    (if (zero? depth)
        (if (and skip-false? (not val))
            #f
            (f val))
        (and val (for/list ([val (in-list val)])
                   (loop val (sub1 depth)))))))

;; Like `(attribute-map .... #:skip-false? #f)`, but n-ary like `map`.
(define (attributes-map f depth . vals)
  (let loop ([vals vals]
             [depth depth])
    (if (zero? depth)
        (apply f vals)
        (and (andmap values vals)
             (apply map (λ vals (loop vals (sub1 depth))) vals)))))

(begin-for-syntax
  (define (empty-id? id)
    (eq? (syntax-e id) '||))

  (define (dotted-id base-id sub-id)
    ;; Behave like `~var` when the base identifier is the empty
    ;; identifier: don’t do any prefixing, but still alter the
    ;; result’s lexical context.
    (if (empty-id? base-id)
        (datum->syntax base-id (syntax-e sub-id) base-id base-id)
        (format-id base-id "~a.~a" base-id sub-id #:subs? #t)))

  (define-syntax-class attr-decl
    #:attributes [id depth]
    #:commit
    (pattern id:id
      #:attr depth #'0)
    (pattern {id:id depth:nat})))

;; Like `~bind`, but binds all the attributes as nested attributes of
;; a given base attribute. Expects usage of the following form:
;;
;;   {~bind/nested [<base-attr-decl> <base-expr>]
;;                 <tmp-id>
;;                 ([<sub-attr-decl> <sub-expr>] ...)}
;;
;; <base-attr-decl> is bound to the result of <base-expr>. Then, for
;; each provided <sub-attr-decl>, a nested attribute with the name
;; `<base-attr-decl.id>.<sub-attr-decl.id>` is bound to the result
;; of evaluating the corresponding <sub-expr> once per non-#f (leaf)
;; value of <base-attr-decl> with <tmp-id> bound to the value.
;;
;; As a special case, if <base-attr-decl.id> is the empty identifier,
;; <base-expr> is not bound to any attribute, and the sub-attributes
;; are bound without any prefix. (This mirrors the behavior of `~var`.)
(define-syntax ~bind/nested
  (pattern-expander
   (λ (stx)
     (define-syntax-class (nested-clause base-id base-depth base-tmp-id tmp-id)
       #:attributes [bind-clause]
       (pattern [nested-attr:attr-decl nested-e:expr]
         #:attr bind-clause
         (quasisyntax/loc this-syntax
           [{#,(dotted-id base-id #'nested-attr.id)
             #,(+ base-depth (syntax-e #'nested-attr.depth))}
            #,(quasisyntax/loc #'nested-e
                (attribute-map #,(quasisyntax/loc #'nested-e
                                   (λ (#,tmp-id) nested-e))
                               #,base-depth
                               #,base-tmp-id))])))

     (syntax-parse stx
       [(_ {~optional {~and #:only-nested only-nested?}}
           [base-attr:attr-decl base-e:expr]
           tmp-id:id
           ({~var nested (nested-clause #'base-attr.id
                                        (syntax-e #'base-attr.depth)
                                        #'base-tmp
                                        #'tmp-id)}
            ...))

        #`{~and {~do (define base-tmp base-e)}
                {~bind #,@(if (or (attribute only-nested?)
                                  (empty-id? #'base-attr.id))
                              '()
                              (list #'[base-attr base-tmp]))
                       nested.bind-clause ...}}]))))

(define-template-metafunction with-loc
  (syntax-parser
    [(_ src body)
     (datum->syntax #'body (syntax-e #'body) #'src #'body)]))

(define-template-metafunction with-loc+name
  (syntax-parser
    [(_ src body)
     (add-inferred-name
      (datum->syntax #'body (syntax-e #'body) #'src #'body)
      (syntax-local-infer-name #'src))]))

(define-template-metafunction with-inferred-name
  (syntax-parser
    [(_ name:id body)
     (add-inferred-name #'body (syntax-e #'name))]))
