#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         racket/syntax
         syntax/name
         syntax/parse
         syntax/parse/experimental/template
         syntax/stx)

(provide raise-stx-err
         check-id
         checked-syntax->list

         set!-trans-extract
         lookup
         static/extract
         make-relative-introducer

         split-requires
         split-requires*

         (for-syntax empty-id?
                     dotted-id)
         ~bind/nested
         with-loc
         with-loc+name)

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

(define-template-metafunction with-loc
  (syntax-parser
    [(_ src body)
     (datum->syntax #'body (syntax-e #'body) #'src #'body)]))

(define-template-metafunction with-loc+name
  (syntax-parser
    [(_ src body)
     (syntax-property
      (datum->syntax #'body (syntax-e #'body) #'src #'body)
      'inferred-name
      (syntax-local-infer-name #'src))]))
