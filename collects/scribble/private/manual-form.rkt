#lang scheme/base
(require "../decode.rkt"
         "../struct.rkt"
         "../scheme.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         "qsloc.rkt"
         "manual-utils.rkt"
         "manual-vars.rkt"
         "manual-scheme.rkt"
         "manual-bind.rkt"
         scheme/list
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide defform defform* defform/subs defform*/subs defform/none
         defidform defidform/inline
         specform specform/subs
         specsubform specsubform/subs specspecsubform specspecsubform/subs
         specsubform/inline
         defsubform defsubform*
         racketgrammar racketgrammar*
         (rename-out [racketgrammar schemegrammar]
                     [racketgrammar* schemegrammar*])
         var svar)

(define-syntax (defform*/subs stx)
  (syntax-case stx ()
    [(_ #:kind kind #:id defined-id #:literals (lit ...) [spec spec1 ...]
        ([non-term-id non-term-form ...] ...)
        #:contracts ([contract-nonterm contract-expr] ...)
        desc ...)
     (with-syntax ([(defined-id defined-id-expr)
                    (if (identifier? #'defined-id)
                        (syntax [defined-id (quote-syntax defined-id)])
                        #'defined-id)])
       (with-syntax ([new-spec
                      (let loop ([spec #'spec])
                        (if (and (identifier? spec)
                                 (free-identifier=? spec #'defined-id))
                            (datum->syntax #'here '(unsyntax x) spec spec)
                            (syntax-case spec ()
                              [(a . b)
                               (datum->syntax spec
                                              (cons (loop #'a) (loop #'b))
                                              spec
                                              spec)]
                              [_ spec])))])
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error #f
                                           "expected an identifier for a literal"
                                           stx
                                           id)))
                   (syntax->list #'(lit ...)))
         #'(with-togetherable-racket-variables
            (lit ...)
            ([form [defined-id spec]] [form [defined-id spec1]] ...
             [non-term (non-term-id non-term-form ...)] ...)
            (*defforms kind defined-id-expr
                       '(spec spec1 ...)
                       (list (lambda (x) (racketblock0/form new-spec))
                             (lambda (ignored) (racketblock0/form spec1)) ...)
                       '((non-term-id non-term-form ...) ...)
                       (list (list (lambda () (racket non-term-id))
                                   (lambda () (racketblock0/form non-term-form))
                                   ...)
                             ...)
                       (list (list (lambda () (racket contract-nonterm))
                                   (lambda () (racketblock0 contract-expr)))
                             ...)
                       (lambda () (list desc ...))))))]
    [(fm #:id defined-id #:literals (lit ...) [spec spec1 ...]
         ([non-term-id non-term-form ...] ...)
         #:contracts ([contract-nonterm contract-expr] ...)
         desc ...)
     (syntax/loc stx
       (fm #:kind #f #:id defined-id #:literals (lit ...) [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ([contract-nonterm contract-expr] ...)
           desc ...))]
    [(fm #:id defined-id #:literals (lit ...) [spec spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (syntax/loc stx
       (fm #:id defined-id #:literals (lit ...) [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ()
           desc ...))]
    [(fm #:kind kind #:id defined-id #:literals (lit ...) [spec spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (syntax/loc stx
       (fm #:kind kind #:id defined-id #:literals (lit ...) [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ()
           desc ...))]
    [(fm #:id id [spec spec1 ...] ([non-term-id non-term-form ...] ...)
         desc ...)
     (syntax/loc stx
       (fm #:kind #f #:id id #:literals () [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ()
           desc ...))]
    [(fm #:kind kind #:literals lits [(spec-id . spec-rest) spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (with-syntax ([(_ _ _ _ _ [spec . _] . _) stx])
       (syntax/loc stx
         (fm #:kind kind #:id spec-id #:literals lits [spec spec1 ...]
             ([non-term-id non-term-form ...] ...)
             desc ...)))]
    [(fm #:literals lits [(spec-id . spec-rest) spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (with-syntax ([(_ _ _ [spec . _] . _) stx])
       (syntax/loc stx
         (fm #:kind #f #:id spec-id #:literals lits [spec spec1 ...]
             ([non-term-id non-term-form ...] ...)
             desc ...)))]
    [(fm #:kind kind [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
     (syntax/loc stx
       (fm #:kind kind #:literals () [spec spec1 ...] ([non-term-id non-term-form ...] ...)
           desc ...))]
    [(fm [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
     (syntax/loc stx
       (fm #:kind #f #:literals () [spec spec1 ...] ([non-term-id non-term-form ...] ...)
           desc ...))]))

(define-syntax (defform* stx)
  (syntax-case stx ()
    [(_ #:kind kind #:id id #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id #:literals lits [spec ...] () desc ...))]
    [(_ #:id id #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals lits [spec ...] () desc ...))]
    [(_ #:kind kind #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:literals lits [spec ...] () desc ...))]
    [(_ #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:literals lits [spec ...] () desc ...))]
    [(_ #:kind kind #:id id [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id [spec ...] () desc ...))]
    [(_ #:id id [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:id id [spec ...] () desc ...))]
    [(_ #:kind kind [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind [spec ...] () desc ...))]
    [(_ [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs [spec ...] () desc ...))]))

(define-syntax (defform stx)
  (syntax-case stx ()
    [(_ #:kind kind #:id id #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id #:literals (lit ...) [spec] () desc ...))]
    [(_ #:id id #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals (lit ...) [spec] () desc ...))]
    [(_ #:kind kind #:id id spec desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id #:literals () [spec] () desc ...))]
    [(_ #:id id spec desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals () [spec] () desc ...))]
    [(_ #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:literals (lit ...) [spec] () desc ...))]
    [(_ #:kind kind #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:literals (lit ...) [spec] () desc ...))]
    [(_ #:kind kind spec desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind [spec] () desc ...))]
    [(_ spec desc ...)
     (syntax/loc stx
       (defform*/subs [spec] () desc ...))]))

(define-syntax (defform/subs stx)
  (syntax-case stx ()
    [(_ #:kind kind #:id id #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id #:literals lits [spec] subs desc ...))]
    [(_ #:id id #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals lits [spec] subs desc ...))]
    [(_ #:kind kind #:id id spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:id id #:literals () [spec] subs desc ...))]
    [(_ #:id id spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals () [spec] subs desc ...))]
    [(_ #:kind kind #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind #:literals lits [spec] subs desc ...))]
    [(_ #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:literals lits [spec] subs desc ...))]
    [(_ #:kind kind spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:kind kind [spec] subs desc ...))]
    [(_ spec subs desc ...)
     (syntax/loc stx
       (defform*/subs [spec] subs desc ...))]))

(define-syntax (defform/none stx)
  (syntax-case stx ()
    [(_ #:kind kind #:literals (lit ...) spec #:contracts ([contract-id contract-expr] ...) desc ...)
     (begin
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error #f
                                         "expected an identifier for a literal"
                                         stx
                                         id)))
                 (syntax->list #'(lit ...)))
       #'(with-togetherable-racket-variables
          (lit ...)
          ([form/none spec])
          (*defforms kind #f
                     '(spec) (list (lambda (ignored) (racketblock0/form spec)))
                     null null
                     (list (list (lambda () (racket contract-id))
                                 (lambda () (racketblock0 contract-expr)))
                           ...)
                     (lambda () (list desc ...)))))]
    [(fm #:literals (lit ...) spec #:contracts ([contract-id contract-expr] ...) desc ...)
     (syntax/loc stx
       (fm #:kind #f #:literals (lit ...) spec #:contracts ([contract-id contract-expr] ...) desc ...))]
    [(fm #:kind kind #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (fm #:kind kind #:literals (lit ...) spec #:contracts () desc ...))]
    [(fm #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (fm #:literals (lit ...) spec #:contracts () desc ...))]
    [(fm #:kind kind spec desc ...)
     (syntax/loc stx
       (fm #:kind kind #:literals () spec desc ...))]
    [(fm spec desc ...)
     (syntax/loc stx
       (fm #:literals () spec desc ...))]))

(define-syntax (defidform/inline stx)
  (syntax-case stx (unsyntax)
    [(_ id)
     (identifier? #'id)
     #'(defform-site (quote-syntax id))]
    [(_ (unsyntax id-expr))
     #'(defform-site id-expr)]))

(define-syntax (defidform stx)
  (syntax-case stx ()
    [(_ #:kind kind spec-id desc ...)
     #'(with-togetherable-racket-variables
        ()
        ()
        (*defforms kind (quote-syntax/loc spec-id)
                   '(spec-id)
                   (list (lambda (x) (make-omitable-paragraph (list x))))
                   null
                   null
                   null
                   (lambda () (list desc ...))))]
    [(fm spec-id desc ...)
     (syntax/loc stx
       (fm #:kind #f spec-id desc ...))]))

(define (into-blockquote s)
  (make-blockquote "leftindent"
                   (if (splice? s)
                     (flow-paragraphs (decode-flow (splice-run s)))
                     (list s))))

(define-syntax (defsubform stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform . rest))]))

(define-syntax (defsubform* stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform* . rest))]))

(define-syntax spec?form/subs
  (syntax-rules ()
    [(_ has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        #:contracts ([contract-nonterm contract-expr] ...)
        desc ...)
     (with-racket-variables
      (lit ...)
      ([form/maybe (has-kw? spec)]
       [non-term (non-term-id non-term-form ...)] ...)
      (*specsubform 'spec '(lit ...) (lambda () (racketblock0/form spec))
                    '((non-term-id non-term-form ...) ...)
                    (list (list (lambda () (racket non-term-id))
                                (lambda () (racketblock0/form non-term-form))
                                ...)
                          ...)
                    (list (list (lambda () (racket contract-nonterm))
                                (lambda () (racketblock0 contract-expr)))
                          ...)
                    (lambda () (list desc ...))))]
    [(_ has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     #:contracts ()
                     desc ...)]))

(define-syntax specsubform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (spec?form/subs #f #:literals (lit ...) spec () desc ...)]
    [(_ spec desc ...)
     (specsubform #:literals () spec desc ...)]))

(define-syntax specsubform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs #f #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     desc ...)]
    [(_ spec subs desc ...)
     (specsubform/subs #:literals () spec subs desc ...)]))

(define-syntax-rule (specspecsubform spec desc ...)
  (make-blockquote "leftindent" (list (specsubform spec desc ...))))

(define-syntax-rule (specspecsubform/subs spec subs desc ...)
  (make-blockquote "leftindent" (list (specsubform/subs spec subs desc ...))))

(define-syntax specform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (spec?form/subs #t #:literals (lit ...) spec () desc ...)]
    [(_ spec desc ...)
     (specform #:literals () spec desc ...)]))

(define-syntax specform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs #t #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     desc ...)]
    [(_ spec ([non-term-id non-term-form ...] ...) desc ...)
     (specform/subs #:literals () spec ([non-term-id non-term-form ...] ...)
                    desc ...)]))

(define-syntax-rule (specsubform/inline spec desc ...)
  (with-racket-variables
   ()
   ([form/maybe (#f spec)])
   (*specsubform 'spec null #f null null null (lambda () (list desc ...)))))

(define-syntax racketgrammar
  (syntax-rules ()
    [(_ #:literals (lit ...) id clause ...)
     (with-racket-variables
      (lit ...)
      ([non-term (id clause ...)])
      (*racketgrammar '(lit ...)
                      '(id clause ...)
                      (lambda ()
                        (list (list (racket id)
                                    (racketblock0/form clause) ...)))))]
    [(_ id clause ...) (racketgrammar #:literals () id clause ...)]))

(define-syntax racketgrammar*
  (syntax-rules ()
    [(_ #:literals (lit ...) [id clause ...] ...)
     (with-racket-variables
      (lit ...)
      ([non-term (id clause ...)] ...)
      (*racketgrammar '(lit ...)
                      '(id ... clause ... ...)
                      (lambda ()
                        (list (list (racket id) (racketblock0/form clause) ...)
                              ...))))]
    [(_ [id clause ...] ...)
     (racketgrammar* #:literals () [id clause ...] ...)]))

(define-syntax-rule (var id)
  (*var 'id))

(define-syntax-rule (svar id)
  (*var 'id))


(define (meta-symbol? s) (memq s '(... ...+ ?)))

(define (defform-site kw-id)
  (let ([target-maker (id-to-form-target-maker kw-id #t)]
        [content (list (definition-site (syntax-e kw-id)
                         kw-id #t))])
    (if target-maker
        (target-maker
         content
         (lambda (tag)
           (make-toc-target-element
            #f
            (if kw-id
                (list (make-index-element
                       #f content tag
                       (list (datum-intern-literal (symbol->string (syntax-e kw-id))))
                       content
                       (with-exporting-libraries
                        (lambda (libs)
                          (make-form-index-desc (syntax-e kw-id)
                                                libs)))))
                content)
            tag)))
        (car content))))

(define (*defforms kind kw-id forms form-procs subs sub-procs contract-procs content-thunk)
  (parameterize ([current-meta-list '(... ...+)])
    (make-box-splice
     (cons
      (make-blockquote
       vertical-inset-style
       (list
        (make-table
         boxed-style
         (append
          (for/list ([form (in-list forms)]
                     [form-proc (in-list form-procs)]
                     [i (in-naturals)])
            (list
             ((if (zero? i) (add-background-label (or kind "syntax")) values)
              (list
               ((or form-proc
                    (lambda (x)
                      (make-omitable-paragraph
                       (list (to-element `(,x . ,(cdr form)))))))
                (and kw-id
                     (eq? form (car forms))
                     (defform-site kw-id)))))))
          (if (null? sub-procs)
              null
              (list (list flow-empty-line)
                    (list (make-flow
                           (list (let ([l (map (lambda (sub)
                                                 (map (lambda (f) (f)) sub))
                                               sub-procs)])
                                   (*racketrawgrammars "specgrammar"
                                                       (map car l)
                                                       (map cdr l))))))))
          (make-contracts-table contract-procs)))))
      (content-thunk)))))

(define (*specsubform form lits form-thunk subs sub-procs contract-procs content-thunk)
  (parameterize ([current-meta-list '(... ...+)])
    (make-blockquote
     "leftindent"
     (cons
      (make-blockquote
       vertical-inset-style
       (list
        (make-table
         boxed-style
         (cons
          (list
           (make-flow
            (list
             (if form-thunk
                 (form-thunk)
                 (make-omitable-paragraph (list (to-element form)))))))
          (append
           (if (null? sub-procs)
               null
               (list (list flow-empty-line)
                     (list (make-flow
                            (list (let ([l (map (lambda (sub)
                                                  (map (lambda (f) (f)) sub))
                                                sub-procs)])
                                    (*racketrawgrammars "specgrammar"
                                                        (map car l)
                                                        (map cdr l))))))))
           (make-contracts-table contract-procs))))))
      (flow-paragraphs (decode-flow (content-thunk)))))))

(define (*racketrawgrammars style nonterms clauseses)
  (make-table
   `((valignment baseline baseline baseline baseline baseline)
     (alignment right left center left left)
     (style ,style))
   (cdr
    (append-map
     (lambda (nonterm clauses)
       (list*
        (list flow-empty-line flow-empty-line flow-empty-line
              flow-empty-line flow-empty-line)
        (list (to-flow nonterm) flow-empty-line (to-flow "=") flow-empty-line
              (make-flow (list (car clauses))))
        (map (lambda (clause)
               (list flow-empty-line flow-empty-line
                     (to-flow "|") flow-empty-line
                     (make-flow (list clause))))
             (cdr clauses))))
     nonterms clauseses))))

(define (*racketrawgrammar style nonterm clause1 . clauses)
  (*racketrawgrammars style (list nonterm) (list (cons clause1 clauses))))

(define (*racketgrammar lits s-expr clauseses-thunk)
  (let ([l (clauseses-thunk)])
    (*racketrawgrammars #f
                        (map (lambda (x)
                               (make-element #f
                                             (list (hspace 2)
                                                   (car x))))
                             l)
                        (map cdr l))))

(define (*var id)
  (to-element (*var-sym id)))

(define (*var-sym id)
  (string->symbol (format "_~a" id)))

(define (make-contracts-table contract-procs)
  (if (null? contract-procs)
      null
      (append
       (list (list flow-empty-line))
       (list (list (make-flow
                    (map (lambda (c)
                           (make-table
                            "argcontract"
                            (list
                             (list (to-flow (hspace 2))
                                   (to-flow ((car c)))
                                   flow-spacer
                                   (to-flow ":")
                                   flow-spacer
                                   (make-flow (list ((cadr c))))))))
                         contract-procs)))))))
