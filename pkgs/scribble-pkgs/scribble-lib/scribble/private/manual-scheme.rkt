#lang racket/base
(require "../decode.rkt"
         "../struct.rkt"
         "../scheme.rkt"
         "../search.rkt"
         "../basic.rkt"
         (only-in "../core.rkt" style style-properties)
         "manual-style.rkt"
         "manual-utils.rkt" ;; used via datum->syntax
         "on-demand.rkt"
         (for-syntax racket/base)
         (for-label racket/base))

(provide racketblock RACKETBLOCK racketblock/form
         racketblock0 RACKETBLOCK0 racketblock0/form
         racketresultblock racketresultblock0
         RACKETRESULTBLOCK RACKETRESULTBLOCK0
         racketblockelem
         racketinput RACKETINPUT
         racketinput0 RACKETINPUT0
         racketmod
         racketmod0
         racket RACKET racket/form racketresult racketid 
         racketmodname
         racketmodlink indexed-racket
         racketlink
         
         (rename-out [racketblock schemeblock]
                     [RACKETBLOCK SCHEMEBLOCK]
                     [racketblock/form schemeblock/form]
                     [racketblock0 schemeblock0]
                     [RACKETBLOCK0 SCHEMEBLOCK0]
                     [racketblock0/form schemeblock0/form]
                     [racketblockelem schemeblockelem]
                     [racketinput schemeinput]
                     [racketmod schememod]
                     [racket scheme]
                     [RACKET SCHEME]
                     [racket/form scheme/form]
                     [racketresult schemeresult]
                     [racketid schemeid]
                     [racketmodname schememodname]
                     [racketmodlink schememodlink]
                     [indexed-racket indexed-scheme]
                     [racketlink schemelink]))

(define-code racketblock0 to-paragraph)
(define-code racketblock to-block-paragraph)
(define-code RACKETBLOCK to-block-paragraph UNSYNTAX)
(define-code RACKETBLOCK0 to-paragraph UNSYNTAX)

(define (to-block-paragraph v)
  (code-inset (to-paragraph v)))

(define (to-result-paragraph v)
  (to-paragraph v 
                #:color? #f 
                #:wrap-elem
                (lambda (e) (make-element result-color e))))
(define (to-result-paragraph/prefix a b c)
  (let ([to-paragraph (to-paragraph/prefix a b c)])
    (lambda (v)
      (to-paragraph v 
                    #:color? #f 
                    #:wrap-elem
                    (lambda (e) (make-element result-color e))))))

(define-code racketresultblock0 to-result-paragraph)
(define-code racketresultblock (to-result-paragraph/prefix (hspace 2) (hspace 2) ""))
(define-code RACKETRESULTBLOCK (to-result-paragraph/prefix (hspace 2) (hspace 2) "")
  UNSYNTAX)
(define-code RACKETRESULTBLOCK0 to-result-paragraph UNSYNTAX)

(define interaction-prompt (make-element 'tt (list "> " )))
(define-code racketinput to-input-paragraph/inset)
(define-code RACKETINPUT to-input-paragraph/inset)
(define-code racketinput0 to-input-paragraph)
(define-code RACKETINPUT0 to-input-paragraph)

(define to-input-paragraph
  (to-paragraph/prefix
   (make-element #f interaction-prompt)
   (hspace 2)
   ""))
  
(define to-input-paragraph/inset
  (lambda (v)
    (code-inset (to-input-paragraph v))))

(define-syntax (racketmod0 stx)
  (syntax-case stx ()
    [(_ #:file filename #:escape unsyntax-id lang rest ...)
     (with-syntax ([modtag (datum->syntax
                            #'here
                            `(unsyntax (make-element
                                        #f
                                        (list (hash-lang)
                                              spacer
                                              ,(if (identifier? #'lang)
                                                   `(as-modname-link
                                                     ',#'lang
                                                     (to-element ',#'lang)
                                                     #f)
                                                   #'(racket lang)))))
                            #'lang)])
       (if (syntax-e #'filename)
           (quasisyntax/loc stx
             (filebox
              filename
              #,(syntax/loc stx (racketblock0 #:escape unsyntax-id modtag rest ...))))
           (syntax/loc stx (racketblock0 #:escape unsyntax-id modtag rest ...))))]
    [(_ #:file filename lang rest ...)
     (syntax/loc stx (racketmod0 #:file filename #:escape unsyntax lang rest ...))]
    [(_ lang rest ...)
     (syntax/loc stx (racketmod0 #:file #f lang rest ...))]))

(define-syntax-rule (racketmod rest ...)
  (code-inset (racketmod0 rest ...)))

(define (to-element/result s)
  (make-element result-color (list (to-element/no-color s))))
(define (to-element/id s)
  (make-element symbol-color (list (to-element/no-color s))))

(define-syntax (keep-s-expr stx)
  (syntax-case stx (quote)
    [(_ ctx '#t #(src line col pos 5))
     #'(make-long-boolean #t)]
    [(_ ctx '#f #(src line col pos 6))
     #'(make-long-boolean #f)]
    [(_ ctx s srcloc)
     (let ([sv (syntax-e
                (syntax-case #'s (quote)
                  [(quote s) #'s]
                  [_ #'s]))])
       (if (or (number? sv)
               (boolean? sv)
               (and (pair? sv)
                    (identifier? (car sv))
                    (or (free-identifier=? #'cons (car sv))
                        (free-identifier=? #'list (car sv)))))
           ;; We know that the context is irrelvant
           #'s
           ;; Context may be relevant:
           #'(*keep-s-expr s ctx)))]))
(define (*keep-s-expr s ctx)
  (if (symbol? s)
    (make-just-context s ctx)
    s))

(define (add-sq-prop s name val)
  (if (eq? name 'paren-shape)
    (make-shaped-parens s val)
    s))

(define-code racketblockelem to-element)

(define-code racket to-element unsyntax keep-s-expr add-sq-prop)
(define-code RACKET to-element UNSYNTAX keep-s-expr add-sq-prop)
(define-code racketresult to-element/result unsyntax keep-s-expr add-sq-prop)
(define-code racketid to-element/id unsyntax keep-s-expr add-sq-prop)
(define-code *racketmodname to-element unsyntax keep-s-expr add-sq-prop)

(define-syntax (**racketmodname stx)
  (syntax-case stx ()
    [(_ form)
     (let ([stx #'form])
       #`(*racketmodname
          ;; We want to remove lexical context from identifiers
          ;; that correspond to module names, but keep context
          ;; for `lib' or `planet' (which are rarely used)
          #,(if (identifier? stx)
                (datum->syntax #f (syntax-e stx) stx stx)
                (if (and (pair? (syntax-e stx))
                         (memq (syntax-e (car (syntax-e stx))) '(lib planet file)))
                    (let ([s (car (syntax-e stx))]
                          [rest (let loop ([a (cdr (syntax-e stx))] [head? #f])
                                  (cond
                                   [(identifier? a) (datum->syntax #f (syntax-e a) a a)]
                                   [(and head? (pair? a) (and (identifier? (car a))
                                                              (free-identifier=? #'unsyntax (car a))))
                                    a]
                                   [(pair? a) (cons (loop (car a) #t) 
                                                    (loop (cdr a) #f))]
                                   [(syntax? a) (datum->syntax a
                                                               (loop (syntax-e a) head?)
                                                               a 
                                                               a)]
                                   [else a]))])
                      (datum->syntax stx (cons s rest) stx stx))
                    stx))))]))

(define-syntax racketmodname
  (syntax-rules (unsyntax)
    [(racketmodname #,n)
     (let ([sym n])
       (as-modname-link sym (to-element sym) #f))]
    [(racketmodname n)
     (as-modname-link 'n (**racketmodname n) #f)]
    [(racketmodname #,n #:indirect)
     (let ([sym n])
       (as-modname-link sym (to-element sym) #t))]
    [(racketmodname n #:indirect)
     (as-modname-link 'n (**racketmodname n) #t)]))

(define-syntax racketmodlink
  (syntax-rules (unsyntax)
    [(racketmodlink n content ...)
     (*as-modname-link 'n (elem #:style #f content ...) #f)]))

(define (as-modname-link s e indirect?)
  (if (symbol? s)
      (*as-modname-link s e indirect?)
      e))

(define-on-demand indirect-module-link-color
  (struct-copy style module-link-color
               [properties (cons 'indirect-link
                                 (style-properties module-link-color))]))

(define (*as-modname-link s e indirect?)
  (make-link-element (if indirect?
                         indirect-module-link-color
                         module-link-color)
                     (list e)
                     `(mod-path ,(datum-intern-literal (format "~s" s)))))

(define-syntax-rule (indexed-racket x)
  (add-racket-index 'x (racket x)))

(define (add-racket-index s e)
  (let ([k (cond [(and (pair? s) (eq? (car s) 'quote)) (format "~s" (cadr s))]
                 [(string? s) s]
                 [else (format "~s" s)])])
    (index* (list k) (list e) e)))

(define-syntax-rule (define-/form id base)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ a)
       (with-syntax ([ellipses (datum->syntax #'a '(... ...))])
         #'(let ([ellipses #f])
             (base a)))])))

(define-/form racketblock0/form racketblock0)
(define-/form racketblock/form racketblock)
(define-/form racket/form racket)

(define (*racketlink stx-id id style . s)
  (let ([content (decode-content s)])
    (make-delayed-element
     (lambda (r p ri)
       (make-link-element
        style
        content
        (or (find-racket-tag p ri stx-id #f)
            `(undef ,(format "--UNDEFINED:~a--" (syntax-e stx-id))))))
     (lambda () content)
     (lambda () content))))

(define-syntax racketlink
  (syntax-rules ()
    [(_ id #:style style . content)
     (*racketlink (quote-syntax id) 'id style . content)]
    [(_ id . content)
     (*racketlink (quote-syntax id) 'id #f . content)]))
