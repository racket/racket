#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../basic.ss"
         scheme/list
         "manual-utils.ss"
         "manual-style.ss"
         (for-syntax scheme/base)
         (for-label scheme/base))

(provide schemeblock SCHEMEBLOCK schemeblock/form
         schemeblock0 SCHEMEBLOCK0 schemeblock0/form
         schemeblockelem
         schemeinput
         schememod
         scheme SCHEME scheme/form schemeresult schemeid 
         schememodname
         schememodlink indexed-scheme
         schemelink)

(define-code schemeblock0 to-paragraph)
(define-code schemeblock (to-paragraph/prefix (hspace 2) (hspace 2) ""))
(define-code SCHEMEBLOCK (to-paragraph/prefix (hspace 2) (hspace 2) "")
                         UNSYNTAX)
(define-code SCHEMEBLOCK0 to-paragraph UNSYNTAX)
(define interaction-prompt (make-element 'tt (list "> " )))
(define-code schemeinput
  (to-paragraph/prefix
   (make-element #f (list (hspace 2) interaction-prompt))
   (hspace 4)
   ""))

(define-syntax (schememod stx)
  (syntax-case stx ()
    [(_ #:file filename lang rest ...)
     (with-syntax ([modtag (datum->syntax
                            #'here
                            `(unsyntax (make-element
                                        #f
                                        (list (hash-lang)
                                              spacer
                                              ,(if (identifier? #'lang)
                                                   `(as-modname-link
                                                     ',#'lang
                                                     (to-element ',#'lang))
                                                   #'(scheme lang)))))
                            #'lang)]
                   [(file ...)
                    (if (syntax-e #'filename)
                        (list
                         (datum->syntax
                          #'filename
                          `(code:comment (unsyntax (t "In \"" ,(syntax-e #'filename) "\":")))
                          #'filename))
                        null)])
       (syntax/loc stx (schemeblock file ... modtag rest ...)))]
    [(_ lang rest ...)
     (syntax/loc stx (schememod #:file #f lang rest ...))]))

(define (to-element/result s)
  (make-element result-color (list (to-element/no-color s))))
(define (to-element/id s)
  (make-element symbol-color (list (to-element/no-color s))))

(define-syntax (keep-s-expr stx)
  (syntax-case stx ()
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

(define-code schemeblockelem to-element)

(define-code scheme to-element unsyntax keep-s-expr add-sq-prop)
(define-code SCHEME to-element UNSYNTAX keep-s-expr add-sq-prop)
(define-code schemeresult to-element/result unsyntax keep-s-expr add-sq-prop)
(define-code schemeid to-element/id unsyntax keep-s-expr add-sq-prop)
(define-code *schememodname to-element unsyntax keep-s-expr add-sq-prop)

(define-syntax schememodname
  (syntax-rules (unsyntax)
    [(schememodname #,n)
     (let ([sym n])
       (as-modname-link sym (to-element sym)))]
    [(schememodname n)
     (as-modname-link 'n (*schememodname n))]))

(define-syntax schememodlink
  (syntax-rules (unsyntax)
    [(schememodlink n content ...)
     (*as-modname-link 'n (elem #:style #f content ...))]))

(define (as-modname-link s e)
  (if (symbol? s)
      (*as-modname-link s e)
      e))

(define (*as-modname-link s e)
  (make-link-element module-link-color
                     (list e)
                     `(mod-path ,(format "~s" s))))

(define-syntax-rule (indexed-scheme x)
  (add-scheme-index 'x (scheme x)))

(define (add-scheme-index s e)
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

(define-/form schemeblock0/form schemeblock0)
(define-/form schemeblock/form schemeblock)
(define-/form scheme/form scheme)

(define (*schemelink stx-id id . s)
  (let ([content (decode-content s)])
    (make-delayed-element
     (lambda (r p ri)
       (list
        (make-link-element
         #f
         content
         (or (find-scheme-tag p ri stx-id #f)
             `(undef ,(format "--UNDEFINED:~a--" (syntax-e stx-id)))))))
     (lambda () content)
     (lambda () content))))

(define-syntax-rule (schemelink id . content)
  (*schemelink (quote-syntax id) 'id . content))

