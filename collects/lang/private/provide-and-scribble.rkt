#lang at-exp racket

(require (for-syntax syntax/parse) scribble/manual scribble/eval racket/sandbox)

(provide 
 define-module-local-eval 
 provide-and-scribble all-from all-from-except
 )

;; ---------------------------------------------------------------------------------------------------

;; **********************************************************************************
;; this needs something like a @defmodule[localname] but perhaps
;; this should be added when I require the doc submodule
;; **********************************************************************************

(define-for-syntax *add #f)

(define-syntax-rule 
  (all-from a ...)
  (raise-syntax-error "use with provide-and-scribble only"))

(define-syntax-rule 
  (all-from-except a ...)
  (raise-syntax-error "use with provide-and-scribble only"))

(define-syntax (provide-and-scribble stx)
  (syntax-parse stx #:literals (defproc)
    [(provide-and-scribble doc-tag:id) #'(void)]
    [(provide-and-scribble doc-tag:id row rows ...)
     (define-values (provides add-docs-and-provide)
       (syntax-parse #'row #:literals (defproc all-from all-from-except)
         [(all-from-except tag:id path label:id f:id ...)
          (provide-all-from #'doc-tag #'path #'label #'tag #'(rows ...) #'(f ...))]
         [(all-from tag:id path label:id)
          (provide-all-from #'doc-tag #'path #'label #'tag #'(rows ...) #'())]
         [(title (defproc (name args ...) range w ...) ...)
          (define name* (syntax->list #'(name ...)))
          (values #`((provide #,@(optional-rename-out name*))
                     (provide-and-scribble doc-tag rows ...))
                  (lambda ()  ;; delay the syntax creation until add-sections is set
                    (with-syntax ([(ex ...) (extract-external-name name*)])
                      #`(#,*add title (list (cons #'ex (defproc (ex args ...) range w ...)) ...)))))]))
     (provide-and-scribble-code #'doc-tag add-docs-and-provide provides)]))

;; Identifier Path Identifier Identifier [Listof RowSpec] [Listof Identifier] ->* Syntax [-> Syntax]
;; create the require and provide clauses AND
;; delayed code for merging documentations from path -> label into the 'documentation' doc-tag submod
(define-for-syntax (provide-all-from doc-tag path label prefix row* f*)
  (with-syntax ([doc-tag doc-tag]
                [path (syntax-case path (submod)
                        [(submod nested-path nested-tag) #'nested-path]
                        [_ path])]
                [(nested-tag ...)
                 (syntax-case path (submod)
                   [(submod nested-path nested-tag ...) #'(nested-tag ...)]
                   [_ #'()])]
                [label label]
                [prefix prefix]
                [(f ...) (syntax->list f*)]
                [(rows ...) (syntax->list row*)])
    (values #'((require (submod path nested-tag ... label))
               ;; import from path with prefix, exclude f ...
               (require (prefix-in prefix (except-in (submod path nested-tag ...) f ...)))
               ;; export the bindings without prefix 
               (local-require (only-in racket/provide filtered-out))
               (provide (filtered-out (lambda (name)
                                        (define prefix (format "^~a" (syntax-e #'prefix)))
                                        (and (regexp-match? prefix name)
                                             (regexp-replace prefix name "")))
                                      (all-from-out (submod path nested-tag ...))))
               ;; on to the rest
               (provide-and-scribble doc-tag rows ...))     
            (lambda ()  ;; delay the syntax creation until add-sections is set
              #`(for ((s (docs #'f ...)))
                  (#,*add (car s) (cadr s)))))))

;; Identifier [-> Syntax] Syntax -> Syntax 
;; generate (module+ doc-tag ...) with the documentation in add-docs-and-provide, 
;; the first time it adds functions to (module+ doc-tag ...) that help render the docs
;; export the provides list 
(define-for-syntax (provide-and-scribble-code doc-tag add-docs-and-provide provides)
  (cond 
    [*add #`(begin (module+ #,doc-tag #,(add-docs-and-provide))
                   #,@provides)]
    [else (set! *add (syntax-local-introduce #'add-sections))
          #`(begin (module+ #,doc-tag 
                            ;; -----------------------------------------------------------------------
                            ;; Section  = [Listof (cons Identifier Doc)]
                            ;; Sections = [Listof (list Identifier Section)]
                            (provide 
                             ;; Identfier ... *-> Sections 
                             ;; retrieve the document without the specified identfiers
                             docs
                             ;; Sections -> [Listof ScribbleBox]
                             ;; render the sections as a scribble list of splice-boxes 
                             render-sections)
                            ;; -----------------------------------------------------------------------
                            ;;
                            (define (render-sections s)
                              (cond
                                [(null? s) '()]
                                [else 
                                 (define-values (section-title stuff) (apply values (car s)))
                                 (cons @section{@section-title}
                                       (cons (map cdr @stuff) (render-sections (cdr s))))]))
                            ;;
                            (define (docs . exceptions)
                              (define s (reverse *sections))
                              (define (is-exception i)
                                (memf (lambda (j) (eq? (syntax-e j) (syntax-e i))) exceptions))
                              (for/fold ((result '())) ((s *sections))
                                (define sectn (second s))
                                (define clean 
                                  (filter (lambda (i) (not (is-exception (car i)))) sectn))
                                (cons (list (first s) clean) result)))
                            ;; 
                            ;; state variable Sections
                            (define *sections '())
                            ;; String Sections -> Void 
                            ;; add _scontent_ section to *sections in the doc submodule 
                            (define (#,*add stitle scontent)
                              (set! *sections (cons (list stitle scontent) *sections)))
                            #,(add-docs-and-provide))
                   #,@provides)]))

;; [Listof (u Identifier (Identifier Identifier))] -> [Listof Identifier]
(define-for-syntax (extract-external-name lon)
  (map (lambda (name-or-pair)
         (syntax-parse name-or-pair 
           [(internal:id external:id) #'external]
           [name:id #'name]))
       lon))

;; [Listof (u Identifier (Identifier Identifier))] -> [Listof Identifier]
;; create rename-out qualifications as needed
(define-for-syntax (optional-rename-out lon)
  (map (lambda (name-or-pair)
         (syntax-parse name-or-pair 
           [(internal:id external:id) #'(rename-out (internal external))]
           [name:id #'name]))
       lon))

;; ---------------------------------------------------------------------------------------------------

;; (define-module-local-eval name-of-evaluator)
;; a make-base-eval whose namespace is initialized with the module where the macro is used 
(define-syntax-rule 
  (define-module-local-eval name)
  (begin
    (define-namespace-anchor ns)
    (define name 
      (parameterize ([sandbox-namespace-specs (list (lambda () (namespace-anchor->namespace ns)))]
                     [sandbox-error-output 'string]
                     [sandbox-output 'string])
        (make-base-eval)))))
