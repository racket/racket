#lang at-exp racket

#|
FIX THESE: 

0. close 10498; ok

1. the links within documentations don't work
   example: see list? in Advanced; 
   example: printing is pre-determined via the eval in the doc file 
   Robby suggests using a macro to generate documentation and to expand it
   in the context where the documentation is placed 

2. need to expand the allowed scribble entries to 
     defthing for e, pi, null, and eof in Beginner (and possibly elsewhere). done 
     it feels a bit kludgey and needs changes in two files, but I don't have time 
     to think about it more. 

     still to do: 
     defproc* for case-> in Advanced 
     rewrite the case-> based things in Advanced 

3. Fix up how primitives are "overwritten" in Advanced and friends 

4. clean up primitive functions:
   list? in Intermediate, others like that 
   possibly legacy issues: 
     why do we need eof or eof-object? in Beginner
     why do we need eq? in Beginner

tests to run: 
 collects/tests/htdp-lang/
|#

(require (for-syntax syntax/parse))

(require racket/provide)

(provide 
 provide-and-scribble 
 (for-syntax extract-names)
 all-from all-from-except defproc)

;; ---------------------------------------------------------------------------------------------------

(define-for-syntax *add #f)

(define-for-syntax (provide-and-scribble-only stx)
  (raise-syntax-error #f "use with provide-and-scribble only" stx))
(define-syntax all-from provide-and-scribble-only)
(define-syntax all-from-except provide-and-scribble-only)
(define-syntax defproc provide-and-scribble-only)
(define-syntax defthing provide-and-scribble-only)

(define-syntax (provide-and-scribble stx)
  (syntax-parse stx #:literals ()
    [(provide-and-scribble doc-tag:id requires rows ...)
     (provide-and-scribble-proc #'requires #'doc-tag #'(rows ...))]))

(define-for-syntax (provide-and-scribble-proc requires doc-tag row*)
  (define-values (add-docs-and-provide provides)
    (for/fold ((add-docs-and-provide '()) (provides '())) ((row (syntax->list row*)))
      (syntax-parse row #:literals (defproc all-from all-from-except)
        [(all-from-except tag:id path label:id f:id ...)
         (define-values (a p) (provide-all-from #'path #'label #'tag #'(f ...)))
         (values (cons a add-docs-and-provide) (append (syntax->list p) provides))]
        [(all-from tag:id path label:id)
         (define-values (a p) (provide-all-from #'path #'label #'tag #'()))
         (values (cons a add-docs-and-provide) (append (syntax->list p) provides))]
        [(title df ...)
         (define name* (extract-names (syntax->list #'(df ...))))
         (define exnm* (extract-external-name name*))
         (define defs* (rewrite-defs (syntax->list #'(df ...)) exnm*))
         (values (cons (lambda ()  ;; delay the syntax creation until add-sections is set
                         (with-syntax ([(ex ...) exnm*]
                                       [(df ...) defs*])
                           #`(#,*add title (list (cons #'ex df) ...))))
                       add-docs-and-provide)
                 (cons #`(provide #,@(optional-rename-out name*))
                       provides))])))
  (provide-and-scribble-code requires doc-tag add-docs-and-provide provides))

;; (U defproc defthing) -> [Listof (U (Id Id) Id)]
;; extract names from documented definitions in section 
(define-for-syntax (extract-names defs)
  (map (λ (d)
         (syntax-case d ()
           [(defproc (name args ...) range w ...) #'name]
           [(defthing name stuff ...) #'name]))
       defs))

;; (U defproc defthing) -> [Listof Syntax]
;; get documented definitions in section ready (parameterized over context)
(define-for-syntax (rewrite-defs defs names)
  (define (rewrite-one-def d n)
    (syntax-case d ()
      [(defproc (name args ...) range w ...)
       (with-syntax ([ex n])
         #'(lambda (c)
             (defproc #:id [ex (datum->syntax c 'ex)]
               (ex args ...) range w ...)))]
      [(defthing name range w ...)
       #'(lambda (c) (defthing #:id (datum->syntax c 'name) name range w ...))]))
  (map rewrite-one-def defs names))


;; Path Identifier Identifier [Listof Identifier] ->* [-> Syntax] Syntax[List]
;; create the require and provide clauses AND
;; delayed code for merging documentations from path -> label into the 'documentation' doc-tag submod
(define-for-syntax (provide-all-from path label prefix f*)
  (with-syntax ([path (syntax-case path (submod)
                        [(submod nested-path nested-tag) #'nested-path]
                        [_ path])]
                [(nested-tag ...)
                 (syntax-case path (submod)
                   [(submod nested-path nested-tag ...) #'(nested-tag ...)]
                   [_ #'()])]
                [label label]
                [prefix prefix]
                [(f ...) (syntax->list f*)]
                [mydocs (gensym 'mydocs)])
    (values (lambda ()  ;; delay the syntax creation until add-sections is set
              ;; ******************************************************************
              ;; I was really hoping to make 
              ;;   (local-require (only-in (submod path nested-tag ... label) (docs mydocs)))
              ;; to work but that gave me problems about 'docs' already required before
              ;; so I went with dynamic-require. Argh. 
              ;; ******************************************************************
              #`(for ((s ((dynamic-require '(submod path nested-tag ... label) 'docs) #'f ...)))
                  (#,*add (car s) (cadr s))))
            #`(;; import from path with prefix, exclude f ...
               (require (prefix-in prefix (except-in (submod path nested-tag ...) f ...)))
               ;; export the bindings without prefix 
               ; (local-require (only-in racket/provide filtered-out))
               (provide (filtered-out (lambda (name)
                                        (define prefix (format "^~a" (syntax-e #'prefix)))
                                        (and (regexp-match? prefix name)
                                             (regexp-replace prefix name "")))
                                      (all-from-out (submod path nested-tag ...))))))))

;; Identifier [Listof [-> Syntax]] [Listof Syntax] -> Syntax 
;; generate (module+ doc-tag ...) with the documentation in add-docs-and-provide, 
;; the first time it adds functions to (module+ doc-tag ...) that help render the docs
;; export the provides list 
(define-for-syntax (provide-and-scribble-code requires doc-tag add-docs-and-provide provides)
  (with-syntax ([(p* ...) (reverse provides)])
    (cond 
      [*add #`(begin p* ... (module+ #,doc-tag #,@(map (lambda (adp) (adp)) (reverse add-docs-and-provide))))]
      [else
       (set! *add (syntax-local-introduce #'add-sections))
       #`(begin (module+ #,doc-tag 
                         (require scribble/manual)
                         #,requires
                         ;; -----------------------------------------------------------------------
                         ;; Section  = [Listof (cons Identifier Doc)]
                         ;; Sections = [Listof (list Title Section)]
                         (provide 
                          ;; Identfier ... *-> Sections 
                          ;; retrieve the document without the specified identfiers
                          docs
                          
                          ;; Sections String -> [Listof ScribbleBox]
                          ;; render the sections as a scribble list of splice-boxes: #:tag-prefix p
                          render-sections)
                         ;; -----------------------------------------------------------------------
                         ;;
                         
                         (define (render-sections s c p)
                           (let render-sections ([s s])
                             (cond
                               [(null? s) '()]
                               [else 
                                (define section1 (car s))
                                (define others (render-sections (cdr s)))
                                (define-values (section-title stuff) (apply values section1))
                                (define sorted 
                                  (sort stuff string<=? #:key (compose symbol->string syntax-e car)))
                                (define typed (for/list ((s sorted)) (re-context c (car s) (cdr s))))
                                (cons @section[#:tag-prefix p]{@section-title}
                                      (cons typed others))])))
                         
                         (define (re-context c id defproc)
                           (defproc c))
                         
                         ;;
                         (define (docs . exceptions)
                           (define (is-exception i)
                             (memf (lambda (j) (eq? (syntax-e j) (syntax-e i))) exceptions))
                           (for/list ((s *sections))
                             (define sectn (second s))
                             (define clean (filter (lambda (i) (not (is-exception (car i)))) sectn))
                              (list (first s) clean)))
                         ;; 
                         ;; state variable: Sections
                         (define *sections '())
                         ;; String Section -> Void 
                         ;; add _scontent_ section to *sections in the doc submodule 
                         (define (#,*add stitle scontent)
                           (define exists #f)
                           (define sections
                             (for/list ((s *sections))
                               (cond
                                 [(string=? (first s) stitle) 
                                  (set! exists #t)
                                  (list stitle (append (second s) scontent))]
                                 [else s])))
                           (if exists
                               (set! *sections sections)
                               (set! *sections (append sections (list (list stitle scontent))))))
                         
                         #,@(map (lambda (adp) (adp)) (reverse add-docs-and-provide)))
                p* ...)])))

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

