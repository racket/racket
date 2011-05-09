#lang racket/base
(require scribble/doclang
         scribble/core
         (except-in scribble/base author)
         scribble/decode
         "../private/defaults.ss"
         (for-syntax racket/base racket/list)
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties)

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])
         abstract include-abstract
         authors author
         affiliation affiliation-mark affiliation-sep
         maketitle)

;; No options, currently, but keep in case we want to support some:
(define-syntax (module-begin stx)
  (syntax-case* stx () (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process) () . body)]))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8
                 "\\documentclass{llncs}\n") ; (format "\\documentclass{llncs}\n\\usepackage{times}\n\\usepackage{qcourier}\n"))
                (scribble-file "lncs/style.tex")
                (list (scribble-file "lncs/llncs.cls"))
                #f))


(define jfp-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scribble" "lncs")))])
    (list
     (make-css-addition (abs "lncs.css"))
     (make-tex-addition (abs "lncs.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" jfp-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (extract-abstract p)
  (unless (part? p)
    (error 'include-abstract "doc binding is not a part: ~e" p))
  (unless (null? (part-parts p))
    (error 'include-abstract "abstract part has sub-parts: ~e" (part-parts p)))
  (when (part-title-content p)
    (error 'include-abstract "abstract part has title content: ~e" (part-title-content p)))
  (part-blocks p))

(define-syntax-rule (include-abstract mp)
  (begin
    (require (only-in mp [doc abstract-doc]))
    (make-nested-flow abstract-style (extract-abstract abstract-doc))))

;; ----------------------------------------
;; Author

(define-syntax (author stx)
  (raise-syntax-error 'author "can only be used inside 'authors'" stx))
(define-syntax (authors stx)
  (syntax-case stx (author)
    [(_ (author . args) ...)
     #`(make-element (style "author" '())
                     (decode-content
                      (list
                       #,@(apply 
                           append
                           (add-between
                            (for/list ([stx (in-list (syntax->list #'(args ...)))])
                              (syntax-case stx ()
                                [(#:inst string rest ...)
                                 (append (syntax->list #'(rest ...))
                                         (list #'(element (style "inst" '()) (decode-content (list string)))))]
                                [(rest ...)
                                 (syntax->list #'(rest ...))]))
                            (list #'(element (style "and" '()) '())))))))]
    [(_ . rest)
     (raise-syntax-error 'authors "expected a sequence of authors" stx)]))

(define (maketitle)
  (element (style "maketitle" jfp-extras) '()))

(define (affiliation . txt)
  (make-element (make-style "Affiliation" jfp-extras) (decode-content txt)))

(define (affiliation-mark . txt)
  (make-element (make-style "MarkSuperscript" jfp-extras) (decode-content txt)))

(define (affiliation-sep)
  (hspace 2))
