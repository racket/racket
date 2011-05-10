#lang racket/base
(require scribble/doclang
         scribble/core
         (except-in scribble/base author)
         (prefix-in s/b: scribble/base)
         scribble/decode
         "../private/defaults.ss"
         (for-syntax racket/base racket/list)
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties
         racket/stxparam
         (for-syntax racket/stxparam-exptime 
                     racket/base
                     setup/dirs))

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])
         abstract include-abstract
         authors author
         institute institutes
         email)

(define-syntax (module-begin stx)
  (unless (file-exists? (collection-file-path "llncs.cls" "scribble" "lncs"))
    (define cd (find-collects-dir))
    (raise-syntax-error 'scribble/lncs
                        (format "Please download the llncs.cls file (in llncs2e.zip) and put it in this directory:\n  ~a~a"
                                (build-path (find-user-collects-dir)
                                            "scribble" "lncs")
                                (if cd
                                    (format "\nor in this one:\n  ~a"
                                            (build-path cd "scribble" "lncs"))
                                    ""))
                        #f))
  ;; No options, currently, but keep in case we want to support some:
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
                (string->bytes/utf-8 "\\documentclass{llncs}\n") 
                (scribble-file "lncs/style.tex")
                (list (scribble-file "lncs/llncs.cls"))
                #f))

(define lncs-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scribble" "lncs")))])
    (list
     (make-css-addition (abs "lncs.css"))
     (make-tex-addition (abs "lncs.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" lncs-extras))

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
     #`(paragraph
        (style 'author '())
        (make-element (style "LNCSauthor" lncs-extras)
                      (decode-content
                       (list
                        #,@(apply 
                            append
                            (add-between
                             (for/list ([stx (in-list (syntax->list #'(args ...)))])
                               (syntax-case stx ()
                                 [(#:inst string rest ...)
                                  (append (syntax->list #'(rest ...))
                                          (list #'(element (style "LNCSinst" lncs-extras) (decode-content (list string)))))]
                                 [(rest ...)
                                  (syntax->list #'(rest ...))]))
                             (list #'(element (style "LNCSand" lncs-extras) '()))))))))]
    [(_ . rest)
     (raise-syntax-error 'authors "expected a sequence of authors" stx)]))


(define-syntax-parameter email-ok #f)

(define-syntax (institute stx)
  (raise-syntax-error #f "can only be used inside 'institutes'" stx))
(define-syntax (institutes stx)
  (syntax-case stx (author)
    [(_ (inst . args) ...)
     #`(syntax-parameterize 
        ((email-ok #t))
        (paragraph 
         (style 'author '())
         (make-element (style "LNCSinstitutes" lncs-extras)
                       (decode-content
                        (list
                         #,@(apply 
                             append
                             (add-between
                              (for/list ([stx (in-list (syntax->list #'(args ...)))])
                                (syntax-case stx ()
                                  [(rest ...)
                                   (syntax->list #'(rest ...))]))
                              (list #'(element (style "LNCSand" lncs-extras) '())))))))))]
    [(_ . rest)
     (raise-syntax-error 'institutes "expected a sequence of institutes" stx)]))

(define-syntax (email stx)
  (syntax-case stx ()
    [(_ . args)
     (begin
       (unless (syntax-parameter-value #'email-ok)
         (raise-syntax-error 'email "email can appear inside institutes only"))
       #'(make-element (style "LNCSemail" lncs-extras)
                       (decode-content (list . args))))]))
