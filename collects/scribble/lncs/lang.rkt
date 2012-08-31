#lang racket/base
(require scribble/doclang
         scribble/core
         racket/file
         (except-in scribble/base author)
         (prefix-in s/b: scribble/base)
         scribble/decode
         "../private/defaults.rkt"
         setup/main-collects
         scribble/html-properties
         scribble/latex-properties
         scribble/latex-prefix
         racket/stxparam
         net/ftp
         file/gunzip
         (for-syntax racket/base
                     racket/list
                     racket/stxparam-exptime))

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])
         abstract include-abstract
         authors author
         institute institutes
         email)

(define-syntax (module-begin stx)
  ;; No options, currently, but keep in case we want to support some:
  (syntax-case* stx () (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process) () . body)]))

(define cls-file
  (let ([p (scribble-file "lncs/llncs.cls")])
    (if (file-exists? (main-collects-relative->path p))
        p
        (downloaded-file "llncs.cls"))))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8 (string-append "\\documentclass{llncs}\n"
                                                    unicode-encoding-packages))
                (scribble-file "lncs/style.tex")
                (list cls-file)
                #f))

(define lncs-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scribble" "lncs")))])
    (list
     (make-css-addition (abs "lncs.css"))
     (make-tex-addition (abs "lncs.tex")))))

(unless (or (not (path? cls-file))
            (file-exists? cls-file))
  (log-error (format "File not found: ~a" cls-file))
  (define site "ftp.springer.de")
  (define path "pub/tex/latex/llncs/latex2e")
  (define file "llncs2e.zip")
  (log-error (format "Downloading via ftp://~a/~a/~a..." site path file))
  (define c (ftp-establish-connection site 21 "anonymous" "user@racket-lang.org"))
  (ftp-cd c path)
  (make-directory* (find-system-path 'temp-dir))
  (ftp-download-file c (find-system-path 'temp-dir) file)
  (ftp-close-connection c)
  (define z (build-path (find-system-path 'temp-dir) file))
  ;; Poor man's unzip (replace it when we have an `unzip' library):
  (define i (open-input-file z))
  (define (skip n) (file-position i (+ (file-position i) n)))
  (define (get n) 
    (define s (read-bytes n i))
    (unless (and (bytes? s) (= n (bytes-length s)))
      (error "unexpected end of file"))
    s)
  (let loop ()
    (cond
     [(equal? #"PK\3\4" (get 4))
      ;; local file header
      (skip 2)
      (define data-desc? (bitwise-bit-set? (bytes-ref (get 1) 0) 3))
      (skip 11)
      (define sz (integer-bytes->integer (get 4) #f #f))
      (skip 4)
      (define name-sz (integer-bytes->integer (get 2) #f #f))
      (define extra-sz (integer-bytes->integer (get 2) #f #f))
      (define name (bytes->string/utf-8 (get name-sz) #\?))
      (skip extra-sz)
      (if (equal? name "llncs.cls")
          (call-with-output-file cls-file
            (lambda (o)
              (inflate i o)))
          (begin
            (skip sz)
            (when data-desc?
              skip 12)
            (loop)))]
     [else (error "didn't find file in archive")]))
  (close-input-port i)
  (delete-file z))

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
