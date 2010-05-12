#lang scheme
(require "path-utils.ss"
         "dirstruct.ss"
         "scm.ss")

(define PROP:command-line "drdr:command-line")
(define PROP:timeout "drdr:timeout")

(define (path-command-line a-path)
  (match (get-prop a-path 'drdr:command-line #f)
    [#f
     (define suffix (filename-extension a-path))
     (and suffix
          (cond
            [(ormap (lambda (bs) (bytes=? suffix bs))
                    (list #"ss" #"scm" #"scrbl" #"rkt" #"sls"))
             (list "racket" "-qt" (path->string* a-path))]
            [(ormap (lambda (bs) (bytes=? suffix bs))
                    (list #"rktl"))
             (list "racket" "-f" (path->string* a-path))]
            [else
             #f]))]
    [""
     #f]
    [(? string? s)
     (map (lambda (s)
            (regexp-replace (regexp-quote "~s") s (path->string* a-path)))
          (regexp-split #rx" " s))]))

(define (path-timeout a-path)
  (get-prop a-path 'drdr:timeout #f))

(define (path-responsible a-path)
  (get-prop a-path 'responsible #:as-string? #t))

; XXX Document on help page
; XXX Use in computing "changes?"
(define (path-random? a-path)
  (get-prop a-path 'drdr:random))

(provide/contract
 [PROP:command-line string?]
 [PROP:timeout string?]
 [path-responsible (path-string? . -> . (or/c string? false/c))]
 [path-command-line (path-string? . -> . (or/c (listof string?) false/c))]
 [path-random? (path-string? . -> . boolean?)]
 [path-timeout (path-string? . -> . (or/c exact-nonnegative-integer? false/c))])

;;; Property lookup
(define props-cache (make-hasheq))
(define (get-prop a-fs-path prop [def #f] #:as-string? [as-string? #f])
  (define rev (current-rev))
  (define a-path
    (substring
     (path->string
      ((rebase-path (revision-trunk-dir rev) "/") a-fs-path))
     1))
  (define props:get-prop
    (hash-ref! props-cache rev
               (lambda ()
                 (define tmp-file (make-temporary-file "props~a.ss" #f (current-temporary-directory)))
                 (and
                  ; Checkout the props file
                  (scm-export
                   rev
                   (plt-repository)
                   "collects/meta/props" 
                   tmp-file)
                  ; Dynamic require it
                  (begin0
                    (dynamic-require `(file ,(path->string tmp-file))
                                     'get-prop)
                    (delete-file tmp-file))))))
  (unless props:get-prop
    (error 'get-prop "Could not load props file for ~e" (current-rev)))
  ; XXX get-prop is stupid and errors when a-path is invalid rather than returning def
  (with-handlers ([exn? (lambda (x) def)])
    (props:get-prop a-path prop def
                    #:as-string? as-string?)))
