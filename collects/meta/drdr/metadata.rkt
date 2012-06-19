#lang racket
(require "path-utils.rkt"
         "dirstruct.rkt"
         "scm.rkt")

(define PROP:command-line "drdr:command-line")
(define PROP:timeout "drdr:timeout")

(define (path-command-line a-path)
  (define suffix (filename-extension a-path))
  (define default-cmd
    (and suffix
         (case (string->symbol (bytes->string/utf-8 suffix))
           [(ss scm scrbl rkt sls) '(racket *)]
           [(rktl)                 '(racket "-f" *)]
           [else                   #f])))
  (define (replace-* s)
    (if (eq? '* s)
        (path->string* a-path)
        s))
  (match (get-prop a-path 'drdr:command-line default-cmd)
    [#f #f]
    [(? list? l)
     (map replace-* l)]))

(define (path-timeout a-path)
  (get-prop a-path 'drdr:timeout #f))

(define (path-responsible a-path)
  (get-prop a-path 'responsible #:as-string? #t))

(define (path-random? a-path)
  (get-prop a-path 'drdr:random))

(provide/contract
 [PROP:command-line string?]
 [PROP:timeout string?]
 [path-responsible (path-string? . -> . (or/c string? false/c))]
 [path-command-line (path-string? . -> . (or/c (cons/c symbol? (listof string?)) false/c))]
 [path-random? (path-string? . -> . boolean?)]
 [path-timeout (path-string? . -> . (or/c exact-nonnegative-integer? false/c))])

;;; Property lookup
(provide props-cache)
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
                 (define tmp-file (make-temporary-file "props~a.rkt" #f (current-temporary-directory)))
                 (and
                  ; Checkout the props file
                  (scm-export-file
                   rev
                   (plt-repository)
                   "collects/meta/props" 
                   tmp-file)
                  ; Dynamic require it
                  (begin0
                    (with-handlers ([exn? (λ (x) #f)])
                      (dynamic-require `(file ,(path->string tmp-file))
                                       'get-prop))
                    (delete-file tmp-file))))))
  #;(unless props:get-prop
     (error 'get-prop "Could not load props file for ~e" (current-rev)))
  ; XXX get-prop is stupid and errors when a-path is invalid rather than returning def
  (with-handlers ([exn? (lambda (x) def)])
    (props:get-prop a-path prop def
                    #:as-string? as-string?)))
