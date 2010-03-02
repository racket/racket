#lang scheme
(require "path-utils.ss"
         "svn.ss")

(define (testable-file? pth)
  (define suffix (filename-extension pth))
  (and suffix
       (ormap (lambda (bs) (bytes=? suffix bs))
              (list #"ss" #"scm" #"scrbl"))))

(define SVN-PROP:command-line "plt:drdr:command-line")
(define SVN-PROP:timeout "plt:drdr:timeout")

(define (path-command-line a-path)
  (match (svn-property-value/root a-path SVN-PROP:command-line)
    [#f
     (if (testable-file? a-path)
         (list "mzscheme" "-qt" (path->string* a-path))
         #f)]
    [""
     #f]
    [(? string? s)
     (map (lambda (s)
            (regexp-replace (regexp-quote "$path") s (path->string* a-path)))
          (regexp-split #rx" " s))]))

(define (path-timeout a-path)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (string->number (svn-property-value/root a-path SVN-PROP:timeout))))

(provide/contract
 [SVN-PROP:command-line string?]
 [SVN-PROP:timeout string?]
 [path-command-line (path-string? . -> . (or/c (listof string?) false/c))]
 [path-timeout (path-string? . -> . (or/c exact-nonnegative-integer? false/c))])