#lang racket/base
(require racket/path
         net/url
         "dirs.rkt"
         "repo-path.rkt"
         "path.rkt")

;; An "orig-pkg" is the way that that a pacage source is recorded
;; in the installed-package database.

(provide desc->orig-pkg)

(define (desc->orig-pkg type src extra-path)
  (case type
    [(name) `(catalog ,src)]
    [(link static-link) `(,type
                          ,(path->string
                            (find-relative-path (pkg-installed-dir)
                                                (simple-form-path src)
                                                #:more-than-root? #t)))]
    [(clone) 
     (define-values (host port repo branch path)
       (split-git-or-hub-url (string->url src)))
     `(clone ,(path->string
               (find-relative-path (pkg-installed-dir)
                                   (simple-form-path 
                                    (apply build-path
                                           extra-path
                                           path))
                                   #:more-than-root? #t))
       ,src)]
    [(file dir) `(,type ,(simple-form-path* src))]
    [else `(url ,src)]))
