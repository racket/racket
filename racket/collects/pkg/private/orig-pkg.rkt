#lang racket/base
(require racket/path
         net/url
         "dirs.rkt"
         "repo-path.rkt"
         "path.rkt")

;; An "orig-pkg" is the way that that a pacage source is recorded
;; in the installed-package database.

(provide desc->orig-pkg
         same-orig-pkg?)

(define (desc->orig-pkg type src extra-path #:repo-url [repo-url #f])
  (case type
    [(name) (if repo-url
                `(catalog ,src ,repo-url)
                `(catalog ,src))]
    [(link static-link) `(,type
                          ,(path->string
                            (find-relative-path (pkg-installed-dir)
                                                ;; normalize with ending slash
                                                (path->directory-path
                                                 (simple-form-path src))
                                                #:more-than-root? #t)))]
    [(clone) 
     (define-values (transport host port repo branch path)
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

;; Ignore URL that is potentially recorded for a 'catalog kind:
(define (same-orig-pkg? a b)
  (if (and (eq? 'catalog (car a))
           (eq? 'catalog (car b)))
      (equal? (cadr a) (cadr b))
      (equal? a b)))
