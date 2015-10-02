#lang racket/base
(require net/url
         racket/string
         racket/format
         racket/match
         racket/list
         "../name.rkt"
         "download.rkt"
         "desc.rkt")

(provide split-github-url
         split-git-url
         split-git-or-hub-url
         enclosing-path-for-repo
         real-git-url
         use-git-for-github?)

(define use-git-for-github? (not (getenv "PLT_USE_GITHUB_API")))

(define (split-github-url pkg-url)
  (if (equal? (url-scheme pkg-url) "github")
      ;; github://
      (map path/param-path (url-path/no-slash pkg-url))
      ;; git://
      (let* ([paths (map path/param-path (url-path/no-slash pkg-url))])
        (list* (car paths)
               (regexp-replace* #rx"[.]git$" (cadr paths) "")
               (or (url-fragment pkg-url) "master")
               (extract-git-path pkg-url)))))

(define (extract-git-path pkg-url)
  (let ([a (assoc 'path (url-query pkg-url))])
    (or (and a (cdr a) (string-split (cdr a) "/"))
        null)))

;; returns: (values host repo branch path)
(define (split-git-url pkg-url)
  (values (string->symbol (url-scheme pkg-url))
          (url-host pkg-url)
          (url-port pkg-url)
          (string-join (map (compose ~a path/param-path)
                            (url-path/no-slash pkg-url))
                       "/")
          (or (url-fragment pkg-url) "master")
          (extract-git-path pkg-url)))

(define (split-git-or-hub-url pkg-url #:type [type #f])
  (if (or (equal? "github" (url-scheme pkg-url))
          (eq? type 'github))
      (match (split-github-url pkg-url)
        [(list* user repo branch path)
         (values 'https "github.com" #f (~a user "/" repo) branch path)])
      (split-git-url pkg-url)))

(define (enclosing-path-for-repo url-str in-repo-dir)
  (define-values (transport host port repo branch path)
    (split-git-or-hub-url (string->url url-str)))
  (let loop ([path path]
             [in-repo-dir (simplify-path in-repo-dir)])
    (cond
     [(null? path) in-repo-dir]
     [else
      (define-values (base name dir?) (split-path in-repo-dir))
      (if (not (path? base))
          (error "path for git repo link is too short for path in package source")
          (loop (cdr path) base))])))

(define (real-git-url pkg-url host port repo #:type [type #f])
  (url->string
   (if (or (equal? "github" (url-scheme pkg-url))
           (eq? type 'github))
       ;; Convert "github://" to a real URL:
       (url "https" #f host port #t
            (map (lambda (s) (path/param s null)) (string-split repo "/"))
            null
            #f)
       ;; Drop any query or fragment in the URL:
       (struct-copy url pkg-url
                    [query null]
                    [fragment #f]))))
