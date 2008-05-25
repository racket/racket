#lang scheme/base

(require scribble/manual
         scribble/struct
         scribble/decode
         setup/dirs)
(provide front-toc)

(define bug-url "http://bugs.plt-scheme.org/")

(define spacer
  (make-toc-element #f null '(nbsp)))

(define ((to-toc here) there target label)
  (let* ([elt (make-element "tocsubseclink" (list label))]
         [elt (link target #:underline? (eq? here there) elt)])
    (make-toc-element #f null (list elt))))

;; FIXME: Use this to avoid hard-wiring manual titles and paths below
(define (resolve s [f s])
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(format "scribblings/~a/~a.scrbl" s f))
                            #f))))

(define (front-toc here main?)
  (define docdir (let ([d (find-doc-dir)]) (lambda (p) (build-path d p))))
  (let ([to-toc (to-toc here)]
        [up (lambda (s)
              (if main?
                  s
                  ;; This needs to use a cookie(?) to always get to the
                  ;; user-specific page... ?
                  (string-append "../" s)))])
    (make-splice
     (list (to-toc 'start   (up "index.html") "PLT Scheme Documentation")
           (to-toc 'search  (up "search/index.html") "Search PLT Manuals")
           spacer
           (to-toc 'index   (up "master-index/index.html") "Master Index")
           spacer
           (to-toc 'license (docdir "license/index.html") "License")
           (to-toc 'acks    (docdir "acks/index.html")    "Acknowledgments")
           (to-toc 'release (docdir "release/index.html") "Release Notes")
           spacer
           (to-toc #f (format "~a?v=~a" bug-url (version)) "Report a Bug")))))
