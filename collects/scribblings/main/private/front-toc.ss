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
         [elt (if (eq? here there) elt (link target #:underline? #f elt))])
    (make-toc-element #f null (list elt))))

;; FIXME: Use this to avoid hard-wiring manual titles and paths below
(define (resolve s [f s])
  (resolved-module-path-name
   (module-path-index-resolve
    (module-path-index-join `(lib ,(format "scribblings/~a/~a.scrbl" s f))
                            #f))))

(define (front-toc here)
  (define docdir (let ([d (find-doc-dir)]) (lambda (p) (build-path d p))))
  (let ([to-toc (to-toc here)])
    (make-splice
     (list (to-toc 'start   "index.html" "PLT Manuals")
           (to-toc 'search  "search/index.html" "Search PLT Manuals")
           spacer
           (to-toc 'index   "master-index/index.html" "Master Index")
           spacer
           (to-toc 'license (docdir "license/index.html") "License")
           (to-toc 'acks    (docdir "acks/index.html")    "Acknowledgments")
           (to-toc 'release (docdir "release/index.html") "Release Notes")
           spacer
           (to-toc #f (format "~a?v=~a" bug-url (version)) "Report a Bug")))))
