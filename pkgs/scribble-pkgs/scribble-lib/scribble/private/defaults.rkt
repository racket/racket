#lang scheme/base
(require scribble/core
         scribble/latex-properties
         scribble/html-properties
         setup/collects)

(provide scribble-file
         downloaded-file
         add-defaults)

(define (add-property properties pred new)
  (if (ormap pred properties)
      properties
      (cons new properties)))

(define (scribble-file s)
  (path->collects-relative (collection-file-path s "scribble")))

(define (downloaded-file s)
  (build-path (find-system-path 'addon-dir) s))

(define (add-defaults doc pfx styl extras version?
                      #:html [html #f]
                      #:properties [properties null])
  (struct-copy part doc [style (make-style (style-name (part-style doc))
                                           ((if version? add-property (lambda (x y z) x))
                                            (add-property
                                             ((if html add-property (lambda (x y z) x))
                                              (append
                                               (style-properties (part-style doc))
                                               properties)
                                              html-defaults?
                                              html)
                                             latex-defaults?
                                             (make-latex-defaults
                                              pfx
                                              styl
                                              extras))
                                            document-version?
                                            (make-document-version (version))))]))
