#lang scheme/base
(require scribble/core
         scribble/latex-properties
         setup/main-collects)

(provide scribble-file
         downloaded-file
         add-defaults)

(define (add-property properties pred new)
  (if (ormap pred properties)
      properties
      (cons new properties)))

(define (scribble-file s)
  (path->main-collects-relative (collection-file-path s "scribble")))

(define (downloaded-file s)
  (build-path (find-system-path 'addon-dir) s))

(define (add-defaults doc pfx styl extras version?)
  (struct-copy part doc [style (make-style (style-name (part-style doc))
                                           ((if version? add-property (lambda (x y z) x))
                                            (add-property
                                             (style-properties (part-style doc))
                                             latex-defaults?
                                             (make-latex-defaults
                                              pfx
                                              styl
                                              extras))
                                            document-version?
                                            (make-document-version (version))))]))
