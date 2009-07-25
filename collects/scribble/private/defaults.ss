#lang scheme/base
(require scribble/core
         scribble/latex-variants
         setup/main-collects)

(provide scribble-file
         add-defaults)

(define (add-variant variants pred new)
  (if (ormap pred variants)
      variants
      (cons new variants)))

(define (scribble-file s)
  (path->main-collects-relative (build-path (collection-path "scribble") s)))

(define (add-defaults doc pfx styl extras version?)
  (struct-copy part doc [style (make-style (style-name (part-style doc))
                                           ((if version? add-variant (lambda (x y z) x))
                                            (add-variant
                                             (style-variants (part-style doc))
                                             latex-defaults?
                                             (make-latex-defaults
                                              pfx
                                              styl
                                              extras))
                                            document-version?
                                            (make-document-version (version))))]))
