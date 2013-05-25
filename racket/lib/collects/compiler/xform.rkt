#lang scheme/base

(require dynext/compile
         setup/dirs
         (prefix-in xform: "private/xform.rkt"))

(provide xform)

(define (xform quiet? src dest header-dirs #:keep-lines? [keep-lines? #f])
  (let ([exe (current-extension-compiler)]
        [flags (expand-for-compile-variant
                (current-extension-preprocess-flags))]
        [headers (apply append
                        (map (current-make-compile-include-strings)
                             (append
                              header-dirs
                              (list (find-include-dir)))))])
    (xform:xform quiet?
                 (cons exe
                       (append flags headers))
                 src
                 dest
                 keep-lines?
                 #f #t #t
                 #f #f
                 #f #f
                 #f)))

