#lang racket
(require file/xpm
         tests/eli-tester)

(define tree-xpms
  (for*/list ([collect (in-list (list "icons" "guibuilder"))]
              [file (in-directory (collection-path collect))]
              #:when (regexp-match #rx"\\.xpm$" (path->bytes file)))
    file))

(define (xpm-test f)
  (test
   (xpm->bitmap% (with-input-from-file f xpm-read))))

(test
 (for ([f (in-list tree-xpms)])
   (test #:failure-prefix (path->string f)
         (xpm-test f))))
