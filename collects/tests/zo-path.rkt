#lang racket
(require setup/dirs)

;; Paths from the build location shouldn't show up in bytecode files
;; or documentation. Check ".zo", ".dep", and ".html" files in the
;; build on the assumption that the first three elements of the
;; build path are unique enough that they shouldn't appear anywhere.

(define rx:dir
  (byte-regexp
   (regexp-quote
    (path->bytes
     (apply build-path
            (take (explode-path (find-collects-dir)) 
                  3))))))

(define (check-content rx:name)
  (lambda (name kind v)
    (when (regexp-match? rx:name name)
      (call-with-input-file* name
        (lambda (in)
          (when (regexp-match? rx:dir in)
            (eprintf "Found ~s in ~s\n" rx:dir name)))))))

(fold-files (check-content #rx"[.](?:zo|dep)$")
            (void)
            (find-collects-dir))

;; Check rendered docs, too:
(fold-files (check-content #rx"[.](?:html)$")
            (void)
            (find-doc-dir))
