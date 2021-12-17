#lang racket/base
(require racket/place)

; Make sure certain parameter values are propagated to a new place.

(define (get)
  (list (current-directory)
        (current-library-collection-paths)
        (current-library-collection-links)
        (current-compiled-file-roots)))

(define (go)
  (place pch
         (place-channel-put pch (get))))

(module+ main
  (define tmp-dir (find-system-path 'temp-dir))
  (current-directory tmp-dir)
  (current-library-collection-paths (cons (build-path tmp-dir "no-such-dir")
                                          (current-library-collection-paths)))
  (current-library-collection-links (cons (build-path tmp-dir "no-such-file.rktl")
                                          (current-library-collection-links)))
  (current-compiled-file-roots (cons "no-such-compiled" (current-compiled-file-roots)))
  (define ph (go))
  (define got (place-channel-get ph))
  (void (place-wait ph))
  (unless (equal? (get) got)
    (error 'test "place parameter mismatch: ~s" got)))
