#lang racket/base
(require racket/place)

;; When a thread is killed during `directory-list`, there's
;; a clean-up action attached to both the thread and its
;; `dynamic-wind` chain (effectively). Check that only one
;; of them runs, otherwise closedir() is called twice (and
;; libc will typically detect a mistake and abort).

(define (go)
  (place
   pch
   (place-channel-put pch 'ok)
   (let loop ()
     (directory-list
      (let-values ([(base name dir?)
                    (split-path (collection-file-path "place.rkt" "racket"))])
        base))
     (loop))))

(module+ main
  (for ([i 25])
    (printf "~a\n" i)
    (define p (go))
    (place-channel-get p)
    (place-kill p)))
