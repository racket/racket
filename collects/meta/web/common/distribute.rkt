#lang racket/base

(require racket/system racket/list racket/promise)

(define rsync-exe (delay (or (find-executable-path "rsync")
                             (error 'distribute "couldn't find `rsync'"))))

(define (rsync . args)
  (unless (apply system* (force rsync-exe) args)
    (error 'distribute "errors when running: rsync with ~s" args)))

(provide distribute)
(define (distribute specs)
  (for ([s (in-list specs)])
    (let ([srcs (cdr s)] [tgt (car s)])
      (apply rsync "-aqz" "-e" "ssh" "--delete" `(,@srcs ,tgt)))))
