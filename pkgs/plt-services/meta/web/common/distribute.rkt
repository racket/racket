#lang racket/base

(require racket/system racket/list racket/promise)

(define rsync-exe (lazy (or (find-executable-path "rsync")
                            (error 'distribute "couldn't find `rsync'"))))

(define (rsync . args)
  (unless (apply system* (force rsync-exe) args)
    (error 'distribute "errors when running: rsync with ~s" args)))

(define (flatten-path path)
  (define m (regexp-match #rx"^(.*?)/\\*(/.*|$)$" path))
  (if m
    (append-map
     flatten-path
     (sort (map (λ(p) (string-append
                       (cadr m) "/" (path-element->string p) (caddr m)))
                (directory-list (cadr m)))
           string<?))
    (list path)))

(provide distribute)
;; see "../config.rkt" for a description of the specs
(define (distribute specs)
  (for ([s (in-list specs)])
    (let ([srcs (append-map flatten-path (cdr s))] [tgt (car s)])
      (printf "  to ~a\n" tgt)
      (apply rsync "-aqz" "-e" "ssh" "--delete" `(,@srcs ,tgt)))))
