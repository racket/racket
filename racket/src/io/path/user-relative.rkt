#lang racket/base
(require racket/fixnum
         "path.rkt"
         "parameter.rkt"
         "sep.rkt")

(provide relative-to-user-directory)

(define (relative-to-user-directory p)
  (define dir (current-directory-for-user))
  (define dir-bs (path-bytes dir))
  (define p-bs (path-bytes p))
  (define dir-len (bytes-length dir-bs))
  (define p-len (bytes-length p-bs))
  (cond
    [(and (dir-len . < . p-len)
          (for/and ([i (in-range dir-len)])
            (eq? (bytes-ref dir-bs i)
                 (bytes-ref p-bs i))))
     (let loop ([i dir-len])
       (cond
         [(and (i . < . p-len)
               (is-sep? (bytes-ref p-bs i) (path-convention p)))
          (loop (fx+ i 1))]
         [else
          (path (subbytes p-bs i) (path-convention p))]))]
    [else p]))
