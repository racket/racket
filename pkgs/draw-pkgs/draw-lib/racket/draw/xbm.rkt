#lang racket/base

(provide read-xbm)

(define rx:define #rx#"#define[ \t]+[-A-Za-z0-9_]+[ \t]+([0-9]+)")
(define rx:byte #rx#"0x([0-9a-fA-F][0-9a-fA-F])")

(define (read-xbm in)
  (let/ec esc
    (let ([w (regexp-match rx:define in)]
          [h (regexp-match rx:define in)])
      (if (and w h)
          (let ([w (string->number (bytes->string/latin-1 (cadr w)))]
                [h (string->number (bytes->string/latin-1 (cadr h)))])
            (if (and (exact-integer? w)
                     (exact-integer? h)
                     (positive? w)
                     (positive? h))
                (values
                 w
                 h
                 (list->vector
                  (for/list ([i (in-range h)])
                    (list->bytes
                     (for/list ([j (in-range (quotient (+ w 7) 8))])
                       (let ([m (regexp-match rx:byte in)])
                         (if m
                             (string->number (bytes->string/latin-1 (cadr m)) 16)
                             (esc #f #f #f))))))))
                (values #f #f #f)))
          (values #f #f #f)))))
