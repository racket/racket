;
;  Faster, more idiomatic Scheme by Neil Van Dyke
;

#lang racket/base

(define (main iport)
  (apply printf "~s ~s ~s\n"
         (let wc ((i #f) (lines 0) (words 0) (chars 0))
           (let ((x (read-char iport)))
             (if (eof-object? x)
                 (list lines words chars)
                 (case x
                   ((#\newline)     (wc #f (add1 lines) words (add1 chars)))
                   ((#\space #\tab) (wc #f       lines  words (add1 chars)))
                   (else
                    (wc #t lines (if i words (add1 words)) (add1 chars)))))))))

(main (current-input-port))
