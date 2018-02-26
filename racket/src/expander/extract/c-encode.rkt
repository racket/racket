#lang racket/base

(provide encode-to-c)

;; Take a stream that has a single S-expression and converts it to C
;; code for a string that contains the S-expression

(define (encode-to-c in out)
  (fprintf out "#define EVAL_STARTUP EVAL_ONE_STR(startup_source)\n")
  (fprintf out "static const char *startup_source =\n")
  (for ([l (in-lines in)])
    (let* ([l (regexp-replace* #rx"\\\\" l "\\\\\\\\")]
           [l (regexp-replace* #rx"\"" l "\\\\\"")]
           [l (regexp-replace* #rx"\t" l " ")]
           [l (if (regexp-match? #rx"\"" l)
                  ;; Has a string - can't safely delete more spaces
                  l
                  (let ([l (regexp-replace* #rx" +" l " ")])
                    (regexp-replace* #rx" \\(" l "(")))])
      (fprintf out "\"~a\"\n" l)))
  (fprintf out ";\n"))
