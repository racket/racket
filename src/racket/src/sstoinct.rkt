#lang racket/base

(define (brackets->parens l)
  (regexp-replace* #rx"\\["
                   (regexp-replace* #rx"\\]" l ")")
                   "("))

(let loop ([ready? #f] [parens 0])
  (let ([l (read-line)])
    (cond
     [(eof-object? l)
      (when ready?
        (printf ");\n"))]
     [(regexp-match? #px"^\\s*$" l)
      ;; just spaces; do nothing
      (loop ready? parens)]
     [(regexp-match #px"^\\s*;" l)
      ;; comment; do nothing
      (loop ready? parens)]
     [else
      (unless ready?
        (printf "  EVAL_ONE_STR(\n"))
      (let* ([l (if (regexp-match? #rx"\"[^\"]*\\[[^\"]*\"" l)
                    l
                    (brackets->parens l))]
             [l (regexp-replace* #rx"\\\\" l "\\\\\\\\")]
             [l (regexp-replace* #rx"\"" l "\\\\\"")]
             [l (regexp-replace* #rx"\t" l " ")]
             [l (regexp-replace* #rx" +" l " ")]
             [l (if (regexp-match? #rx"\"" l)
                    ;; Has a string - can't safely delete more spaces
                    l
                    (regexp-replace* #rx" \\(" l "("))]
             [l
              ;; Check for comments:
              (if (regexp-match? #rx"[\"\\]" l)
                  ;; If there's a comment char, add a newline,
                  ;; just in case:
                  (if (regexp-match? #rx";" l)
                      (string-append l "\\n")
                      l)
                  (regexp-replace #rx";.*$" l ""))])
        (printf "\"~a\"\n" l)
        (let* ([l 
                ;; Remove strings before counting parens:
                (regexp-replace*
                 #rx"\"[^\"]*\""
                 (regexp-replace*
                  #rx"\\\"" l "")
                 "")]
               [l
                ;; Convert sq brackets to parens and remove escaped
                (regexp-replace* #rx"\\[()]"
                                 (brackets->parens l)
                                 "")])
          (let ([parens (for/fold ([parens parens]) ([c (in-string l)])
                          (case c
                            [(#\() (+ parens 1)]
                            [(#\)) (- parens 1)]
                            [else parens]))])
            (if (zero? parens)
                (begin
                  (printf ");\n")
                  (loop #f 0))
                (loop #t parens)))))])))
