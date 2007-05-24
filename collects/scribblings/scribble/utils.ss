
(module utils mzscheme
  (require "../struct.ss"
           "../manual.ss"
           (prefix scheme: "../scheme.ss")
           (prefix scribble: "../reader.ss")
           (lib "string.ss"))

  (provide at
           litchar/lines
           scribble-examples)

  (define at "@")

  (define (litchar/lines s)
    (let ([strs (regexp-split #rx"\n" s)])
      (if (= 1 (length strs))
          (litchar s)
          (make-table
           #f
           (map (lambda (s)
                  (list (make-flow (list (make-paragraph (list (litchar s)))))))
                strs)))))

  (define (as-flow e)
    (make-flow (list (if (flow-element? e)
                         e
                         (make-paragraph (list e))))))

  (define spacer (hspace 2))

  (define (scribble-examples . lines)
    (make-table
     #f
     (map (lambda (line)
            (let ([line (if (string? line)
                            (list (litchar/lines line)
                                  (scheme:to-element (scribble:read (open-input-string line))))
                            line)])
              (list (as-flow spacer)
                    (as-flow (if line (car line) ""))
                    (as-flow (if line (make-paragraph (list spacer "reads as" spacer)) ""))
                    (as-flow (if line (cadr line) "")))))
          lines))))
