#lang racket/base

(require
 "source-transform.rkt"
) ; end require

(provide
 parse-template-string
) ; end provide

(define (parse-template-string input)
  (unless (string? input)
    (raise-argument-error 'parse-template-string "string?" input)
  ) ; end unless string?
  (define length (string-length input))
  (let loop ((index 0)
             (strings '())
             (expressions '())
             (static-out (open-output-string))
        ) ; end loop bindings
    (cond
      ((= index length)
       (values (reverse (cons (get-output-string static-out) strings))
               (reverse expressions)
       ) ; end values
      ) ; end end of input
      (else
       (define ch (string-ref input index))
       (cond
         ((char=? ch #\{)
          (cond
            ((and (< (add1 index) length)
                  (char=? (string-ref input (add1 index)) #\{)
             ) ; end and
             (write-char #\{ static-out)
             (loop (+ index 2)
                   strings
                   expressions
                   static-out
             ) ; end loop
            ) ; end escaped left brace
            (else
             (define-values (expression next-index)
               (read-interpolation input (add1 index))
             ) ; end define-values
             (define static-part (get-output-string static-out))
             (loop next-index
                   (cons static-part strings)
                   (cons expression expressions)
                   (open-output-string)
             ) ; end loop
            ) ; end interpolation
          ) ; end cond left brace
         ) ; end left brace case
         ((char=? ch #\})
          (cond
            ((and (< (add1 index) length)
                  (char=? (string-ref input (add1 index)) #\})
             ) ; end and
             (write-char #\} static-out)
             (loop (+ index 2)
                   strings
                   expressions
                   static-out
             ) ; end loop
            ) ; end escaped right brace
            (else
             (raise-arguments-error 'parse-template-string
                                    "unmatched } in template string; use }} for a literal }"
                                    "input"
                                    input
                                    "index"
                                    index
             ) ; end raise-arguments-error
            ) ; end unmatched right brace
          ) ; end cond right brace
         ) ; end right brace case
         (else
          (write-char ch static-out)
          (loop (add1 index)
                strings
                expressions
                static-out
          ) ; end loop
         ) ; end ordinary character
       ) ; end cond char dispatch
      ) ; end more input
    ) ; end cond
  ) ; end let loop
) ; end define parse-template-string

(define (read-interpolation input start-index)
  (define length (string-length input))
  (define expression-out (open-output-string))
  (let loop ((index start-index)
             (brace-depth 0)
        ) ; end loop bindings
    (cond
      ((= index length)
       (raise-arguments-error 'parse-template-string
                              "unclosed interpolation in template string"
                              "input"
                              input
                              "index"
                              start-index
       ) ; end raise-arguments-error
      ) ; end end of input
      (else
       (define ch (string-ref input index))
       (cond
         ((char=? ch #\")
          (define next-index (find-racket-string-end input index))
          (write-string (substring input index next-index) expression-out)
          (loop next-index
                brace-depth
          ) ; end loop
         ) ; end racket string
         ((template-prefix-at? input index)
          (define next-index (find-template-source-end input (add1 index)))
          (write-string (substring input index next-index) expression-out)
          (loop next-index
                brace-depth
          ) ; end loop
         ) ; end nested template
         ((char=? ch #\{)
          (write-char ch expression-out)
          (loop (add1 index)
                (add1 brace-depth)
          ) ; end loop
         ) ; end nested brace
         ((char=? ch #\})
          (cond
            ((zero? brace-depth)
             (define expression (get-output-string expression-out))
             (when (string-blank? expression)
               (raise-arguments-error 'parse-template-string
                                      "empty interpolation is not allowed"
                                      "input"
                                      input
                                      "index"
                                      start-index
               ) ; end raise-arguments-error
             ) ; end when empty expression
             (values expression (add1 index))
            ) ; end interpolation close
            (else
             (write-char ch expression-out)
             (loop (add1 index)
                   (sub1 brace-depth)
             ) ; end loop
            ) ; end nested brace close
          ) ; end cond right brace
         ) ; end right brace
         (else
          (write-char ch expression-out)
          (loop (add1 index)
                brace-depth
          ) ; end loop
         ) ; end expression character
        ) ; end cond char dispatch
      ) ; end more input
    ) ; end cond
  ) ; end let loop
) ; end define read-interpolation

(define (string-blank? text)
  (let loop ((index 0))
    (cond
      ((= index (string-length text)) #t)
      ((char-whitespace? (string-ref text index))
       (loop (add1 index))
      ) ; end whitespace character
      (else #f)
    ) ; end cond
  ) ; end let loop
) ; end define string-blank?
