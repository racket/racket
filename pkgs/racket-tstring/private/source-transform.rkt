#lang racket/base

(provide
 transform-template-prefixes
 find-racket-string-end
 find-template-source-end
 template-prefix-at?
) ; end provide

(define (transform-template-prefixes source)
  (define length (string-length source))
  (define out (open-output-string))
  (let loop ((index 0))
    (cond
      ((= index length)
       (get-output-string out)
      ) ; end end of source
      (else
       (define ch (string-ref source index))
       (cond
         ((char=? ch #\")
          (define next-index (copy-racket-string source index out))
          (loop next-index)
         ) ; end ordinary string
         ((char=? ch #\;)
          (define next-index (copy-line-comment source index out))
          (loop next-index)
         ) ; end line comment
         ((and (char=? ch #\#)
               (< (add1 index) length)
               (char=? (string-ref source (add1 index)) #\|)
          ) ; end and
          (define next-index (copy-block-comment source index out))
          (loop next-index)
         ) ; end block comment
         ((template-prefix-at? source index)
          (define next-index (copy-template-form source index out))
          (loop next-index)
         ) ; end template prefix
         (else
          (write-char ch out)
          (loop (add1 index))
         ) ; end ordinary character
       ) ; end cond char dispatch
      ) ; end more source
    ) ; end cond
  ) ; end let loop
) ; end define transform-template-prefixes

(define (template-prefix-at? source index)
  (define length (string-length source))
  (and (< (add1 index) length)
       (or (char=? (string-ref source index) #\f)
           (char=? (string-ref source index) #\t)
       ) ; end or
       (template-prefix-start? source index)
       (char=? (string-ref source (add1 index)) #\")
  ) ; end and
) ; end define template-prefix-at?

(define (template-prefix-start? source index)
  (or (= index 0)
      (delimiter? (string-ref source (sub1 index)))
  ) ; end or
) ; end define template-prefix-start?

(define (delimiter? ch)
  (or (char-whitespace? ch)
      (memv ch '(#\( #\) #\[ #\] #\{ #\} #\' #\` #\,))
  ) ; end or
) ; end define delimiter?

(define (copy-template-form source index out)
  (define prefix (string-ref source index))
  (define literal-start (add1 index))
  (define literal-end (find-template-source-end source literal-start))
  (write-string
   (if (char=? prefix #\f)
       "(fpl "
       "(tpl "
   ) ; end if
   out
  ) ; end write-string
  (write-string (template-content->string-literal-source
                 (substring source (add1 literal-start) (sub1 literal-end))
                ) ; end template-content->string-literal-source
                out
  ) ; end write-string
  (write-string ")" out)
  literal-end
) ; end define copy-template-form

(define (template-content->string-literal-source content)
  (define out (open-output-string))
  (write-char #\" out)
  (let loop ((index 0))
    (cond
      ((= index (string-length content))
       (write-char #\" out)
       (get-output-string out)
      ) ; end end of content
      (else
       (define ch (string-ref content index))
       (cond
         ((char=? ch #\\)
          (write-char ch out)
          (when (< (add1 index) (string-length content))
            (write-char (string-ref content (add1 index)) out)
          ) ; end when next character
          (loop (if (< (add1 index) (string-length content))
                    (+ index 2)
                    (add1 index)
                ) ; end if
          ) ; end loop
         ) ; end escape
         ((char=? ch #\")
          (write-string "\\\"" out)
          (loop (add1 index))
         ) ; end quote
         (else
          (write-char ch out)
          (loop (add1 index))
         ) ; end ordinary character
       ) ; end cond char dispatch
      ) ; end more content
    ) ; end cond
  ) ; end let loop
) ; end define template-content->string-literal-source

(define (copy-racket-string source index out)
  (define next-index (find-racket-string-end source index))
  (write-string (substring source index next-index) out)
  next-index
) ; end define copy-racket-string

(define (find-racket-string-end source start-index)
  (define length (string-length source))
  (let loop ((index (add1 start-index)))
    (cond
      ((= index length)
       (raise-arguments-error 'racket-tstring-reader
                              "unclosed string literal"
                              "index"
                              start-index
       ) ; end raise-arguments-error
      ) ; end end of source
      (else
       (define ch (string-ref source index))
       (cond
         ((char=? ch #\\)
          (loop (if (< (add1 index) length)
                    (+ index 2)
                    (add1 index)
                ) ; end if
          ) ; end loop
         ) ; end escape
         ((char=? ch #\")
          (add1 index)
         ) ; end close quote
         (else
          (loop (add1 index))
         ) ; end ordinary string character
       ) ; end cond char dispatch
      ) ; end more source
    ) ; end cond
  ) ; end let loop
) ; end define find-racket-string-end

(define (find-template-source-end source quote-index)
  (define length (string-length source))
  (let loop ((index (add1 quote-index)))
    (cond
      ((= index length)
       (raise-arguments-error 'racket-tstring-reader
                              "unclosed template string literal"
                              "index"
                              quote-index
       ) ; end raise-arguments-error
      ) ; end end of source
      (else
       (define ch (string-ref source index))
       (cond
         ((char=? ch #\\)
          (loop (if (< (add1 index) length)
                    (+ index 2)
                    (add1 index)
                ) ; end if
          ) ; end loop
         ) ; end escape
         ((char=? ch #\")
          (add1 index)
         ) ; end close template quote
         ((and (char=? ch #\{)
               (< (add1 index) length)
               (char=? (string-ref source (add1 index)) #\{)
          ) ; end and
          (loop (+ index 2))
         ) ; end literal left brace
         ((and (char=? ch #\})
               (< (add1 index) length)
               (char=? (string-ref source (add1 index)) #\})
          ) ; end and
          (loop (+ index 2))
         ) ; end literal right brace
         ((char=? ch #\{)
          (loop (find-interpolation-source-end source (add1 index)))
         ) ; end interpolation
         (else
          (loop (add1 index))
         ) ; end text character
       ) ; end cond char dispatch
      ) ; end more source
    ) ; end cond
  ) ; end let loop
) ; end define find-template-source-end

(define (find-interpolation-source-end source start-index)
  (define length (string-length source))
  (let loop ((index start-index)
             (brace-depth 0)
        ) ; end loop bindings
    (cond
      ((= index length)
       (raise-arguments-error 'racket-tstring-reader
                              "unclosed interpolation in template string"
                              "index"
                              start-index
       ) ; end raise-arguments-error
      ) ; end end of source
      (else
       (define ch (string-ref source index))
       (cond
         ((char=? ch #\")
          (loop (find-racket-string-end source index)
                brace-depth
          ) ; end loop
         ) ; end racket string
         ((template-prefix-at? source index)
          (loop (find-template-source-end source (add1 index))
                brace-depth
          ) ; end loop
         ) ; end nested template
         ((char=? ch #\{)
          (loop (add1 index)
                (add1 brace-depth)
          ) ; end loop
         ) ; end nested brace
         ((char=? ch #\})
          (if (zero? brace-depth)
              (add1 index)
              (loop (add1 index)
                    (sub1 brace-depth)
              ) ; end loop
          ) ; end if
         ) ; end right brace
         (else
          (loop (add1 index)
                brace-depth
          ) ; end loop
         ) ; end expression character
       ) ; end cond char dispatch
      ) ; end more source
    ) ; end cond
  ) ; end let loop
) ; end define find-interpolation-source-end

(define (copy-line-comment source index out)
  (define length (string-length source))
  (let loop ((index index))
    (cond
      ((= index length)
       index
      ) ; end end of source
      (else
       (define ch (string-ref source index))
       (write-char ch out)
       (if (char=? ch #\newline)
           (add1 index)
           (loop (add1 index))
       ) ; end if
      ) ; end more source
    ) ; end cond
  ) ; end let loop
) ; end define copy-line-comment

(define (copy-block-comment source index out)
  (define length (string-length source))
  (let loop ((index index)
             (depth 0)
        ) ; end loop bindings
    (cond
      ((= index length)
       index
      ) ; end end of source
      ((and (< (add1 index) length)
            (char=? (string-ref source index) #\#)
            (char=? (string-ref source (add1 index)) #\|)
       ) ; end and
       (write-string "#|" out)
       (loop (+ index 2)
             (add1 depth)
       ) ; end loop
      ) ; end nested open comment
      ((and (< (add1 index) length)
            (char=? (string-ref source index) #\|)
            (char=? (string-ref source (add1 index)) #\#)
       ) ; end and
       (write-string "|#" out)
       (define next-depth (sub1 depth))
       (if (zero? next-depth)
           (+ index 2)
           (loop (+ index 2)
                 next-depth
           ) ; end loop
       ) ; end if
      ) ; end close comment
      (else
       (write-char (string-ref source index) out)
       (loop (add1 index)
             depth
       ) ; end loop
      ) ; end ordinary comment character
    ) ; end cond
  ) ; end let loop
) ; end define copy-block-comment
