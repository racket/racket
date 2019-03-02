(compile-compressed #f)
(define compile-cross? #f)

(define-values (src dest deps)
  (let loop ([args (command-line-arguments)])
    (cond
      [(and (pair? args)
            (equal? (car args) "--compress"))
       (compile-compressed #t)
       (loop (cdr args))]
      [(and (pair? args)
            (equal? (car args) "--cross"))
       (set! compile-cross? #t)
       (loop (cdr args))]
      [(null? args)
       (error 'to-vfasl "missing src argument")]
      [(null? (cdr args))
       (error 'to-vfasl "missing dest argument")]
      [else
       (values (car args) (cadr args) (cddr args))])))

(cond
 [compile-cross?
  (printf "Cross-compile cannot convert to vfasl; leaving as-is\n")
  (let ([i (open-file-input-port src)]
        [o (open-file-output-port dest (file-options no-fail))])
    (let loop ()
      (define c (get-u8 i))
      (unless (eof-object? c)
        (put-u8 o c)
        (loop)))
    (close-port i)
    (close-port o))]
 [else
  (vfasl-convert-file src dest deps)])
