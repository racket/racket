(define compressed? #f)
(define dest ".")

(define-values (petite.boot scheme.boot racket.boot)
  (let loop ([args (command-line-arguments)])
    (cond
      [(and (pair? args)
            (equal? (car args) "--compress"))
       (set! compressed? #t)
       (loop (cdr args))]
      [(and (pair? args)
            (equal? (car args) "--xpatch")
            (pair? (cdr args)))
       (load (cadr args))
       (loop (cddr args))]
      [(and (pair? args)
            (equal? (car args) "--dest")
            (pair? (cdr args)))
       (set! dest (cadr args))
       (loop (cddr args))]
      [(null? args)
       (error 'to-vfasl "missing petite.boot argument")]
      [(null? (cdr args))
       (error 'to-vfasl "missing scheme.boot argument")]
      [(null? (cddr args))
       (error 'to-vfasl "missing racket.boot argument")]
      [(not (null? (cdddr args)))
       (error 'to-vfasl "extra arguments")]
      [else
       (values (car args) (cadr args) (caddr args))])))

(fasl-compressed compressed?)

(define (many fmt)
  (map (lambda (i)
         (format fmt i))
       (iota 10)))

(define (generate in.boot out.boot c~a reg~a start-index)
  (printf "Converting ~a to ~a and ~a\n" in.boot out.boot c~a)
  (flush-output-port)
  (time
   (pbchunk-convert-file in.boot
                         (path-build dest out.boot)
                         (map (lambda (c) (path-build dest c)) (many c~a))
                         (many reg~a)
                         start-index)))

(define post-petite-index (generate petite.boot
                                    "petite-pbchunk.boot"
                                    "petite~a.c"
                                    "register_petite~a_pbchunks"
                                    0))
(define post-scheme-index (generate scheme.boot
                                    "scheme-pbchunk.boot"
                                    "scheme~a.c"
                                    "register_scheme~a_pbchunks"
                                    post-petite-index))
(define post-racket-index (generate racket.boot
                                    "racket-pbchunk.boot"
                                    "racket~a.c"
                                    "register_racket~a_pbchunks"
                                    post-scheme-index))
