
(module util mzscheme
  (require (lib "class.ss"))
  (provide with-unlock
           make-text-port
           mpi->string
           mpi->list)

  ;; with-unlock SYNTAX (expression)
  ;; (with-unlock text-expression . body)
  (define-syntax with-unlock
    (syntax-rules ()
      [(with-unlock text . body)
       (let* ([t text]
              [locked? (send t is-locked?)])
         (send t lock #f)
         (begin0 (let () . body)
                 (send t lock locked?)))]))

  ;; make-text-port : text (-> number) -> port
  ;; builds a port from a text object.  
  (define (make-text-port text end-position)
    (make-output-port #f
                      always-evt
                      (lambda (s start end flush? enable-break?)
                        (send text insert
                              (bytes->string/utf-8 s #f start end)
                              (end-position))
                        (- end start))
                      void
                      (lambda (special buffer? enable-break?)
                        (send text insert special (end-position))
                        #t)))

  ;; mpi->string : module-path-index -> string
  (define (mpi->string mpi)
    (if (module-path-index? mpi)
        (let ([mps (mpi->list mpi)])
          (cond [(and (pair? mps) (pair? (cdr mps)))
                 (apply string-append
                        (format "~s" (car mps))
                        (map (lambda (x) (format " <= ~s" x)) (cdr mps)))]
                [(and (pair? mps) (null? (cdr mps)))
                 (format "~s" (car mps))]
                [(null? mps) "this module"]))
        (format "~s" mpi)))

  ;; mpi->list : module-path-index -> (list-of module-spec)
  (define (mpi->list mpi)
    (cond [(module-path-index? mpi)
           (let-values ([(path rel) (module-path-index-split mpi)])
             (cond [(and (pair? path) (memq (car path) '(file lib planet)))
                    (cons path null)]
                   [path
                    (cons path (mpi->list rel))]
                   [else '()]))]
          [(not mpi)
           '()]
          [else (list mpi)]))
  )
