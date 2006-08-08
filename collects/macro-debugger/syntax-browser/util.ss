
(module util mzscheme
  (require (lib "class.ss"))
  (provide with-unlock
           mpi->string
           mpi->list)
  
  (define-syntax with-unlock
    (syntax-rules ()
      [(with-unlock text . body)
       (let* ([t text]
              [locked? (send t is-locked?)])
         (send t lock #f)
         (let () . body)
         (send t lock locked?))]))

  (define (mpi->string mpi)
    (if (module-path-index? mpi)
        (let ([mps (mpi->list mpi)])
          (cond [(and (pair? mps) (pair? (cdr mps)))
                 (apply string-append
                        (format "~s" (car mps))
                        (map (lambda (x) (format " <= ~s" x)) (cdr mps)))]
                [(and (pair? mps) (null? (cdr mps)))
                 (format "~s" (car mps))]
                [(null? mps) "self"]))
        (format "~s" mpi)))

  (define (mpi->list mpi)
    (if mpi
        (let-values ([(path rel) (module-path-index-split mpi)])
          (if (and (pair? path) (memq (car path) '(file lib planet)))
              (cons path null)
              (cons path (mpi->list rel))))
        '()))

;   ;; mpi->string : module-path-index -> string
;   ;; Human-readable form of module-path-index
;   (define (mpi->string x)
;     (cond [(module-path-index? x)
;            (let-values ([(path base) (module-path-index-split x)])
;              (cond [(eq? path #f)
;                     "self module"]
;                    [(eq? base #f)
;                     (format "top-level => ~a" path)]
;                    [else
;                     (format "~a => ~a" (mpi->string base) path)]))]
;           [else x]))

  )