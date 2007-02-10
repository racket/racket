
(module stx-util mzscheme
  (require (lib "stx.ss" "syntax"))
  
  (provide (all-defined)
           (all-from (lib "stx.ss" "syntax")))
  
  (define (d->so template datum)
    (if (syntax? template)
        (datum->syntax-object template datum template template)
        datum))
  
  (define-syntax (syntax-copier stx)
    (syntax-case stx ()
      [(syntax-copier hole expr pattern)
       #'(let ([expr-var expr])
           (lambda (in-the-hole)
             (with-syntax ([pattern expr-var])
               (with-syntax ([hole in-the-hole])
                 (syntax/restamp pattern #'pattern expr-var)))))]))
  
  (define-syntax syntax/skeleton
    (syntax-rules ()
      [(syntax/skeleton old-expr pattern)
       (syntax/restamp pattern #'pattern old-expr)]))
  
  
  ;; FIXME: Need to avoid turning syntax lists into syntax pairs
  (define-syntax (syntax/restamp stx)
    (syntax-case stx (...)
      [(syntax/restamp (pa (... ...)) new-expr old-expr)
       #`(let ([new-parts (stx->list new-expr)]
               [old-parts (stx->list old-expr)])
           #;
           (unless (= (length new-parts) (length old-parts))
             (printf "** syntax/restamp~n~s~n" (quote-syntax #,stx))
             (printf "pattern : ~s~n" (syntax-object->datum #'(pa (... ...))))
             (printf "old parts: ~s~n" old-parts)
             (printf "new parts: ~s~n" new-parts))
           (d->so
            old-expr
            (map (lambda (new old) (syntax/restamp pa new old))
                 new-parts
                 old-parts)))]
      [(syntax/restamp (pa . pb) new-expr old-expr)
       #'(let ([na (stx-car new-expr)]
               [nb (stx-cdr new-expr)]
               [oa (stx-car old-expr)]
               [ob (stx-cdr old-expr)])
           (d->so old-expr
                  (cons (syntax/restamp pa na oa)
                        (syntax/restamp pb nb ob))))]
      [(syntax/restamp pvar new-expr old-expr)
       #'new-expr]))

  (define (iota n)
    (let loop ([i 0])
      (if (< i n)
          (cons i (loop (add1 i)))
          null)))

  ;; stx-take : syntax-list number -> (list-of syntax)
  (define (stx-take items n)
    (cond [(zero? n) null]
          [else (cons (stx-car items) (stx-take (stx-cdr items) (sub1 n)))]))

  ;; stx-improper-length : syntax -> number
  (define (stx-improper-length stx)
    (let loop ([stx stx] [n 0])
      (if (stx-pair? stx)
          (loop (stx-cdr stx) (add1 n))
          n)))
  )
