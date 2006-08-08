
(module stx-util mzscheme
  (require (lib "stx.ss" "syntax"))
  
  (provide (all-defined)
           (all-from (lib "stx.ss" "syntax")))
  
  #;
  (define-syntax (CC stx)
    (syntax-case stx ()
      [(CC HOLE expr pattern)
       #'(lambda (in-the-hole)
           (with-syntax ([pattern expr])
             (with-syntax ([HOLE in-the-hole])
               #'pattern)))]))
  
  
  (define (d->so template datum)
    (let ([template (and (syntax? template) #f)])
      (datum->syntax-object template datum template template)))
  
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
      #;[(syntax/restamp (pa ...) new-expr old-expr)
       (with-syntax ([(na ...) (generate-temporaries #'(pa ...))]
                     [(oa ...) (generate-temporaries #'(pa ...))])
         #'(with-syntax ([(na ...) new-expr]
                         [(oa ...) old-expr])
             (d->so
              old-expr
              (list (syntax/restamp pa #'na #'oa) ...))))]
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

  (define *args* #f)
  
  (define (take-until stxs tail)
    (set! *args* (list stxs tail))
    (let loop ([stxs stxs])
      (if (eq? stxs tail)
          null
          (cons (stx-car stxs) (loop (stx-cdr stxs))))))
  
  (define (stx-improper-length stx)
    (if (stx-pair? stx)
        (add1 (stx-improper-length (stx-cdr stx)))
        0))

  )
