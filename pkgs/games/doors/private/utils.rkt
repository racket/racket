
(module utils racket
  (provide alternates
           interleave)

  (define (alternates l)
    (let loop ([l l])
      (cond
       [(null? l) (values null null)]
       [(null? (cdr l)) (values l null)]
       [else
        (let-values ([(as bs) (loop (cddr l))])
          (values (cons (car l) as)
                  (cons (cadr l) bs)))])))

  (define (interleave l1 l2)
    (cond
     [(null? l2) l1]
     [else (list* (car l1)
                  (car l2)
                  (interleave (cdr l1) (cdr l2)))])))
