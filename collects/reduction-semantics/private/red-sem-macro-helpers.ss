(module red-sem-macro-helpers mzscheme
  
  (provide extract-names)

  (require (lib "match.ss"))
  
  (define (extract-names stx)
    (let ([dup-names
           (let loop ([sexp (syntax-object->datum stx)]
                      [names null])
             (match sexp
               [`(name ,(and sym (? symbol?)) ,pat)
                (loop pat (cons sym names))]
               [`(in-hole* ,(and sym (? symbol?)) ,pat1 ,pat2)
                (loop pat1
                      (loop pat2
                            (cons sym names)))]
               [`(in-hole ,pat1 ,pat2)
                (loop pat1
                      (loop pat2
                            (cons 'hole names)))]
               [(? list?)
                (let i-loop ([sexp sexp]
                             [names names])
                  (cond
                    [(null? sexp) names]
                    [else (i-loop (cdr sexp) (loop (car sexp) names))]))]
               [else names]))]
          [ht (make-hash-table)])
      (for-each (lambda (name) (hash-table-put! ht name #f)) dup-names)
      (hash-table-map ht (lambda (x y) x)))))
