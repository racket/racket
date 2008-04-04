#lang scheme/base
(provide mpi->list
         mpi->string)

(define (mpi->list mpi)
  (cond [(module-path-index? mpi)
         (let-values ([(path relto) (module-path-index-split mpi)])
           (cond [(not path) null]
                 [(not relto) (list path)]
                 [else (cons path (mpi->list relto))]))]
        [(not mpi) null]
        [else (list mpi)]))

;; mpi->string : module-path-index -> string
(define (mpi->string mpi)
  (if (module-path-index? mpi)
      (let ([mps (mpi->list mpi)])
        (cond [(pair? mps)
               (apply string-append
                      (format "~s" (car mps))
                      (map (lambda (x) (format " <= ~s" x)) (cdr mps)))]
              [(null? mps) "this module"]))
      (format "~s" mpi)))
