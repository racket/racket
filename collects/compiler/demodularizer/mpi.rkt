#lang racket
(require syntax/modresolve)

(define current-module-path (make-parameter #f))

(define (mpi->string modidx)
  (cond
    [(symbol? modidx) modidx]
    [else 
     (mpi->path! modidx)]))

(define MODULE-PATHS (make-parameter #f))
(define (mpi->path! mpi)
  (hash-ref! 
   (MODULE-PATHS) mpi 
   (lambda ()
     (define _pth
       (resolve-module-path-index mpi (current-module-path)))
     (if (path? _pth)
         (simplify-path _pth #t)
         _pth))))
(define (mpi->path* mpi)
  (hash-ref (MODULE-PATHS) mpi 
            (lambda ()
              (error 'mpi->path* "Cannot locate cache of path for ~S" mpi))))

(provide/contract
 [MODULE-PATHS (parameter/c (or/c false/c hash?))]
 [current-module-path (parameter/c path-string?)]
 [mpi->path! (module-path-index? . -> . (or/c symbol? path?))]
 [mpi->path* (module-path-index? . -> . (or/c symbol? path?))])
