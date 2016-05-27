#lang racket/base

(require racket/contract
         syntax/modresolve)

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
     (cond
      [(path? _pth) (simplify-path _pth #t)]
      [(and (pair? _pth)
            (path? (cadr _pth)))
       (list* 'submod (simplify-path (cadr _pth) #t) (cddr _pth))]
      [else _pth]))))
(define (mpi->path* mpi)
  (hash-ref (MODULE-PATHS) mpi 
            (lambda ()
              (error 'mpi->path* "Cannot locate cache of path for ~S" mpi))))

(define submod-path/c
  (cons/c 'submod
          (cons/c (or/c symbol? path?)
                  (listof symbol?))))

(provide/contract
 [MODULE-PATHS (parameter/c (or/c false/c hash?))]
 [current-module-path (parameter/c (or/c path-string? submod-path/c))]
 [mpi->path! (module-path-index? . -> . (or/c symbol? path? submod-path/c))]
 [mpi->path* (module-path-index? . -> . (or/c symbol? path? pair? submod-path/c))])
