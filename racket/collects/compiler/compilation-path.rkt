#lang racket/base

(provide get-compilation-dir+name
         get-compilation-dir
         get-compilation-bytecode-file)

(define (do-get-compilation-dir+name who path modes roots)
  ;; Check arguments
  (unless (path-string? path)
    (raise-argument-error who "path-string?" path))
  (unless (and (list? modes)
               (pair? modes)
               (andmap (lambda (p)
                         (and (path-string? p)
                              (relative-path? p)))
                       modes))
    (raise-argument-error who "(non-empty-listof (and/c path-string? relative-path?))" modes))
  (unless (and (list? roots)
               (pair? roots)
               (andmap (lambda (p)
                         (or (path-string? p) (eq? p 'same)))
                       roots))
    (raise-argument-error who "(non-empty-listof (or/c path-string? 'same))" roots))
  ;; Function to try one combination:
  (define (get-one mode root)
    (let-values ([(base name must-be-dir?) (split-path path)])
      (values 
       (cond
        [(eq? 'relative base) 
         (cond
          [(eq? root 'same) mode]
          [else (build-path root mode)])]
        [else (build-path (cond
                           [(eq? root 'same) base]
                           [(relative-path? root) (build-path base root)]
                           [else (reroot-path base root)])
                          mode)])
       name)))
  ;; Try first root:
  (define-values (p n) (get-one (car modes) (car roots)))
  (if (or (and (null? (cdr roots))
               (null? (cdr modes)))
          (file-exists? (path-add-suffix (build-path p n) #".zo")))
      ;; Only root or first has a ".zo" file:
      (values p n)
      (let loop ([roots (cdr roots)])
        (cond
         [(null? roots) 
          ;; No roots worked, so assume the first mode + root:
          (values p n)]
         [else
          ;; Check next root:
          (let mloop ([modes modes])
            (cond
             [(null? modes) (loop (cdr roots))]
             [else
              (define-values (p n) (get-one (car modes) (car roots)))
              (if (file-exists? (path-add-suffix (build-path p n) #".zo"))
                  (values p n)
                  (mloop (cdr modes)))]))]))))

(define (get-compilation-dir+name path 
                                  #:modes [modes (use-compiled-file-paths)]
                                  #:roots [roots (current-compiled-file-roots)])
  (do-get-compilation-dir+name 'get-compilation-dir+name path modes roots))
  
(define (get-compilation-dir path 
                             #:modes [modes (use-compiled-file-paths)]
                             #:roots [roots (current-compiled-file-roots)])
  (let-values ([(dir name) (do-get-compilation-dir+name 'get-compilation-dir path modes roots)])
    dir))

(define (get-compilation-bytecode-file path 
                                       #:modes [modes (use-compiled-file-paths)]
                                       #:roots [roots (current-compiled-file-roots)])
  (let-values ([(dir name) (do-get-compilation-dir+name 'get-compilation-bytecode-file path modes roots)])
    (build-path dir (path-add-suffix name #".zo"))))
