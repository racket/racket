#lang racket/base

(provide get-compilation-dir+name
         get-compilation-dir
         get-compilation-bytecode-file)

(define (do-get-compilation-dir+name who path modes roots default-root)
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
  (unless (or (eq? default-root 'same) (path-string? default-root))
    (raise-argument-error who "(or/c path-string? 'same)" default-root))
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
  (define orig-root (car roots))
  (define-values (p n) (get-one (car modes) orig-root))
  (if (or (and (null? (cdr roots))
               (null? (cdr modes)))
          (file-exists? (path-add-suffix (build-path p n) #".zo")))
      ;; Only root or first has a ".zo" file:
      (values p n)
      (let loop ([roots (cdr roots)])
        (cond
         [(null? roots) 
          ;; No roots worked, so use the default root
          (if (equal? default-root orig-root)
              (values p n)
              (get-one (car modes) default-root))]
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
                                  #:roots [roots (current-compiled-file-roots)]
                                  #:default-root [default-root (and (pair? roots) (car roots))])
  (do-get-compilation-dir+name 'get-compilation-dir+name path modes roots default-root))
  
(define (get-compilation-dir path 
                             #:modes [modes (use-compiled-file-paths)]
                             #:roots [roots (current-compiled-file-roots)]
                             #:default-root [default-root (and (pair? roots) (car roots))])
  (let-values ([(dir name) (do-get-compilation-dir+name 'get-compilation-dir path modes roots default-root)])
    dir))

(define (get-compilation-bytecode-file path 
                                       #:modes [modes (use-compiled-file-paths)]
                                       #:roots [roots (current-compiled-file-roots)]
                                       #:default-root [default-root (and (pair? roots) (car roots))])
  (let-values ([(dir name) (do-get-compilation-dir+name 'get-compilation-bytecode-file path modes roots default-root)])
    (build-path dir (path-add-suffix name #".zo"))))
