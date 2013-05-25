#lang racket/base
(require racket/contract/base
         "private/modhelp.rkt")

(define (force-relto relto dir? #:path? [path? #t])
  (let ([relto (if (and (pair? relto)
                        (eq? (car relto) 'submod))
                   (cadr relto)
                   relto)]
        [submod (if (and (pair? relto)
                         (eq? (car relto) 'submod))
                    (cddr relto)
                    null)])
    (cond [(path-string? relto)
           (values (and path?
                        (if dir?
                            (let-values ([(base n d?) (split-path relto)])
                              (when d?
                                (error 'resolve-module-path-index
                                       "given a directory path instead of a file path: ~e" relto))
                              (if (eq? base 'relative)
                                  (or (current-load-relative-directory) (current-directory))
                                  base))
                            relto))
                   submod)]
          [(pair? relto) (values relto submod)]
          [(not dir?)
           (values
            (and path?
                 (error 'resolve-module-path-index
                        "can't resolve \"self\" with non-path relative-to: ~e" relto))
            submod)]
          [(procedure? relto) (force-relto (relto) dir? #:path? path?)]
          [else (values (and path? (current-directory)) submod)])))

(define (path-ss->rkt p)
  (let-values ([(base name dir?) (split-path p)])
    (if (regexp-match #rx"[.]ss$" (path->bytes name))
        (path-replace-suffix p #".rkt")
        p)))

(define (combine-submod v p)
  (if (null? p)
      v
      (list* 'submod v p)))

(define (flatten base orig-p)
  (let loop ([accum '()] [p orig-p])
    (cond
     [(null? p) (combine-submod base (reverse accum))]
     [(equal? (car p) "..")
      (if (null? accum)
          (error 'resolve-module-path "too many \"..\"s: ~s"
                 (combine-submod base orig-p))
          (loop (cdr accum) (cdr p)))]
     [else (loop (cons (car p) accum) (cdr p))])))

(define (resolve-module-path s relto)
  ;; relto should be a complete path, #f, or procedure that returns a
  ;; complete path
  (define (get-dir) (force-relto relto #t))
  (cond [(symbol? s)
         ;; use resolver handler:
         (resolved-module-path-name
          (module-path-index-resolve
           (module-path-index-join s #f)))]
        [(string? s)
         ;; Parse Unix-style relative path string
         (define-values (dir submod) (get-dir))
         (path-ss->rkt
          (apply build-path dir (explode-relpath-string s)))]
        [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
         #f]
        [(or (path? s) (eq? (car s) 'file))
         (let ([p (if (path? s) s (expand-user-path (cadr s)))])
           (define-values (d submod) (get-dir))
           (path-ss->rkt
            (path->complete-path
             p 
             (if (path-string? d)
                 d
                 (or (current-load-relative-directory)
                     (current-directory))))))]
        [(or (eq? (car s) 'lib)
             (eq? (car s) 'quote)
             (eq? (car s) 'planet))
         ;; use resolver handler in this case, too:
         (define-values (d submod) (force-relto relto #f #:path? #f))
         (resolved-module-path-name
          (module-path-index-resolve
           (module-path-index-join s #f)))]
        [(eq? (car s) 'submod)
         (define r (cond
                    [(or (equal? (cadr s) ".")
                         (equal? (cadr s) ".."))
                     (define-values (d submod) (force-relto relto #f))
                     (combine-submod d submod)]
                    [else (resolve-module-path (cadr s) relto)]))
         (define base-submods (if (and (or (equal? (cadr s) ".") 
                                           (equal? (cadr s) ".."))
                                       (pair? r)) 
                                  (cddr r)
                                  null))
         (define base (if (pair? r) (cadr r) r))
         (flatten base (append base-submods 
                               (if (equal? (cadr s) "..") (cdr s) (cddr s))))]
        [else #f]))

(define (resolve-module-path-index mpi relto)
  ;; relto must be a complete path
  (let-values ([(path base) (module-path-index-split mpi)])
    (if path
        (resolve-module-path path (resolve-possible-module-path-index base relto))
        (let ()
          (define sm (module-path-index-submodule mpi))
          (define-values (dir submod) (force-relto relto #f))
          (combine-submod (path-ss->rkt dir) (if (and sm submod)
                                                 (append submod sm)
                                                 (or sm submod)))))))

(define (resolve-possible-module-path-index base relto)
  (cond [(module-path-index? base)
         (resolve-module-path-index base relto)]
        [(and (resolved-module-path? base)
              (path? (resolved-module-path-name base)))
         (resolved-module-path-name base)]
        [relto relto]
        [else #f]))


(define rel-to-path-string/c
  (or/c path-string? (cons/c 'submod (cons/c path-string? (listof symbol?)))))

(define rel-to-path-string/thunk/#f
  (or/c rel-to-path-string/c (-> rel-to-path-string/c) false/c))

(provide/contract
 [resolve-module-path (module-path?
                       rel-to-path-string/thunk/#f
                       . -> . (or/c path? symbol? 
                                    (cons/c 'submod (cons/c (or/c path? symbol?) 
                                                            (listof symbol?)))))]
 [resolve-module-path-index ((or/c symbol? module-path-index?)
                             rel-to-path-string/thunk/#f
                             . -> . (or/c path? symbol? 
                                          (cons/c 'submod (cons/c (or/c path? symbol?)
                                                                  (listof symbol?)))))])
