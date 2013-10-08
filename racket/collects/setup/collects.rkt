#lang racket/base
(require racket/list
         racket/string
         pkg/path)

(provide path->module-path
         path->collects-relative
         collects-relative->path)

(define (path->spec p who mode cache)
  (unless (path-string? p)
    (raise-argument-error who "path-string?" p))
  (define simple-p (simplify-path (path->complete-path p) #f))
  (define (make-result new-c-l file)
    (let ([norm-file (regexp-replace #rx"[.]ss$" file ".rkt")])
      (if (eq? mode 'module-path)
          `(lib ,(string-join (append new-c-l (list norm-file))
                              "/"))
          `(collects ,@(map string->bytes/utf-8 new-c-l) ,(string->bytes/utf-8 norm-file)))))
  (define (try-pkg)
    (define-values (pkg subpath pkg-collect) 
      (path->pkg+subpath+collect simple-p #:cache cache))
    (cond
     [pkg
      (define p-l (map path-element->string (reverse (explode-path subpath))))
      (define new-c-l (let ([l (reverse (cdr p-l))])
                        (if pkg-collect
                            (cons pkg-collect l)
                            l)))
      (define c-p (and (pair? new-c-l)
                       (apply collection-file-path (car p-l) new-c-l
                              #:fail (lambda (msg) #f))))
      (and c-p
           (equal? c-p simple-p)
           (make-result new-c-l (car p-l)))]
     [else #f]))
  (define p-l (reverse (explode-path simple-p)))
  (or (and ((length p-l) . > . 2)
           (regexp-match? #rx#"^[-a-zA-Z0-9_+%.]*$" (path-element->bytes (car p-l)))
           ;; Try using path suffixes as library names, checking whether
           ;; `collection-file-path' locates the same path.
           (let ([file (path-element->string (car p-l))])
             (let loop ([c-l null] [p-l (cdr p-l)])
               (cond
                [(null? p-l) #f]
                [(null? (cdr p-l)) #f]
                [(regexp-match? #rx#"^[-a-zA-Z0-9_+%]*$" (path-element->bytes (car p-l)))
                 (define new-c-l (cons (path-element->string (car p-l)) c-l))
                 (define c-p (apply collection-file-path file new-c-l #:fail (lambda (msg) #f)))
                 (if (and c-p
                          (equal? c-p simple-p))
                     (make-result new-c-l file)
                     (loop new-c-l (cdr p-l)))]
                [else #f]))))
      ;; The approach above won't work if a single-collection package's directory
      ;; doesn't match the name of the single collection. In that case, we can
      ;; check whether the directory is in a package, and so on.
      (try-pkg)
      ;; If we get here, no module path reaches the file.
      p))

(define (path->module-path p #:cache [cache #f])
  (path->spec p 'path->module-path 'module-path cache))

(define (path->collects-relative p #:cache [cache #f])
  (path->spec p 'path->collects-relative 'collects-relative cache))

(define (collects-relative->path p)
  (cond
   [(and (pair? p) (list? p)
         ((length p) . >= . 3)
         (eq? 'collects (car p))
         (andmap bytes? (cdr p)))
    (define fn (bytes->string/utf-8 (last p)))
    (define coll (map bytes->string/utf-8 (drop-right (cdr p) 1)))
    (apply collection-file-path 
           fn
           coll
           #:fail (lambda (s)
                    (define l (current-library-collection-paths))
                    (build-path (apply build-path (if (pair? l) (first l) (current-directory)) 
                                       coll)
                                fn)))]
   [(path-string? p) p]
   [(bytes? p) p]
   [else (raise-argument-error
          'collects-relative->path
          (format "~s"
                  '(or/c bytes? path-string?
                         (cons/c 'collects bytes? bytes? (listof bytes?))))
          p)]))
