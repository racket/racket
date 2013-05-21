#lang racket/base
(require racket/list
         racket/string)

(provide path->module-path
         path->collects-relative
         collects-relative->path)

(define (path->spec p who mode)
  (unless (path-string? p)
    (raise-argument-error who "path-string?" p))
  (define simple-p (simplify-path (path->complete-path p) #f))
  (define p-l (reverse (explode-path simple-p)))
  (or (and ((length p-l) . > . 2)
           (regexp-match? #rx#"^[-a-zA-Z0-9_+%.]*$" (path-element->bytes (car p-l)))
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
                     (let ([norm-file (regexp-replace #rx"[.]ss$" file ".rkt")])
                       (if (eq? mode 'module-path)
                           `(lib ,(string-join (append new-c-l (list norm-file))
                                               "/"))
                           `(collects ,@(map string->bytes/utf-8 new-c-l) ,(string->bytes/utf-8 norm-file))))
                     (loop new-c-l (cdr p-l)))]
                [else #f]))))
      p))

(define (path->module-path p)
  (path->spec p 'path->module-path 'module-path))

(define (path->collects-relative p)
  (path->spec p 'path->collects-relative 'collects-relative))

(define (collects-relative->path p)
  (cond
   [(and (pair? p) (list? p)
         ((length p) . >= . 3)
         (eq? 'collects (car p))
         (andmap bytes? (cdr p)))
    (apply collection-file-path 
           (bytes->string/utf-8 (last p))
           (map bytes->string/utf-8 (drop-right (cdr p) 1)))]
   [(path-string? p) p]
   [(bytes? p) p]
   [else (raise-argument-error
          'collects-relative->path
          (format "~s"
                  '(or/c bytes? path-string?
                         (cons/c 'collects bytes? bytes? (listof bytes?))))
          p)]))
