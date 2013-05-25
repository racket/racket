#lang racket
(require racket/file)

(define current-temporary-directory
  (make-parameter #f))

(define (directory-list->directory-list* l)
  (sort (filter-not (compose 
                     (lambda (s)
                       (or (regexp-match #rx"^\\." s)
                           (string=? "compiled" s)
                           (link-exists? s)))
                     path->string)
                    l)
        string<=? #:key path->string #:cache-keys? #t))

(define (directory-list* pth)
  (directory-list->directory-list* (directory-list pth)))

(define (safely-delete-directory pth)
  (with-handlers ([exn:fail? (lambda (x) (void))])
    (delete-directory/files pth)))

(define (make-parent-directory pth)  
  (define pth-dir (path-only pth))
  (make-directory* pth-dir))

(define (write-to-file* v pth)
  (define tpth (make-temporary-file))
  (write-to-file v tpth #:exists 'truncate)
  (make-parent-directory pth)
  (rename-file-or-directory tpth pth #t))

(define (rebase-path from to)
  (define froms (explode-path from))
  (define froms-len (length froms))
  (lambda (pth)
    (define pths (explode-path pth))
    (apply build-path to (list-tail pths froms-len))))

(define (path->string* pth-string)
  (if (string? pth-string)
      pth-string
      (path->string pth-string)))

(provide/contract
 [current-temporary-directory (parameter/c (or/c false/c path-string?))]
 [safely-delete-directory (path-string? . -> . void)]
 [directory-list->directory-list* ((listof path?) . -> . (listof path?))]
 [directory-list* (path-string? . -> . (listof path?))]
 [write-to-file* (any/c path-string? . -> . void)]
 [make-parent-directory (path-string? . -> . void)]
 [rebase-path (path-string? path-string? . -> . (path-string? . -> . path?))]
 [path->string* (path-string? . -> . string?)])
