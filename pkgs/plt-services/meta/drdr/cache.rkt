#lang racket
(require "path-utils.rkt")

; (symbols 'always 'cache 'no-cache)
(define cache/file-mode (make-parameter 'cache))
(define (cache/file pth thnk)
  (define mode (cache/file-mode))
  (define (recompute!)
    (define v (thnk))
    (write-cache! pth v)
    v)
  (case mode
    [(always) (recompute!)]
    [(cache no-cache)
     (with-handlers 
         ([exn:fail?
           (lambda (x)
             (case mode
               [(no-cache) (error 'cache/file "No cache available: ~a" pth)]
               [(cache always)
                #;(printf "cache/file: running ~S for ~a\n" thnk pth)
                (recompute!)]))])
       (read-cache pth))]))

(define (cache/file/timestamp pth thnk)
  (cache/file 
   pth
   (lambda ()
     (thnk)
     (current-seconds)))
  (void))

(require "archive.rkt"
         "dirstruct.rkt")

(define (consult-archive pth)
  (define rev (path->revision pth))
  (define archive-path (revision-archive rev))
  (define file-bytes
    (archive-extract-file archive-path pth))
  (with-input-from-bytes file-bytes read))

(define (consult-archive/directory-list* pth)
  (define rev (path->revision pth))
  (define archive-path (revision-archive rev))
  (directory-list->directory-list* (archive-directory-list archive-path pth)))

(define (consult-archive/directory-exists? pth)
  (define rev (path->revision pth))
  (define archive-path (revision-archive rev))
  (archive-directory-exists? archive-path pth))

(define (cached-directory-list* dir-pth)
  (if (directory-exists? dir-pth)
      (directory-list* dir-pth)
      (or (with-handlers ([exn:fail? (lambda _ #f)]) (consult-archive/directory-list* dir-pth))
          (error 'cached-directory-list* "Directory list is not cached: ~e" dir-pth))))

(define (cached-directory-exists? dir-pth)
  (if (file-exists? dir-pth)
      #f
      (or (directory-exists? dir-pth)
          (with-handlers ([exn:fail? (lambda _ #f)]) (consult-archive/directory-exists? dir-pth)))))

(define (read-cache pth)
  (if (file-exists? pth)
      (file->value pth)
      (or (with-handlers ([exn:fail? (lambda _ #f)]) (consult-archive pth))
          (error 'read-cache "File is not cached: ~e" pth))))
(define (read-cache* pth)
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (read-cache pth)))
(define (write-cache! pth v)
  (write-to-file* v pth))
(define (delete-cache! pth)
  (with-handlers ([exn:fail? void])
    (delete-file pth)))

(provide/contract
 [cache/file-mode (parameter/c (symbols 'always 'cache 'no-cache))]
 [cache/file (path-string? (-> any/c) . -> . any/c)]
 [cache/file/timestamp (path-string? (-> void) . -> . void)]
 [cached-directory-list* (path-string? . -> . (listof path-string?))]
 [cached-directory-exists? (path-string? . -> . boolean?)]
 [read-cache (path-string? . -> . any/c)]
 [read-cache* (path-string? . -> . any/c)]
 [write-cache! (path-string? any/c . -> . void)]
 [delete-cache! (path-string? . -> . void)])
