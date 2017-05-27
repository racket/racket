#lang racket/base
(require ffi/unsafe
         (only-in '#%paramz
                  security-guard-check-file
                  security-guard-check-file-link
                  security-guard-check-network))

(provide security-guard-check-file
         security-guard-check-file-link
         security-guard-check-network

         _file/guard
         _file/r
         _file/rw)

(define (_file/guard modes [who '_file/guard])
  (unless (symbol? who)
    (raise-argument-error '_file/guard "symbol?" who))
  (make-ctype
   _path
   (lambda (p)
     (let ([cp (cleanse-path (path->complete-path p))])
       (security-guard-check-file who cp modes)
       cp))
   #f))

(define _file/r (_file/guard '(read) '_file/r))
(define _file/rw (_file/guard '(read write) '_file/rw))
