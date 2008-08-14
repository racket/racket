#lang scheme
(require web-server/servlet
         web-server/servlet-env)

(provide 
 (all-from-out web-server/servlet)
 (except-out (all-from-out scheme) #%module-begin)
 (rename-out [web-module-begin #%module-begin]))

(define extra-files-path #f)
(define launch-browser? #t)

(provide/contract
 (static-files-path ((or/c string? path?) . -> . void?)))
(define (static-files-path path)
  (set! extra-files-path 
        (if (path? path) 
            path
            (string->path path))))

(provide no-web-browser)
(define (no-web-browser)
  (set! launch-browser? false))

(define-syntax (web-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (with-syntax ([start (datum->syntax stx 'start)])
       #`(#%module-begin
          body ...
          (provide/contract (start (request? . -> . response?)))
          (if extra-files-path
              (serve/servlet start
                             #:extra-files-path extra-files-path
                             #:launch-browser? launch-browser?)
              (serve/servlet start
                             #:launch-browser? launch-browser?))
          ))]))