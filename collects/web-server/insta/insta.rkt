#lang racket
(require web-server/servlet
         web-server/servlet-env
         (for-syntax racket)
         (for-syntax syntax/kerncase))

(provide 
 (all-from-out web-server/servlet)
 (except-out (all-from-out racket) #%module-begin)
 (rename-out [web-module-begin #%module-begin]))

(define extra-files-path #f)
(define launch-browser? #t)

(provide/contract
 [static-files-path (path-string? . -> . void?)])
(define (static-files-path path)
  (set! extra-files-path path))

(provide/contract
 [no-web-browser (-> void)])
(define (no-web-browser)
  (set! launch-browser? false))

;; check-for-def : syntax syntax-list -> void
;; Expands body-stxs and determines if id-stx is bound therein.
;; If not error w/ error-msg. stx is the root syntax context for everything
(define-for-syntax (check-for-def stx id-stx error-msg body-stxs)
  (with-syntax ([(pmb body ...)
                 (local-expand 
                  (quasisyntax/loc stx
                    (#%module-begin #,@body-stxs))
                  'module-begin 
                  empty)])
    (let loop ([syns (syntax->list #'(body ...))])
      (if (empty? syns)
          (raise-syntax-error 'insta error-msg stx)
          (kernel-syntax-case (first syns) #t
            [(define-values (id ...) expr)
             (unless
                 (ormap (lambda (id)
                          (and (identifier? id)
                               (free-identifier=? id id-stx)))
                        (syntax->list #'(id ...)))
               (loop (rest syns)))
             ]
            [_
             (loop (rest syns))])))
    (quasisyntax/loc stx
      (pmb body ...))))

(define-syntax (web-module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (let* ([start (datum->syntax stx 'start)]
            [expanded (check-for-def stx 
                                     start "You must provide a 'start' request handler."
                                     #'(body ...))])
       (quasisyntax/loc stx
         (#,@expanded
          (provide/contract (#,start (request? . -> . can-be-response?)))
          (serve/servlet (contract (request? . -> . can-be-response?) #,start
                                   'you 'web-server
                                   "start"
                                   #f)
                         #:port 0
                         #:extra-files-paths (if extra-files-path (list extra-files-path) empty)
                         #:launch-browser? launch-browser?))))]))
