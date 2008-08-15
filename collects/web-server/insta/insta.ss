#lang scheme
(require web-server/servlet
         web-server/servlet-env
         (for-syntax syntax/kerncase))

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
  
  ;; check-for-start: syntax (listof syntax) -> (listof syntax)
  ;; Checks to see that the user has defined a request handler named "start".
  ;; Returns a list of the expanded syntax.
  (define (check-for-start start-stx body-stxs)
    (define (id-for-start? ids)
      (ormap (lambda (id)
               (and (identifier? id)
                    (free-identifier=? id start-stx)))
             ids))
    
    ;; FIXME: this is not quite ready for prime time yet.
    (let ([expanded-bodies
           (let loop ([defns body-stxs])
             (apply append
                    (map (lambda (defn)
                           (let ([d (local-expand
                                     defn
                                     'module
                                     (kernel-form-identifier-list))])
                             (syntax-case d (define-values define-syntaxes begin)
                               [(begin defn ...)
                                (loop (syntax->list (syntax (defn ...))))]
                               [(define-values (id ...) body)
                                (list d)]
                               [(define-values . rest)
                                (list d)]
                               [(define-syntaxes (id ...) body)
                                (list d)]
                               [(define-syntaxes . rest)
                                (list d)]
                               [_else
                                (list d)])))
                         defns)))])
      (let ([ids (apply append (map (lambda (b)
                                      (let ([result
                                             (syntax-case b ()
                                               [(define-values (id ...) . __)
                                                (syntax->list #'(id ...))]
                                               [_ '()])])
                                        result))
                                    expanded-bodies))])
        (unless (id-for-start? ids)
          (raise-syntax-error #f "required \"start\" request handler needs to be defined" stx))
        (values expanded-bodies))))
  
  (syntax-case stx ()
    [(_ body ...)
     (let ([start-stx (datum->syntax stx 'start)])
       (with-syntax ([start start-stx]
                     #;[(expanded-body ...)
                        (check-for-start start-stx (syntax->list #'(body ...)))])
         #`(#%module-begin
            body ...
            (provide/contract (start (request? . -> . response?)))
            (if extra-files-path
                (serve/servlet start
                               #:extra-files-path extra-files-path
                               #:launch-browser? launch-browser?)
                (serve/servlet start
                               #:launch-browser? launch-browser?)))))]))