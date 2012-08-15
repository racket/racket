#lang racket

(require racket/place data/queue racket/async-channel)
(provide start-worker dr serialize-exn deserialize-exn s-exn?)
(struct s-exn (message) #:prefab)
(struct s-exn:fail s-exn () #:prefab)
(struct s-exn:fail:syntax s-exn:fail (exprs) #:prefab)
(struct s-exn:fail:contract s-exn:fail () #:prefab)
(define (serialize-exn e)
  (match e
    [(exn:fail:syntax msg _ exprs)
     (s-exn:fail:syntax msg (map syntax->datum exprs))]
    [(exn:fail:contract msg _)
     (s-exn:fail:contract msg)]
    [(exn:fail msg _)
     (s-exn:fail msg)]
    [(exn msg _)
     (s-exn msg)]))
(define (deserialize-exn e)
  (match e
    [(s-exn:fail:syntax msg exprs)
     (exn:fail:syntax msg (current-continuation-marks) 
                      (map (λ (e) (datum->syntax #f e)) exprs))]
    [(s-exn:fail:contract m)
     (exn:fail:contract m (current-continuation-marks))]    
    [(s-exn:fail m)
     (exn:fail m (current-continuation-marks))]
    [(s-exn m)
     (exn m (current-continuation-marks))]))
(define (dr p)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (dynamic-require `(file ,(if (string? p) p (path->string p))) #f)))

(define (start-worker get-ch name)
  (define p 
    (place ch
           (define n (place-channel-get ch))
           (define get-ch (place-channel-get ch))
           (let loop ()
             (match-define (vector p* res error?) (place-channel-get get-ch))
             (define-values (path p b) (split-path p*))
             (parameterize ([read-accept-reader #t]
                            [current-load-relative-directory
                             (path->complete-path path)]
                            [current-directory path]
                            [current-output-port (open-output-nowhere)]
                            [error-display-handler (if error? void (error-display-handler))])
               (with-handlers ([exn? (λ (e)
                                       (place-channel-put res (serialize-exn e)))])
                 (dr p)
                 (place-channel-put res #t)))
             (loop))))
  (place-channel-put p name)
  (place-channel-put p get-ch))