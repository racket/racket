#lang racket

(require racket/place typed-racket/optimizer/logging
         unstable/open-place compiler/compiler)
(provide start-worker dr serialize-exn deserialize-exn s-exn? generate-log/place)
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

(define (dr p [reg-box #f])
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (dynamic-require `(file ,(if (string? p) p (path->string p))) #f)
    (and reg-box (set-box! reg-box (namespace-module-registry (current-namespace))))))

(define (start-worker get-ch name)
  (open-place ch
    (define reg (box #f))
    (let loop ()
      (match (place-channel-get get-ch)                
        [(vector 'log name dir res)
         (with-handlers ([exn:fail? 
                          (λ (e) (place-channel-put 
                                  res 
                                  (string-append "EXCEPTION: " (exn-message e))))])
           (define lg (generate-log/place name dir reg))
           (place-channel-put res lg))
         (loop)]
        [(vector p* res error?) 
         (define-values (path p b) (split-path p*))
         (parameterize ([read-accept-reader #t]
                        [current-load-relative-directory
                         (path->complete-path path)]
                        [current-directory path]
                        [current-output-port (open-output-nowhere)]
                        [error-display-handler (if error? void (error-display-handler))])
           (with-handlers ([exn? (λ (e)
                                   (place-channel-put res (serialize-exn e)))])
             (dr p reg)
             (place-channel-put res #t)))
         (loop)]))))

(define comp (compile-zos #f #:module? #t))

(define (generate-log/place name dir [reg-box #f])
  ;; some tests require other tests, so some fiddling is required
  (define f (build-path dir name))
  (with-output-to-string
    (lambda ()
      (with-tr-logging-to-port
       (current-output-port)
       (lambda ()
         (comp (list f) 'auto)))
      (parameterize
          ([current-namespace (make-base-empty-namespace)]
           [current-load-relative-directory dir])
        (dynamic-require f #f)
        (and reg-box (set-box! reg-box (namespace-module-registry (current-namespace)))))
      ;; clean up compiled files in prevision of the next testing run
      (delete-directory/files (build-path dir "compiled")))))
