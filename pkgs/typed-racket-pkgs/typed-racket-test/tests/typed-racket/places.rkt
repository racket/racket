#lang racket

(require racket/place typed-racket/optimizer/logging
         unstable/open-place syntax/modcode data/queue)
(provide start-worker dr serialize-exn deserialize-exn s-exn? generate-log/place compile-path/place verbose?)

(define verbose? (make-parameter #f))

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
  (define verb (verbose?))
  (open-place ch
    (let loop ()
      (match (place-channel-get get-ch)                
        [(vector 'log name dir res)
         (dynamic-require 'typed-racket/core #f)
         (with-handlers ([exn:fail? 
                          (λ (e) (place-channel-put res (serialize-exn e)))])
           (define lg (generate-log/place name dir))
           (place-channel-put res lg))
         (loop)]
        [(vector 'compile path res)
         (with-handlers ([exn:fail? 
                          (λ (e) (place-channel-put res (serialize-exn e)))])
            (compile-path/place path)
            (place-channel-put res (void)))
         (loop)]
        [(vector p* res error?) 
         (define-values (path p b) (split-path p*))
         (with-handlers ([exn? (λ (e) (place-channel-put res (serialize-exn e)))])
           (parameterize ([read-accept-reader #t]
                          [current-load-relative-directory
                           (path->complete-path path)]
                          [current-directory path]
                          [current-output-port (open-output-nowhere)]                        
                          [error-display-handler (if error? void (error-display-handler))]) 
             (dr p)
             (place-channel-put res #t)))
         (loop)]))))

(define (compile-path/place path)
  (get-module-code
    path
    #:choose (lambda (src zo so) 'src)))


(define-namespace-anchor anchor)

(define (generate-log/place name dir)
  ;; some tests require other tests, so some fiddling is required
  (define file (simplify-path (build-path dir name)))
  (define orig-load/use-compiled (current-load/use-compiled))
  (define orig-use-compiled-file-paths (use-compiled-file-paths))
  (define full-tr-logs (make-queue))
  (define sub-tr-logs (make-queue))

  (define (test-load/use-compiled path name)
    (parameterize [(use-compiled-file-paths null)
                   (current-load/use-compiled reset-load/use-compiled)]
      (orig-load/use-compiled path name)))
  (define (reset-load/use-compiled path name)
    (parameterize [(use-compiled-file-paths orig-use-compiled-file-paths)
                   (current-load/use-compiled orig-load/use-compiled)]
        (with-tr-logging-to-queue
          sub-tr-logs
          (thunk
            (orig-load/use-compiled path name)))))

  (define regular-output
    (with-output-to-string
      (lambda ()
        (with-tr-logging-to-queue
          full-tr-logs
          (thunk
            (parameterize ([current-namespace (make-base-empty-namespace)]
                           [current-load/use-compiled test-load/use-compiled])
              (define orig-namespace (namespace-anchor->namespace anchor))
              (namespace-attach-module orig-namespace 'racket)
              (namespace-attach-module orig-namespace 'typed-racket/core)
              (dynamic-require file #f)))))))

  (define tr-logs
    (let ((tr-logs (queue->list full-tr-logs)))
      (sort
        (for/fold ((tr-logs tr-logs)) ((entry (in-queue sub-tr-logs)))
          (remove entry tr-logs))
        string<?)))

  (list tr-logs regular-output))
