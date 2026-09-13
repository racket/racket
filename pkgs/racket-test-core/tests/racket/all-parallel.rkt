#lang racket/base
(require racket/place)

;; Runs multiple places perfoming the test suite simultaneously. Each
;; place creates a directory sub<n> to run in, so that filesystem
;; tests don't collide.

;; Note that the old "parallel.rktl" runs multiple threads instead of
;; multiple places.

(define (go)
  (place
   pch
   (define n (place-channel-get pch))
   (define quiet (place-channel-get pch))
   (define all (place-channel-get pch))
   (let ([ns (make-base-namespace)]
         [eh (exit-handler)]
         [ql all])
     (parameterize ([current-namespace ns])
       (namespace-require '(lib "racket/init"))
       (eval `(define Section-prefix ,(format "~a:" n)))
       (when ql
         (eval `(define quiet-load (quote ,ql))))
       (let ([dirname (path->complete-path (format "sub~s" n))])
         (when (directory-exists? dirname)
           (delete-directory* dirname))
         (make-directory dirname)
         (current-directory dirname)
           (load quiet)
           (current-directory (build-path dirname 'up))
           (delete-directory* dirname))))))

(define (delete-directory* dir)
  (for-each (lambda (f)
	      (let ([f (build-path dir f)])
		(if (or (link-exists? f) (file-exists? f))
		    (delete-file f)
		    (delete-directory* f))))
	    (directory-list dir))
  (delete-directory dir))

(module+ main
  (define (parallel n quiet all)
    (define places
      (for/list ([i n])
        (define p (go))
        (place-channel-put p i)
        (place-channel-put p quiet)
        (place-channel-put p all)
        p))
    (for-each place-wait places))
  
  (parallel 3
            (path->complete-path "quiet.rktl")
            (path->complete-path "all.rktl"))
  (exit 0))

(module+ test)
