
;; Runs 3 threads perfoming the test suite simultaneously. Each
;;  thread creates a directory sub<n> to run in, so that filesystem
;;  tests don't collide.

(with-handlers ([exn:fail?
		 (lambda (exn)
		   (namespace-set-variable-value!
		    'parallel-load
		    "quiet.ss"))])
  (namespace-variable-value 'parallel-load))

(define (start x) (x))

;; Uncomment the following expression to have threads start with the
;;  same continuation, which forces sharing of the Scheme stack:
#;
(thread-wait
 (thread (lambda () ((let/cc k (set! start k) void)))))

; Runs n versions of test in parallel threads and namespaces, 
; waiting until all are done
(define (parallel n test)
  (let ([done (make-semaphore)]
	[go (make-semaphore)])
    (let loop ([n n])
      (unless (zero? n)
	      (let ([ns (make-namespace)])
		(thread
		 (lambda ()
		   (start
		    (lambda ()
		      (parameterize ([current-namespace ns])
			(namespace-transformer-require 'mzscheme)
			(let ([dirname (format "sub~s" n)])
			  (when (directory-exists? dirname)
			    (delete-directory* dirname))
			  (make-directory dirname)
			  (current-directory dirname)
			  (dynamic-wind
			      void
			      (lambda ()
				(load test))
			      (lambda ()
				(semaphore-post done)
				(semaphore-wait go)
				(printf "~nThread ~s:" n)
				(eval '(report-errs))
				(current-directory (build-path 'up))
				(delete-directory* dirname)
				(semaphore-post done)))))))))
		(loop (sub1 n)))))
    (let loop ([n n])
      (unless (zero? n)
	      (semaphore-wait done)
	      (loop (sub1 n))))
    (let loop ([n n])
      (unless (zero? n)
	      (semaphore-post go)
	      (semaphore-wait done)
	      (loop (sub1 n))))))

(define (delete-directory* dir)
  (for-each (lambda (f)
	      (let ([f (build-path dir f)])
		(if (or (link-exists? f) (file-exists? f))
		    (delete-file f)
		    (delete-directory* f))))
	    (directory-list dir))
  (delete-directory dir))

(parallel 3 (path->complete-path parallel-load (current-load-relative-directory)))
