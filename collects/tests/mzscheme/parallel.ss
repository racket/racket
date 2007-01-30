
;; Runs 3 threads perfoming the test suite simultaneously. Each
;;  thread creates a directory sub<n> to run in, so that filesystem
;;  tests don't collide.

(with-handlers ([exn:fail?
		 (lambda (exn)
		   (namespace-set-variable-value!
		    'parallel-load
		    "quiet.ss"))])
  (namespace-variable-value 'parallel-load))

(define in-shared-k #f)
;; Some threads start with the
;;  same continuation, which forces sharing of the Scheme stack:
(thread-wait
 (thread (lambda () ((let/cc k (set! in-shared-k k) void)))))

(define (start n x) 
  (if (odd? n)
      (x)
      (in-shared-k x)))

; Runs n versions of test in parallel threads and namespaces, 
; waiting until all are done
(define (parallel n test)
  (let ([done (make-semaphore)]
	[go (make-semaphore)]
        [custodians (let loop ([n n])
                      (if (zero? n)
                          null
                          (cons (make-custodian) (loop (sub1 n)))))])
    (let loop ([n n])
      (unless (zero? n)
        (let ([ns (make-namespace)]
              [eh (exit-handler)]
              [cust (list-ref custodians (sub1 n))])
          (parameterize ([current-custodian cust])
            (thread
             (lambda ()
               (start
                n
                (lambda ()
                  (parameterize ([current-namespace ns]
                                 [exit-handler (lambda (v)
                                                 (for-each (lambda (c)
                                                             (unless (eq? c cust)
                                                               (custodian-shutdown-all c)))
                                                           custodians)
                                                 (eh v))])
                    (namespace-transformer-require 'mzscheme)
                    (eval `(define Section-prefix ,(format "~a:" n)))
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
                            (semaphore-post done))))))))))
          (loop (sub1 n)))))
    (with-handlers ([exn? (lambda (exn)
                            (for-each custodian-shutdown-all
                                      custodians)
                            (raise exn))])
      (let loop ([n n])
        (unless (zero? n)
          (semaphore-wait done)
          (loop (sub1 n))))
      (let loop ([n n])
        (unless (zero? n)
          (semaphore-post go)
          (semaphore-wait done)
          (loop (sub1 n)))))))

(define (delete-directory* dir)
  (for-each (lambda (f)
	      (let ([f (build-path dir f)])
		(if (or (link-exists? f) (file-exists? f))
		    (delete-file f)
		    (delete-directory* f))))
	    (directory-list dir))
  (delete-directory dir))

(parallel 3 (path->complete-path parallel-load (current-load-relative-directory)))
