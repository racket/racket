
;; Runs 3 threads perfoming the test suite simultaneously. Each
;;  thread creates a directory sub<n> to run in, so that filesystem
;;  tests don't collide.

(namespace-variable-value 'parallel-load #f
                          (lambda ()
                            (namespace-set-variable-value! 'parallel-load "quiet.ss")))

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
  (let ([custodians (let loop ([n n])
                      (if (zero? n)
                          null
                          (cons (make-custodian) (loop (sub1 n)))))]
        [o (current-error-port)])
    (define threads
      (let loop ([n n])
        (if (zero? n)
            null
            (cons
             (let ([ns (make-namespace)]
                   [eh (exit-handler)]
                   [cust (list-ref custodians (sub1 n))])
               (parameterize ([current-custodian cust])
                 (thread
                  (lambda ()
                    (start
                     n
                     (lambda ()
                       (parameterize ([current-namespace ns])
                         (namespace-transformer-require 'mzscheme)
                         (eval `(define Section-prefix ,(format "~a:" n)))
                         (let ([dirname (path->complete-path (format "sub~s" n))])
                           (when (directory-exists? dirname)
                             (delete-directory* dirname))
                           (make-directory dirname)
                           (current-directory dirname)
                           (parameterize ([exit-handler (lambda (v)
                                                          (current-directory (build-path dirname 'up))
                                                          (delete-directory* dirname)
                                                          (if (zero? v)
                                                              ;; Shut down self:
                                                              (custodian-shutdown-all cust)
                                                              (begin
                                                                ;; Shut down all the others:
                                                                (for-each (lambda (c)
                                                                            (unless (eq? c cust)
                                                                              (custodian-shutdown-all c)))
                                                                          custodians)
                                                                ;; Exit whole process:
                                                                (eh v))))])
                             (load test)
                             (exit 0))))))))))
             (loop (sub1 n))))))
    (with-handlers ([exn? (lambda (exn)
                            (for-each custodian-shutdown-all
                                      custodians)
                            (raise exn))])
      (for-each sync threads))))

(define (delete-directory* dir)
  (for-each (lambda (f)
	      (let ([f (build-path dir f)])
		(if (or (link-exists? f) (file-exists? f))
		    (delete-file f)
		    (delete-directory* f))))
	    (directory-list dir))
  (delete-directory dir))

(parallel 3 (path->complete-path parallel-load (current-load-relative-directory)))
(exit 0)
