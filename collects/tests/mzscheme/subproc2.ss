
(define child? (member "child" (vector->list (current-command-line-arguments))))

(define a-lot 500000)

(unless child?
  (load-relative "loadtest.ss")

  (let ([path (find-executable-path (find-system-path 'exec-file) #f)])
    (let-values ([(subproc in out /err) (subprocess #f #f (current-error-port)
                                                    path "-rq"  
                                                    (build-path (current-load-relative-directory)
                                                                "subproc2.ss")
                                                    "child")])
      (test 'running subprocess-status subproc)
      (test out object-wait-multiple 0 out)
      (test #f object-wait-multiple 0 in)
      (fprintf out "go~n")
      
      (test "going" read-line in)
      
      (test #t
            positive?
            ;; Push a-lot chars; should block at least once:
            (let ([s (make-string a-lot #\a)])
              (let loop ([offset 0])
                (let ([ready? (object-wait-multiple 0 out)])
                  (printf "~a ~a~n" offset ready?)
                  (+ (if ready? 0 1)
                     (let ([n (write-string-avail s out offset)])
                       (if (= (+ n offset) a-lot)
                           0
                           (loop (+ offset n)))))))))

      (test "done" read-line in)
      
      'ok)))

(when child?
  (with-handlers ([void (lambda (x)
                          (fprintf (current-error-port) "CHILD ")
                          (raise x))])
    (if (equal? "go" (read-line (current-input-port) 'any))
        (printf "going~n")
        (printf "not go!~n"))
  
    (fprintf (current-error-port) "CHILD: starting sleep~n")
    (sleep 1)
    (fprintf (current-error-port) "CHILD: finished sleep; reading...~n")

    (unless (= a-lot (string-length (read-string a-lot)))
      (fprintf (current-error-port) "CHILD: bad read count"))

    (printf "done~n")
    
    'ok))
