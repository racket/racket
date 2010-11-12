#lang racket
(require "../slave/slave.rkt"
         tests/eli-tester)

(define (test-handle-one-msg 
         password m authenticated?
         expected-authenticated? expected-log expected-bs-rx)
  (define-values (ip-read ip-write) (make-pipe))
  (define op (open-output-bytes))
  (define log empty)
  (define (log! fmt . args)
    (set! log (cons (apply format fmt args) log)))
  (when m
    (write m ip-write))
  (close-output-port ip-write)
  
  (define new-authenticated?
    (handle-one-msg password log! ip-read op authenticated?))
  (define new-log
    (reverse log))
  (define new-bs
    (get-output-bytes op))
  
  (test #:failure-prefix (format "~S" (list password m authenticated?))
        (test new-authenticated? => expected-authenticated?
              new-log => expected-log
              (regexp-match expected-bs-rx new-bs))))

(test
 ; write-output-bytes
 (local [(define obs1 (open-output-bytes))
         (define obs2 (open-output-bytes))]
   (test
    (display "123" obs1)
    (write-output-bytes obs1 obs2)
    (close-output-port obs1)
    (close-output-port obs2)
    (get-output-bytes obs2) => #"3123"))
 
 ; handle-one-msg
 (test-handle-one-msg "foo" '(auth "foo") #t
                      #t '("Authenticated") #"#t")
 
 (test-handle-one-msg "foo" '(auth "foo") #f
                      #t '("Authenticated") #"#t")
 
 (test-handle-one-msg "foo" '(auth "bar") #t
                      #t '("Illegal message: '(auth \"bar\")") #"#f")
 
 (test-handle-one-msg "foo" '(auth "bar") #f
                      #f '("Illegal message: '(auth \"bar\")") #"#f")
 
 (test-handle-one-msg "foo" #f #f
                      (void) '("Master disconnect") #"")
 
 (test-handle-one-msg "foo" '(run 10 "/bin/echo" "foo") #f
                      #f '("Illegal message: '(run 10 \"/bin/echo\" \"foo\")") #"#f")
 
 (test-handle-one-msg "foo" '(run 10 "/bin/echo" "foo") #t
                      #t 
                      '("Running with timeout (10) (\"/bin/echo\" \"foo\")" "Finished running (\"/bin/echo\" \"foo\"), status was 0")
                      #rx"#\\([0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ 0\\)4foo\n0")
 
 (test-handle-one-msg "foo" '(run 0 "/bin/echo" "foo") #t
                      #t 
                      '("Running with timeout (0) (\"/bin/echo\" \"foo\")" "Finished running (\"/bin/echo\" \"foo\"), status was #f")
                      #rx"#\\([0-9]+\\.[0-9]+ [0-9]+\\.[0-9]+ #f\\)00")
 
 ; call-with-safe-read
 (call-with-safe-read (λ () (read (open-input-string "(run 10 \"/bin/echo\" \"foo\")"))))
 =>
 '(run 10 "/bin/echo" "foo")
 
 (call-with-safe-read (λ () (read (open-input-string "(auth \"foo\")"))))
 =>
 '(auth "foo")
 
 (call-with-safe-read (λ () (read (open-input-string ""))))
 =>
 eof
 
 (call-with-safe-read (λ () (read (open-input-string "(auth #&\"foo\")"))))
 =>
 (error 'read "#& expressions not currently enabled")
 
 (call-with-safe-read (λ () (read (open-input-string "(auth #~\"foo\")"))))
 =>
 (error 'read "#~~ compiled expressions not currently enabled")
 
 (call-with-safe-read (λ () (read (open-input-string "#0='(3 #0#)"))))
 =>
 (error 'read "#..= expressions not currently enabled")
 
 ; call-with-safe-read + handle-one-msg
 (call-with-safe-read
  (λ ()
    (test-handle-one-msg "foo" `(auth ,(box "bar")) #f
                         #f 
                         '("Illegal message: (exn:fail:read \"read: #& expressions not currently enabled\" #<continuation-mark-set> (list (srcloc #f #f #f 7 2)))")
                         #"#f")))
 
 ; XXX handle
 ; XXX port-closing-curry
 ; XXX main
 
 )


