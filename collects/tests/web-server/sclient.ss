; copyright 11/29/2001 A.D. by Paul Graunke and the PLT
; This is a rip-off---err---port of the Rice systems group's sclient 2.0 software
; by gaurav, peter, and gang.
; The TCP/IP stuff isn't as finely controlled, so it's my fault if it stinks.
(module sclient mzscheme
  (require (lib "etc.ss"))
  
  ; old-state = 'nothing | 'waiting | 'reading | 'writing
  
  ; old-client = (make-client iport oport state nat nat nat nat nat)
  ;(define-struct
  ; old-client
  ; (in out state bytes-read partial-req-written num-reqs-done timestamp start-time))
  
  ; client = (make-client nat^4)
  (define-struct client (bytes-read num-reqs-done timestamp start-time))
  
  ; why 6Mb? - it isn't used in the original either
  ;(define PERCONBUFSIZE (* 6 1000 1000))
  
  ; FIX? - see about eliminating mutation
  (define *start-time* 0)
  (define *last-connect* 0)
  
  ; main : str str nat nat nat nat nat -> ???
  (define (main host-machine target-file port reps num-clients rate cpu-mhz)
    (let* ([request-string (build-http-request target-file)]
           [request-length (string-length request-string)]
           [cycles-per-request (* cpu-mhz (/ 1000000 rate))]
           [max-connect-cycles (* 50000 cpu-mhz)])
      (printf "A request will be issued every: ~a cycles i.e. every ~a us\n"
              cycles-per-request (/ cycles-per-request cpu-mhz))
      ;(init-clients host-machine port) ; just did DNS lookup in original
      (set! *start-time* (current-milliseconds))
      (let* ([all-clients-threads
              (build-list num-clients
                          (lambda (i) (simple-connect host-machine port request-string)))]
             [all-clients (map car all-clients-threads)]
             [all-threads (map cdr all-clients-threads)])
        (for-each thread-wait all-threads)
        ; print-stats
        (let* ([diff-time0 (- (current-milliseconds) *start-time*)]
               [diff-time (if (zero? diff-time0) 1 diff-time0)]
               [diff-seconds (/ diff-time 1e6)]
               [total-bytes-read (apply + (map client-bytes-read all-clients))]
               [reqs-done (apply + (map client-num-reqs-done all-clients))]
               [reps-done reqs-done] ; FIX?
               [n-diff-samples reqs-done] ; FIX?
               ;[diff-sum (apply + '(...))] FIX!
               [diff-sum -inf.0]
               [speed (* 8.0 (/ total-bytes-read diff-time))])
          ; the original was in micro seconds
          (printf "Elapsed time: ~a milliseconds\n" diff-time)
          (printf "reps done: ~a\n" reps-done)
          (printf "request rate: ~a\n"
                  (/ reqs-done diff-seconds))
          (printf "[ ~a Mb/s, ~a ~a S, ~a B, ~a R, ~a cl, pt ~a ~a c/s]\n"
                  speed target-file diff-seconds total-bytes-read
                  reps-done num-clients port
                  (/ reps-done diff-seconds))
          (printf "average response time: ~a us\n"
                  (exact->inexact (/ diff-sum n-diff-samples)))
          (printf "maximum number of pseudo clients: ~a\n" num-clients)))))
  
  ; build-http-request : str -> str
  (define (build-http-request host-name)
    ; This was clearly broken in the original:
    ;"GET http://fxp2.cs.rice.edu:8080/%s HTTP/1.0\r\n"
    (string-append "GET /" host-name" HTTP/1.0\r\n"
                   "Accept: text/plain\r\n"
                   "Accept: text/html\r\n"
                   "Accept: */*\r\n"))
  
  ; simple-connect : str nat str -> (cons client thread)
  ; original - this took two unused arguments and banged its result into a vector
  ; FIX? - should it report statistics when exceptions occur?
  (define (simple-connect host-name port request)
    (let* ([start-time (current-milliseconds)]
           [client (make-client 0 0 start-time start-time)])
      (cons client
            (thread
             (lambda ()
               (let connect ()
                 (let-values ([(in out) (tcp-connect host-name port)])
                   (set! *last-connect* (current-milliseconds))
                   ; FIX? - Gaurav's paper said something about not filling the server's
                   ; TCP/IP queue with waiting clients.  It sounds like this needs a
                   ; timeout of a little more than twice the expected round trip delay
                   ; or whatever.
                   (send-http in out client request)
                   (read-from-connection in out client)
                   ; FIX? - do something like deadline-passed, perhaps?
                   (set-client-num-reqs-done! client (add1 (client-num-reqs-done client))))
                 (connect)))))))
  
  ; send-http : iport oport client str -> void
  ; The original did all sorts of funny stuff for asynchronous I/O.
  (define (send-http in out client request)
    (display request out))
  
  (define read-increments (* 8 1024))
  (define *singe-read-buffer* (make-string read-increments #\space))
  
  ; read-from-connection : iport oport client -> void
  (define (read-from-connection in out client)
    (let read-all ()
      (let ([x (read-string-avail! *singe-read-buffer* in)])
        (unless (eof-object? x)
          (set-client-bytes-read! client (+ x (client-bytes-read client)))
          (read-all))))))