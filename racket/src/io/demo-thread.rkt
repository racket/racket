#lang racket/base
(require "bootstrap-thread-main.rkt"
         (only-in "../thread/bootstrap.rkt"
                  register-place-symbol!
                  set-io-place-init!)
         (only-in racket/base
                  [current-directory host:current-directory]
                  [path->string host:path->string]))

;; Don't use exceptions here; see "../thread/demo.rkt"

(current-directory (host:path->string (host:current-directory)))
(set-io-place-init! io-place-init!)
(set-make-place-ports+fds! make-place-ports+fds)

(define done? #f)

(define-syntax-rule (test expect rhs)
  (let ([e expect]
        [v rhs])
    (unless (equal? e v)
      (error 'failed "~s: ~e" 'rhs v))))

(call-in-main-thread
 (lambda ()
   
   ;; Make `N` threads trying to write `P` copies
   ;; of each possible byte into a limited pipe, and
   ;; make `N` other threads try to read those bytes.
   (let ()
     (define N 8)
     (define M (/ 256 N))
     (define P 1)
     (define-values (in out) (make-pipe N))
     (test #f (byte-ready? in))
     (test out (sync/timeout #f out))
     (test N (write-bytes (make-bytes N 42) out))
     (test #t (byte-ready? in))
     (test #f (sync/timeout 0 out))
     (test 42 (read-byte in))
     (test #t (byte-ready? in))
     (test out (sync/timeout #f out))
     (write-byte 42 out)
     (test #f (sync/timeout 0 out))
     (test (make-bytes N 42) (read-bytes N in))
     (test #f (byte-ready? in))
     (test out (sync/timeout #f out))
     (define vec (make-vector 256))
     (define lock-vec (for/vector ([i 256]) (make-semaphore 1)))
     (define out-ths
       (for/list ([i N])
         (thread (lambda ()
                   (for ([k P])
                     (for ([j M])
                       (write-byte (+ j (* i M)) out)))))))
     (define in-ths
       (for/list ([i N])
         (thread (lambda ()
                   (for ([k P])
                     (for ([j M])
                       (define v (read-byte in))
                       (semaphore-wait (vector-ref lock-vec v))
                       (vector-set! vec v (add1 (vector-ref vec v)))
                       (semaphore-post (vector-ref lock-vec v))))))))
     (map sync out-ths)
     (map sync in-ths)
     (for ([count (in-vector vec)])
       (unless (= count P)
         (error "contended-pipe test failed"))))

   ;; Peeking effectively extends the buffer:
   (let-values ([(in out) (make-pipe 3)])
     (test 3 (write-bytes-avail #"12345" out))
     (test #f (sync/timeout 0 out))
     (test #\1 (peek-char in))
     (test out (sync/timeout 0 out))
     (test 1 (write-bytes-avail #"12345" out))
     (test #f (sync/timeout 0 out))
     (test #\1 (peek-char in))
     (test 0 (write-bytes-avail* #"12345" out))
     (test #\2 (peek-char in 1))
     (test 1 (write-bytes-avail* #"12345" out))
     (let ([s (make-bytes 6 (char->integer #\-))])
       (test 5 (read-bytes-avail! s in))
       (test #"12311-" s))
     (test 3 (let loop ([n 0])
               (define v (write-bytes-avail* #"1234" out))
               (if (zero? v)
                   n
                   (loop (+ n v))))))

   ;; Further test of peeking in a limited pipe (shouldn't get stuck):
   (let-values ([(i o) (make-pipe 50)]
                [(s) (make-semaphore)])
     (define t
       (thread (lambda ()
                 (peek-bytes 100 0 i)
                 (semaphore-wait s)
                 (peek-bytes 200 0 i))))
     (display (make-bytes 100 65) o)
     (sync (system-idle-evt))
     (semaphore-post s)
     (display (make-bytes 100 66) o)
     (sync t))

   ;; Check progress events
   (define (check-progress-on-port make-in)
     (define (check-progress dest-evt fail-dest-evt)
       (define in (make-in)) ; content = #"hello"
       (test #"he" (peek-bytes 2 0 in))
       (test #"hello" (peek-bytes 5 0 in))
       (test #"hel" (peek-bytes 3 0 in))
       (define progress1 (port-progress-evt in))
       ;(test #t (evt? progress1))
       (test #f (sync/timeout 0 progress1))
       (test #"hel" (peek-bytes 3 0 in))
       (test #f (sync/timeout 0 progress1))
       ;(test #f (port-commit-peeked 3 progress1 fail-dest-evt in))
       (test #"hel" (peek-bytes 3 0 in))
       (test #f (sync/timeout 0 progress1))
       (test #t (port-commit-peeked 3 progress1 dest-evt in))
       (test #"lo" (peek-bytes 2 0 in))
       (test progress1 (sync/timeout #f progress1))
       (test #f (port-commit-peeked 1 progress1 always-evt in))
       (close-input-port in))
     (check-progress always-evt never-evt)
     (check-progress (make-semaphore 1) (make-semaphore 0))
     (check-progress (semaphore-peek-evt (make-semaphore 1)) (semaphore-peek-evt (make-semaphore 0)))
     (let ()
       (define ch1 (make-channel))
       (define ch2 (make-channel))
       (thread (lambda () (channel-put ch1 'ok)))
       (thread (lambda () (channel-get ch2)))
       (sync (system-idle-evt))
       (check-progress ch1 ch2)
       (check-progress (channel-put-evt ch2 'ok) (channel-put-evt ch1 'ok))))
   (check-progress-on-port
    (lambda ()
      (define-values (in out) (make-pipe))
      (write-bytes #"hello" out)
      in))
   (check-progress-on-port
    (lambda ()
      (open-input-bytes #"hello")))
   (call-with-output-file "compiled/hello.txt"
     (lambda (o) (write-bytes #"hello" o))
     'truncate)
   (check-progress-on-port
    (lambda ()
      (open-input-file "compiled/hello.txt")))

   (define (check-out-evt make-out [block #f] [unblock #f])
     (define o (make-out))
     (test #t (port-writes-atomic? o))
     (define evt (write-bytes-avail-evt #"hello" o))
     (test 5 (sync evt))
     (when block
       (block o)
       (define evt (write-bytes-avail-evt #"hello" o))
       (test #f (sync/timeout 0 evt))
       (test #f (sync/timeout 0.1 evt))
       (unblock)
       (test #t (and (memq (sync evt) '(1 2 3 4 5)) #t)))
     (close-output-port o))
   (let ([i #f])
     (check-out-evt (lambda ()
                      (define-values (in out) (make-pipe 10))
                      (set! i in)
                      out)
                    (lambda (o)
                      (write-bytes #"01234" o))
                    (lambda ()
                      (read-bytes 6 i))))
   (check-out-evt (lambda ()
                    (open-output-bytes)))
   (check-out-evt (lambda ()
                    (open-output-file "compiled/hello.txt" 'truncate)))

   ;; Custodian shutdown closes port => don't run out of file descriptors
   (for ([i 512])
     (define c (make-custodian))
     (parameterize ([current-custodian c])
       (for ([j 10])
         (open-input-file "compiled/hello.txt")))
     (custodian-shutdown-all c))

   ;; Places
   ;;  BEWARE: we can run some basic places tests in bootstrap mode,
   ;;  but since "places" are just Racket threads, avoid any rktio-based
   ;;  blocking operations
   (register-place-symbol! 'report
                           (lambda (pch)
                             (write-string "expected place out\n")
                             (write-string "expected place err\n" (current-error-port))))
   (define-values (pl1 pin1 pout1 perr1) (dynamic-place 'dummy 'report
                                                        (current-input-port)
                                                        (current-output-port)
                                                        (current-error-port)))
   (test #t (place? pl1))
   (test #f pin1)
   (test #f pout1)
   (test #f perr1)
   (test 0 (place-wait pl1))

   ;; See warnign about about places in bootstrap-demo mode
   (register-place-symbol! 'echo2
                           (lambda (pch)
                             (define s (read-line))
                             (write-string s)
                             (define s2 (list->string (reverse (string->list s))))
                             (write-string s2 (current-error-port))))
   (define-values (pl2 pin2 pout2 perr2) (dynamic-place 'dummy 'echo2 #f #f #f))
   (test #t (place? pl2))
   (test #t (output-port? pin2))
   (test #t (input-port? pout2))
   (test #t (input-port? perr2))
   (write-string "hello" pin2)
   (close-output-port pin2)
   (test "hello" (read-string 100 pout2))
   (test "olleh" (read-string 100 perr2))
   (test 0 (place-wait pl2))

   ;; Can pass a file-stream port through a place channel, but it
   ;; makes a fresh port on the other end
   (define-values (left1 right1) (place-channel))
   (let ([f (open-input-file "compiled/hello.txt")])
     (file-stream-buffer-mode f 'none)
     (test #\h (read-char f))
     (test 1 (file-position f))
     (place-channel-put left1 f)
     (define f2 (place-channel-get right1))
     (file-stream-buffer-mode f2 'none)
     (test #\e (read-char f2))
     (test 2 (file-position f2))
     (close-input-port f2)
     (test 2 (file-position f))
     (test #\l (read-char f))
     (test 3 (file-position f))
     (close-input-port f))
   ;; Paths are ok as place messages:
   (let ([p (bytes->path #"ok" 'windows)])
     (place-channel-put left1 p)
     (test p (place-channel-get right1)))
   (let ([p (bytes->path #"ok" 'unix)])
     (place-channel-put left1 p)
     (test p (place-channel-get right1)))

   ;; TCP and accept evts
   (parameterize ([current-custodian (make-custodian)])
     (define l (tcp-listen 59078 5 #t))
     (test #t (tcp-listener? l))

     (define acc-evt (tcp-accept-evt l))
     (test #f (sync/timeout 0 acc-evt))

     (define-values (ti to) (tcp-connect "localhost" 59078))

     (define-values (tai tao) (apply values (sync acc-evt)))

     (test 6 (write-string "hello\n" to))
     (flush-output to)
     (test "hello" (read-line tai))

     (custodian-shutdown-all (current-custodian)))

   ;; UDP and evts
   (define u1 (udp-open-socket))
   (test (void) (udp-bind! u1 #f 10768))

   (define u2 (udp-open-socket))

   (define bstr (make-bytes 10))
   (define r1-evt (udp-receive!-evt u1 bstr))

   (test #f (sync/timeout 0 r1-evt))

   (test (void) (sync (udp-send-to-evt u2 "localhost" 10768 #"hello")))

   (let ([l (sync r1-evt)])
     (test 5 (car l))
     (test #"hello" (subbytes bstr 0 5)))

   (test #f (sync/timeout 0 r1-evt))
   (udp-close u1)
   (udp-close u2)

   ;; Check some expected errors:
   (printf "[two expected errors coming up...]\n")
   (sync (thread (lambda () (sync r1-evt))))
   (sync (thread (lambda () (sync (udp-send-to-evt u2 "localhost" 10768 #"")))))
   (printf "[two error messages about a UDP socket being closed were expected]\n")

   ;; ----------------------------------------

   (printf "Enter to continue after confirming process sleeps...\n")
   (read-line)
   
   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))
