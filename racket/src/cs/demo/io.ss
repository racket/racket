(import (rumble)
        (io)
        (thread))

(define-syntax test
  (syntax-rules ()
    [(_ expect rhs)
     (let ([e expect]
           [v rhs])
       (unless (equal? e v)
         (error 'failed "~s: ~e" 'rhs v)))]))

;; ----------------------------------------

(test #t (directory-exists? "demo"))
(test #f (directory-exists? "no-such-demo"))

;; ----------------------------------------

(define abcdefghijklmn (string->bytes/utf-8 "abcdefghijklmn"))
(define __abcdefghijklmn__ (string->bytes/utf-8 "__abcdefghijklmn__"))

(test '#vu8(133 215 197 255 64 58 190 114 223 91 138 39 8 130 30 227 60 208 188 206) (sha1-bytes abcdefghijklmn))
(test '#vu8(224 251 178 1 109 225 6 86 234 36 73 82 201 125 232 120 55 223 100 179 208 163 167 232 226 25 82 32)
      (sha224-bytes abcdefghijklmn))
(test '#vu8(6 83 199 233 146 215 170 212 12 178 99 87 56 184 112 228 193 84 175 179 70 52 13 2 199 151 212 144 221 82 213 249)
      (sha256-bytes abcdefghijklmn))
(test '#vu8(133 215 197 255 64 58 190 114 223 91 138 39 8 130 30 227 60 208 188 206) (sha1-bytes (open-input-bytes abcdefghijklmn)))
(test '#vu8(133 215 197 255 64 58 190 114 223 91 138 39 8 130 30 227 60 208 188 206) (sha1-bytes (open-input-bytes __abcdefghijklmn__) 2 16))

;; ----------------------------------------

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ([total 0])
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop (+ total (string-length s)))))
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-char p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([i 1000000] [v #f])
   (if (zero? i)
       v
       (loop (sub1 i)
             (bytes->string/utf-8 (string->bytes/utf-8 "ap\x3BB;ple"))))))


;; ----------------------------------------

(let ([c (make-custodian)])
  (with-continuation-mark
      parameterization-key
      (extend-parameterization (continuation-mark-set-first #f parameterization-key) current-custodian c)
    (let ()
      (define p (open-input-file "compiled/io.scm"))
      (define wb (make-weak-box p))
      (define we (make-will-executor))
      (will-register we p values)
      (set! p #f)
      (collect (collect-maximum-generation))
      (test #t (input-port? (will-try-execute we)))
      (collect (collect-maximum-generation))
      (test #f (weak-box-value wb))
      (custodian-shutdown-all c))))

;; ----------------------------------------

(call-in-main-thread
 (lambda ()
   (define root-logger (make-logger))

   (test 'none (log-max-level root-logger))
   (add-stderr-log-receiver! root-logger 'warning)

   (test 'warning (log-max-level root-logger))

   (log-message root-logger 'error "this should print to stderr" 5)

   (let ()
     (define demo1-logger (make-logger 'demo1 root-logger))
     (define demo2-logger (make-logger 'demo2 root-logger 'fatal))

     (log-message demo1-logger 'error "this should print to stderr, too" 5)
     (log-message demo2-logger 'error "this should not print to stderr" 5)

     (test 'warning (log-max-level demo1-logger))
     (test 'fatal (log-max-level demo2-logger))

     (let ()
       (define lr1 (make-log-receiver root-logger 'info 'cats))

       (test 'info (log-max-level demo1-logger))
       (test 'fatal (log-max-level demo2-logger))

       (test 'info (log-max-level demo1-logger 'cats))
       (test 'fatal (log-max-level demo2-logger 'cats))

       (test 'warning (log-max-level demo1-logger 'dogs))
       (test 'fatal (log-max-level demo2-logger 'dogs))

       (test #t (log-level? demo1-logger 'info 'cats))
       (test #f (log-level? demo1-logger 'debug 'cats))
       (test #f (log-level? demo1-logger 'info 'dogs))

       (let ()
         (define msg1 #f)
         (define th1 (thread (lambda () (set! msg1 (sync lr1)))))
         (sync (system-idle-evt))
         (test #f msg1)

         (log-message demo1-logger 'info 'cats "hello" 7)
         (sync (system-idle-evt))
         (test '#(info "cats: hello" 7 cats) msg1)

         (log-message demo1-logger 'info 'cats "goodbye" 9)
         (test '#(info "cats: goodbye" 9 cats) (sync lr1)))))


   ;; ----------------------------------------

   (when (threaded?)
     (let* ([place-symbols (make-hasheq)]
            [register-place-symbol!
             (lambda (sym proc)
               (hash-set! place-symbols sym proc))])
       (set-make-place-ports+fds! make-place-ports+fds)
       (set-start-place!
        (lambda (pch mod sym in out err cust plumber)
          (io-place-init! in out err cust plumber)
          (lambda ()
            ((hash-ref place-symbols sym) pch))))

       ;; Check file port passed across places
       (let ([f (open-input-file "compiled/io.scm")])
         (file-stream-buffer-mode f 'none)
         (let ([content (read-bytes 5 f)])
           (file-position f 0)

           (register-place-symbol! 'read-byte
                                   (lambda (pch)
                                     (let ([f (place-channel-get pch)])
                                       (file-stream-buffer-mode f 'none)
                                       (let ([b (read-byte f)])
                                         (close-input-port f)
                                         (place-channel-put pch b)))))
           (let-values ([(pl in out err) (dynamic-place 'dummy 'read-byte #f #f #f)])
             (test (bytes-ref content 0) (read-byte f))
             (place-channel-put pl f)
             (test (bytes-ref content 1) (place-channel-get pl))
             (test (bytes-ref content 2) (read-byte f))
             (close-input-port f))))))

   ;; Thread can be GCed if it's block on a place channel with no writer
   (let ()
     (define-values (left1 right1) (place-channel))
     (define saved #f)
     (define not-saved (gensym))
     (define weak-saved (make-weak-box not-saved))
     (define weak-right1 (make-weak-box right1))
     (place-channel-put right1 not-saved)
     ;; DON'T USE `right1` from here on...
     (let ()
       (define weak-thread
         (make-weak-box
          (thread (lambda ()
                    (define local-saved (place-channel-get left1))
                    (place-channel-get left1) ; no writer for this channel
                    (set! saved local-saved)))))
       (sync (system-idle-evt))
       (collect-garbage)
       (test #f (weak-box-value weak-right1))
       (test #f (weak-box-value weak-thread))
       (test #f (weak-box-value weak-saved))))

   (void)))
