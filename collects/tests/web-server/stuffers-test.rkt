#lang racket
(require rackunit
         web-server/stuffers
         web-server/private/servlet
         web-server/http
         net/url
         racket/serialize)
(provide all-stuffers-tests)

(define (stuffer-test s)
  (define x (string->bytes/utf-8 (number->string (random 1000))))
  (check-equal? ((stuffer-out s) ((stuffer-in s) x))
                x))

(define (store-test s)
  (define x (string->bytes/utf-8 (number->string (random 1000))))
  (define y (string->bytes/utf-8 (number->string (random 1000))))
  (check-equal? (begin ((store-write s) x y)
                       ((store-read s) x))
                y))

(define prep-stuffer
  (make-stuffer
   (lambda (v) (bytes-append #"20" v))
   (lambda (v) (subbytes v 2))))

(define (context-wrap thnk)
  (parameterize ([current-execution-context
                               (make-execution-context 
                                (make-request #"GET" (string->url "http://www.google.com") 
                                              empty (delay empty)
                                              #"" "127.0.0.1" 80
                                              "127.0.0.1"))])
    (thnk)))

(define all-stuffers-tests
  (test-suite
   "Stuffers"
   
   (test-suite "stuffer"
               (test-case "id-stuffer" (stuffer-test id-stuffer))
               
               (test-case "stuffer-compose" (stuffer-test (stuffer-compose id-stuffer id-stuffer)))
               (test-case "stuffer-compose" (stuffer-test (stuffer-compose prep-stuffer id-stuffer)))
               (test-case "stuffer-compose" (stuffer-test (stuffer-compose id-stuffer prep-stuffer)))
               (test-case "stuffer-compose" (stuffer-test (stuffer-compose prep-stuffer prep-stuffer)))
               
               (test-case "stuffer-sequence" (stuffer-test (stuffer-sequence id-stuffer id-stuffer)))
               (test-case "stuffer-sequence" (stuffer-test (stuffer-sequence prep-stuffer id-stuffer)))
               (test-case "stuffer-sequence" (stuffer-test (stuffer-sequence id-stuffer prep-stuffer)))
               (test-case "stuffer-sequence" (stuffer-test (stuffer-sequence prep-stuffer prep-stuffer)))
               
               (test-case "stuffer-if" (stuffer-test (stuffer-if (lambda (v) #t) id-stuffer)))
               (test-case "stuffer-if" (stuffer-test (stuffer-if (lambda (v) #f) id-stuffer)))
               (test-case "stuffer-if" (stuffer-test (stuffer-if (lambda (v) #t) prep-stuffer)))
               (test-case "stuffer-if" (stuffer-test (stuffer-if (lambda (v) #f) prep-stuffer)))
               
               (test-case "stuffer-chain" (stuffer-test (stuffer-chain)))
               (test-case "stuffer-chain" (stuffer-test (stuffer-chain (lambda (v) #f))))
               (test-case "stuffer-chain" (stuffer-test (stuffer-chain (lambda (v) #f) prep-stuffer)))
               (test-case "stuffer-chain" (stuffer-test (stuffer-chain prep-stuffer (lambda (v) #f) prep-stuffer))))
   
   (test-suite "base64"
               (test-case "base64-stuffer" (stuffer-test base64-stuffer)))
   
   (test-suite "gzip"
               (test-case "gzip-stuffer" (stuffer-test gzip-stuffer)))
   
   (test-suite "serialize"
               (test-case "serialize-stuffer" (stuffer-test serialize-stuffer)))
   
   (test-suite "store"
               (test-case "dir-store" (store-test (dir-store (find-system-path 'temp-dir)))))
   
   (test-suite "hash"
               (test-case "md5-stuffer" (stuffer-test (md5-stuffer (find-system-path 'temp-dir)))))
   
   (test-suite "hmac-sha1"
               (test-case "hmac-sha1 len" 
                          (check-equal? (bytes-length (HMAC-SHA1 (make-bytes 10 (random 255))
                                                                 (make-bytes 10 (random 255))))
                                        20))
               
               (test-case "hmac-sha1 len" 
                          (check-equal? (bytes-length (HMAC-SHA1 (make-bytes 10 (random 255))
                                                                 (make-bytes 100 (random 255))))
                                        20))
               
               (test-case "hmac-sha1 len" 
                          (check-equal? (bytes-length (HMAC-SHA1 (make-bytes 10 (random 255))
                                                                 (make-bytes 1000 (random 255))))
                                        20))
               
               (test-case "hmac-sha1 stuffer" (stuffer-test (HMAC-SHA1-stuffer (make-bytes 10 (random 255)))))
               
               (test-case "hmac-sha1 stuffer (err)"
                          (check-exn exn?
                                     (lambda ()
                                       ((stuffer-out (HMAC-SHA1-stuffer (make-bytes 10 (random 255))))
                                        #"123456789012345678901234567890")))))
   
   (test-suite "stuff-url"
               (test-case "make-default-stuffer"
                          (context-wrap 
                           (lambda () 
                             (stuffer-test (make-default-stuffer (find-system-path 'temp-dir))))))
               
               (test-case "is-url-too-big?"
                          (context-wrap
                           (lambda ()
                             (check-false (is-url-too-big? (make-bytes 1 65))))))
               (test-case "is-url-too-big?"
                          (context-wrap
                           (lambda ()
                             (check-false (is-url-too-big? (make-bytes 10 65))))))
               (test-case "is-url-too-big?"
                          (context-wrap
                           (lambda ()
                             (check-false (is-url-too-big? (make-bytes 100 65))))))
               (test-case "is-url-too-big?"
                          (context-wrap
                           (lambda ()
                             (check-false (is-url-too-big? (make-bytes 1000 65))))))
               (test-case "is-url-too-big?"
                          (context-wrap
                           (lambda ()
                             (check-not-false (is-url-too-big? (make-bytes 3000 65)))))))))
