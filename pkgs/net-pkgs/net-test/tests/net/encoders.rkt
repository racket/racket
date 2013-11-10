#lang scheme
(require net/base64 net/qp tests/eli-tester)

(define tricky-strings
  (let ([net.rktl (collection-file-path "base64.rkt" "tests" "net")])
    (list (make-bytes 200 32)
          (make-bytes 200 9)
          (make-bytes 200 (char->integer #\x))
          (make-bytes 201 (char->integer #\x))
          (make-bytes 202 (char->integer #\x))
          (make-bytes 203 (char->integer #\x))
          (make-bytes 204 (char->integer #\x))
          (list->bytes (for/list ([i (in-range 256)]) i))
          ;; Something that doesn't end with a LF:
          (bytes-append (with-input-from-file net.rktl
                          (lambda () (read-bytes 500)))
           #"xxx")
          ;; CRLF:
          (regexp-replace #rx#"\r?\n"
                          (with-input-from-file net.rktl
                            (lambda () (read-bytes 500)))
           #"\r\n"))))

(define (check-same encode decode port line-rx max-w)
  (let ([p (open-output-bytes)])
    (copy-port port p)
    (let ([bytes (get-output-bytes p)]
          [r (open-output-bytes)])
      (encode (open-input-bytes bytes) r)
      (let ([p (open-input-bytes (get-output-bytes r))])
        (let loop ()
          (let ([l (read-bytes-line p 'any)])
            (unless (eof-object? l)
              (test ; #:failure-message (format "line too long; ~s" encode)
                    (<= (bytes-length l) max-w))
              (let ([m (regexp-match-positions line-rx l)])
                (test ; #:failure-message (format "bad line; ~s" encode)
                      (and m (= (bytes-length l) (cdar m)))))
              (loop))))
        (let ([q (open-output-bytes)])
          (decode (open-input-bytes (get-output-bytes r)) q)
          (unless (equal? (get-output-bytes q) bytes)
            (with-output-to-file "/tmp/x0" (lambda () (display (get-output-bytes r))) 'truncate)
            (with-output-to-file "/tmp/x1" (lambda () (display (get-output-bytes q))) 'truncate)
            (with-output-to-file "/tmp/x2" (lambda () (display bytes)) 'truncate)
            (error 'decode "failed")))))))

(define ((check-same-file encode decode line-rx max-w) file)
  (call-with-input-file file
    (lambda (p) (check-same encode decode p line-rx max-w))))

(define (check-same-all encode decode line-rx max-w)
  (for-each (lambda (tricky-string)
              (check-same encode decode
                          (open-input-bytes tricky-string)
                          line-rx max-w))
            tricky-strings)
  (let* ([dir (collection-path "tests" "racket")]
         [files (filter-map
                 (lambda (f)
                   ;; check 1/4 of the files, randomly
                   (let ([p (build-path dir f)])
                     (and (zero? (random 4))
                          (not (regexp-match #rx"^flat.*\\.rktl$"
                                             (path-element->string f)))
                          (file-exists? p)
                          p)))
                 (directory-list dir))])
    (for-each (check-same-file encode decode line-rx max-w) files)))

(provide tests)
(module+ main (tests))
(define (tests)
  (test
   do (check-same-all (lambda (i o) (qp-encode-stream i o))
                      qp-decode-stream
                      #rx#"^(|[\t \41-\176]*[\41-\176]+)$"
                      76)
   do (check-same-all base64-encode-stream
                      base64-decode-stream
                      #rx#"^[0-9a-zA-Z+=/]*$"
                      72)))

#|
Use this to compare base64 encode/decode against the unix utilities
(require net/base64 scheme/system)
(define (base64-encode* bstr)
  (let ([o (open-output-bytes)])
     (parameterize ([current-output-port o]
                    [current-input-port (open-input-bytes bstr)])
       (system "base64-encode"))
     (let* ([o (get-output-bytes o)]
            [o (regexp-replace #rx#"(.)(?:\r?\n)?$" o #"\\1\r\n")]
            [o (regexp-replace* #rx#"\r?\n" o #"\r\n")])
       o)))
(define (base64-decode* bstr)
  (let ([o (open-output-bytes)])
     (parameterize ([current-output-port o]
                    [current-input-port (open-input-bytes bstr)])
       (system "base64-decode"))
     (get-output-bytes o)))
(define (check-base64-encode bstr)
  (equal? (base64-encode bstr) (base64-encode* bstr)))
(define (check-base64-decode bstr)
  (equal? (base64-decode bstr) (base64-decode* bstr)))
(define (check-base64-both bstr)
  (let ([en (base64-encode bstr)])
    (and (equal? en (base64-encode* bstr))
         (equal? (base64-decode en) (base64-decode* en)))))
|#

(module+ test (require (submod ".." main))) ; for raco test & drdr
