#lang racket/base
(require file/unzip
         file/gunzip
         racket/runtime-path
         racket/port
         racket/file
         tests/eli-tester)

(define-runtime-path unzip-me.zip "unzip-me.zip")

;; test-me.zip's directory structure is test-zip/1/data.dat
(define (test-with-unzip in)
  (call-with-unzip in
                   (lambda (tmp_dir)
                     (with-input-from-file (build-path tmp_dir "test-zip" "1" "data.dat")
                       (lambda ()
                         (test (read-line) => "chenxiao"))))))

(define (test-with-direct-unzip in unzip
                                #:check-attributes? [check-attributes? #f]
                                #:check-timestamps? [check-timestamps? check-attributes?])
  (define dir (make-temporary-directory))
  (let ([in (path->complete-path in)])
    (parameterize ([current-directory dir])
      (unzip in)))
  (with-input-from-file (build-path dir "test-zip" "1" "data.dat")
    (lambda ()
      (test (read-line) => "chenxiao")))
  (when check-timestamps?
    (let loop ([dir dir])
      (for ([c (in-list (directory-list dir #:build? #t))])
        (define (check-date)
          (define d (seconds->date (file-or-directory-modify-seconds c)))
          (test (list c 2018 2) list c (date-year d) (date-month d)))
        (cond
          [(file-exists? c)
           (check-date)
           (define p (file-or-directory-permissions c))
           (define read-only? (and check-attributes?
                                   (let-values ([(base name dir?) (split-path c)])
                                     (equal? "readonly.txt" (path->string name)))))
           (test (list c read-only?) 'permissions (list c (and (memq 'write p) #t)))]
          [else
           (when check-attributes?
             (check-date))
           (loop c)]))))
  (delete-directory/files dir))

(define (test-with-unzip-entry)
  (call-with-unzip-entry unzip-me.zip
                         (build-path "test-zip" "1" "data.dat")
                         (lambda (tmp_file)
                           (with-input-from-file tmp_file
                             (lambda ()
                               (test (read-line) => "chenxiao"))))))

(define (run-tests)
  (test-with-unzip unzip-me.zip)
  (test-with-direct-unzip unzip-me.zip unzip)
  (test-with-direct-unzip unzip-me.zip (lambda (file)
                                         (unzip file (make-filesystem-entry-reader))))
  (test-with-direct-unzip unzip-me.zip (let ([reader (make-filesystem-entry-reader)])
                                         (lambda (file)
                                           (unzip file (lambda (name dir? in)
                                                         (reader name dir? in))
                                                  #:preserve-timestamps? #f))))
  (test-with-direct-unzip unzip-me.zip (let ([reader (make-filesystem-entry-reader)])
                                         (lambda (file)
                                           (unzip file (lambda (name dir? in ts)
                                                         (reader name dir? in ts))
                                                  #:preserve-timestamps? #t)))
                          #:check-timestamps? #t)
  (test-with-direct-unzip unzip-me.zip (let ([reader (make-filesystem-entry-reader)])
                                         (lambda (file)
                                           (unzip file (lambda (name dir? in ts)
                                                         (reader name dir? in ts))
                                                  #:preserve-attributes? #t)))
                          #:check-attributes? #t)
  (call-with-input-file* unzip-me.zip test-with-unzip)
  (call-with-input-file* unzip-me.zip
                         (lambda (in_port) (test-with-unzip (input-port-append #f in_port))))
  (test-with-unzip-entry)

  (test (let ()
          (define out (open-output-bytes))
          (define infinite-voids
            (make-input-port
             'voids
             (lambda (s) (lambda args 'void))
             (lambda (skip s evt) (lambda args 'void))
             void))
          (inflate infinite-voids out))
        =error> "non-character in an unsupported context")

  (define sample-zip
    (bytes-append
     #"PK\3\4\24\0\0\0\b\0\35FbR\237b\371AO\0\0\0x\0\0\0\5\0\34\0x.rkt"
     #"UT\t\0\3j^>`l^>`ux\v\0\1\4\365\1\0\0\4\0\0\0\0S\316I\314KW(JL\316N-\321OJ,"
     #"N\345\342\322HIM\313\314KU\320HS\250\320\344RP\320\310LS\320(KM."
     #"\311/\262W\250P0\4\211\201\200\2066LX7'5/\275$\3\242\34\n0\244`r\352\371"
     #"\331@6\0PK\1\2\36\3\24\0\0\0\b\0\35FbR\237b\371AO\0\0\0x\0\0\0\5\0"
     #"\30\0\0\0\0\0\1\0\0\0\244\201\0\0\0\0x.rktUT\5\0\3j^>`ux\v\0\1\4\365\1"
     #"\0\0\4\0\0\0\0PK\5\6\0\0\0\0\1\0\1\0K\0\0\0\216\0\0\0\0\0"))

  ;; Check that a blocked input port can be interrupted:
  (test (let ()
          (define-values (i o) (make-pipe))
          (void (write-bytes (subbytes sample-zip 0 100) o))
          (let ([t (thread
                    (lambda ()
                      (with-handlers ([exn:break? void])
                        (call-with-unzip i void))))])
            (sync (system-idle-evt))
            (break-thread t)
            (sync t)
            'done))
        => 'done)

  ;; Check that truncated input doesn't stall (regression test for inflate layer):
  (test (let ()
          (call-with-unzip (open-input-bytes (subbytes sample-zip 0 130)) void))
        =error> "unexpected EOF")

  ;; Check that truncated input doesn't stall (regression test for unzip layer):
  (test (let ()
          (call-with-unzip (open-input-bytes (subbytes sample-zip 0 131)) void))
        =error> "bad inflate code")

  (test (call-with-unzip (open-input-bytes #"not a zip stream") void  #:must-unzip? #f)
        => (void))
  (test (call-with-unzip (open-input-bytes #"not a zip stream")
                         void
                         #:must-unzip? #t)
        =error> "input does not appear to be an archive"))


(provide tests)
(module+ main (tests))
(define (tests) (test do (run-tests)))
