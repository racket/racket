#lang racket/base
(require racket/file
         racket/port
         file/zip
         file/unzip)

;; checks that timestamps and file permissions
;; are zipped and unzipped correctly

(define dir (make-temporary-directory))
(define zip-file (make-temporary-file "~a.zip"))

(call-with-output-file (build-path dir "x")
  (lambda (o) (fprintf o "123\n")))
(call-with-output-file (build-path dir "y")
  (lambda (o) (fprintf o "abc\n")))
(make-directory (build-path dir "d"))
(call-with-output-file (build-path dir "d" "z")
  (lambda (o) (fprintf o "xyz\n")))

(file-or-directory-permissions (build-path dir "d" "z")
                               (if (eq? 'windows (system-type))
                                   #o555
                                   #o444))

(call-with-output-file zip-file
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list "x" "y" "d" "d/z") out))))

;; ".zip" dates are rounded to even numbers, but second 59 caps at 58 (MS-DOS time format)
(define (rounded v)
  (cond
    [(= (modulo v 60) 59) (- v 1)]
    [else (+ v (modulo v 2))]))

(define (check-same a b)
  (unless (equal? a b)
    (error "different" a b)))

(define (check-true v)
  (unless v
    (error "expected true" v)))

(define (check-exn-message rx thunk)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (unless (regexp-match? rx (exn-message exn))
                       (raise exn))
                     #t)])
    (thunk)
    (error "expected exception")))

(define (test unzip)
  (define dir2 (make-temporary-directory))
  (parameterize ([current-directory dir2])
    (unzip zip-file #:preserve-attributes? #t))

  (define (compare f)
    (check-same (file->string (build-path dir f))
                (file->string (build-path dir2 f)))
    (check-same (rounded (file-or-directory-modify-seconds (build-path dir f)))
                (rounded (file-or-directory-modify-seconds (build-path dir2 f))))
    (check-same (file-or-directory-permissions (build-path dir f))
                (file-or-directory-permissions (build-path dir2 f))))

  (compare "x")
  (compare "y")
  (compare (build-path "d" "z"))

  (unless (eq? 'windows (system-type))
    (check-same (rounded (file-or-directory-modify-seconds (build-path dir "d")))
                (rounded (file-or-directory-modify-seconds (build-path dir2 "d")))))

  (delete-directory/files dir2))

(define (local-compression-method path)
  (call-with-input-file path
    (lambda (in)
      (file-position in 8)
      (integer-bytes->integer (read-bytes 2 in) #f #f))))

(define (local-entry-info path)
  (call-with-input-file path
    (lambda (in)
      (let loop ([infos null])
        (define sig-bstr (read-bytes 4 in))
        (cond
          [(eof-object? sig-bstr) (reverse infos)]
          [else
           (define sig (integer-bytes->integer sig-bstr #f #f))
           (cond
             [(= sig #x04034b50)
              (read-bytes 2 in) ; version
              (read-bytes 2 in) ; bits
              (define compression (integer-bytes->integer (read-bytes 2 in) #f #f))
              (read-bytes 2 in) ; time
              (read-bytes 2 in) ; date
              (read-bytes 4 in) ; crc
              (define compressed-size (integer-bytes->integer (read-bytes 4 in) #f #f))
              (read-bytes 4 in) ; uncompressed
              (define filename-length (integer-bytes->integer (read-bytes 2 in) #f #f))
              (define extra-length (integer-bytes->integer (read-bytes 2 in) #f #f))
              (define filename (read-bytes filename-length in))
              (read-bytes extra-length in)
              (read-bytes compressed-size in)
              (loop (cons (cons filename compression) infos))]
             [else (reverse infos)])])))))

(define default-zip (make-temporary-file "~a-default.zip"))
(call-with-output-file default-zip
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list "x") out))))

(define stored-zip (make-temporary-file "~a-stored.zip"))
(call-with-output-file stored-zip
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list "x") out #:level 0))))

(check-same (local-compression-method default-zip) 8)
(check-same (local-compression-method stored-zip) 0)

(define ordered-output-zip (make-temporary-file "~a-ordered-output.zip"))
(call-with-output-file ordered-output-zip
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list (make-zip-entry "y" 0)
                         (make-zip-entry "x" 6))
                   out))))

(check-same (local-entry-info ordered-output-zip)
            (list (cons #"y" 0)
                  (cons #"x" 8)))

(define ordered-zip (make-temporary-file "~a-ordered.zip"))
(delete-file ordered-zip)
(parameterize ([current-directory dir])
  (zip ordered-zip
       (make-zip-entry "y" 0)
       "x"))

(check-same (local-entry-info ordered-zip)
            (list (cons #"y" 0)
                  (cons #"x" 8)))

(define synthetic-zip (make-temporary-file "~a-synthetic.zip"))
(call-with-output-file synthetic-zip
  #:exists 'truncate
  (lambda (out)
    (zip->output
     (list (make-zip-entry 'file "mimetype" #"application/epub+zip" #f #hash() 0)
           (make-zip-entry 'directory "META-INF" #f 0 #hash() 0)
           (make-zip-entry 'file "META-INF/container.xml" #"<container/>" #f #hash() 6))
     out)))

(check-same (local-entry-info synthetic-zip)
            (list (cons #"mimetype" 0)
                  (cons #"META-INF/" 0)
                  (cons #"META-INF/container.xml" 8)))

(define synthetic-dir (make-temporary-directory))
(parameterize ([current-directory synthetic-dir])
  (unzip synthetic-zip))

(check-same (file->string (build-path synthetic-dir "mimetype"))
            "application/epub+zip")
(check-same (file->string (build-path synthetic-dir "META-INF" "container.xml"))
            "<container/>")

(define attr-seconds 1700000001)
(define synthetic-extra-zip (make-temporary-file "~a-synthetic-extra.zip"))
(call-with-output-file synthetic-extra-zip
  #:exists 'truncate
  (lambda (out)
    (zip->output
     (list (make-zip-entry 'file
                           "thunk.txt"
                           (lambda () (open-input-bytes #"from thunk"))
                           #f
                           #hash((modify-seconds . 1700000000))
                           6)
           (make-zip-entry 'file
                           "empty.txt"
                           #""
                           0
                           #hash()
                           0)
           (make-zip-entry 'file
                           "attrs.txt"
                           #"attrs"
                           5
                           #hash((permissions . 420)
                                 (modify-seconds . 1700000001))
                           0))
     out)))

(check-same (local-entry-info synthetic-extra-zip)
            (list (cons #"thunk.txt" 8)
                  (cons #"empty.txt" 0)
                  (cons #"attrs.txt" 0)))

(define synthetic-extra-dir (make-temporary-directory))
(parameterize ([current-directory synthetic-extra-dir])
  (unzip synthetic-extra-zip #:preserve-attributes? #t))

(check-same (file->string (build-path synthetic-extra-dir "thunk.txt"))
            "from thunk")
(check-same (file->string (build-path synthetic-extra-dir "empty.txt"))
            "")
(check-same (file->string (build-path synthetic-extra-dir "attrs.txt"))
            "attrs")
(check-same (rounded (file-or-directory-modify-seconds
                      (build-path synthetic-extra-dir "attrs.txt")))
            (rounded attr-seconds))
(unless (eq? 'windows (system-type))
  (check-same (file-or-directory-permissions (build-path synthetic-extra-dir "attrs.txt"))
              '(write read)))

(define size-mismatch-zip (make-temporary-file "~a-size-mismatch.zip"))
(check-true
 (check-exn-message
  #rx"entry size does not match content length"
  (lambda ()
    (call-with-output-file size-mismatch-zip
      #:exists 'truncate
      (lambda (out)
        (zip->output
         (list (make-zip-entry 'file "bad.txt" #"abc" 4 #hash() 0))
         out))))))

(define big-content
  (apply bytes-append
         (for/list ([i (in-range 1024)])
           #"abcdefghijklmnopqrstuvwxyz012345")))
(define streamed-zip (make-temporary-file "~a-streamed.zip"))
(call-with-output-file streamed-zip
  #:exists 'truncate
  (lambda (out)
    (zip->output
     (list (make-zip-entry 'file
                           "port.txt"
                           (open-input-bytes #"from port")
                           9
                           #hash()
                           0)
           (make-zip-entry 'file
                           "big.bin"
                           big-content
                           (bytes-length big-content)
                           #hash()
                           6))
     out)))

(check-same (local-entry-info streamed-zip)
            (list (cons #"port.txt" 0)
                  (cons #"big.bin" 8)))

(define streamed-dir (make-temporary-directory))
(parameterize ([current-directory streamed-dir])
  (unzip streamed-zip))

(check-same (file->string (build-path streamed-dir "port.txt"))
            "from port")
(check-same (file->bytes (build-path streamed-dir "big.bin"))
            big-content)

(check-true
 (check-exn-message
  #rx"directory entries cannot have content"
  (lambda ()
    (with-output-to-bytes
     (lambda ()
       (zip->output (list (make-zip-entry 'directory "bad-dir" #"x" 0 #hash() 0))))))))

(check-true
 (check-exn-message
  #rx"directory entries cannot have a non-zero size"
  (lambda ()
    (with-output-to-bytes
     (lambda ()
       (zip->output (list (make-zip-entry 'directory "bad-dir" #f 1 #hash() 0))))))))

(check-true
 (check-exn-message
  #rx"input-port\\?|procedure\\?|bytes\\?"
  (lambda ()
    (with-output-to-bytes
     (lambda ()
       (zip->output (list (make-zip-entry 'file "bad-file" 17 0 #hash() 0))))))))

(check-true
 (check-exn-message
  #rx"integer-in 0 9"
  (lambda ()
    (with-output-to-bytes
     (lambda ()
       (zip->output (list (make-zip-entry 'file "bad-level" #"x" 1 #hash() 10))))))))

(check-true
 (check-exn-message
  #rx"relative-path\\?"
  (lambda ()
    (with-output-to-bytes
     (lambda ()
       (zip->output
        (list (make-zip-entry 'file
                              (build-path (current-directory) "bad-abs.txt")
                              #"x"
                              1
                              #hash()
                              0))))))))

(define six-arg-path-zip (make-temporary-file "~a-six-arg-path.zip"))
(call-with-output-file six-arg-path-zip
  #:exists 'truncate
  (lambda (out)
    (parameterize ([current-directory dir])
      (zip->output (list (make-zip-entry 'path "x" #f #f #hash() 0))
                   out))))

(check-same (local-entry-info six-arg-path-zip)
            (list (cons #"x" 0)))

"waiting 2 seconds"
(sleep 2)

(test unzip)
(test (lambda (path #:preserve-attributes? yes)
        (call-with-input-file
         path
         (lambda (in)
           (define zd (read-zip-directory in))
           (define todos
             (for/list ([e (in-list (zip-directory-entries zd))])
               (unzip-entry in zd e #:preserve-attributes? #t)))
           (for ([todo (in-list todos)])
             (when todo (todo)))))))

(delete-file zip-file)
(delete-file default-zip)
(delete-file stored-zip)
(delete-file ordered-output-zip)
(delete-file ordered-zip)
(delete-file synthetic-zip)
(delete-file synthetic-extra-zip)
(delete-file size-mismatch-zip)
(delete-file streamed-zip)
(delete-file six-arg-path-zip)
(delete-directory/files synthetic-dir)
(delete-directory/files synthetic-extra-dir)
(delete-directory/files streamed-dir)
(delete-directory/files dir)
