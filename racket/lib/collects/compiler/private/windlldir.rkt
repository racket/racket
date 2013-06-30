(module windlldir racket/base
  (require racket/port
           "winutf16.rkt")

  (provide update-dll-dir
           get-current-dll-dir)

  (define label (byte-regexp (bytes->utf-16-bytes #"dLl dIRECTORy:")))
  (define max-dir-len (* 512 2)) ; sizeof(wchar_t) is 2

  (define (update-dll-dir dest path)
    (let ([path-bytes (bytes->utf-16-bytes
                       (cond [(eq? path #t) #"<system>"]
                             [(path? path) (path->bytes path)]
                             [else (string->bytes/locale path)]))])
      (unless ((bytes-length path-bytes) . <= . max-dir-len)
        (error 'update-dll-dir "path too long: ~e" path))
      (let ([m (with-input-from-file dest
                 (lambda ()
                   (regexp-match-positions label (current-input-port))))])
        (unless m
          (error 'update-ddl-dir "cannot find DLL path in file: ~e" dest))
        (with-output-to-file dest
          (lambda ()
            (file-position (current-output-port) (cdar m))
            (write-bytes path-bytes)
            (write-byte 0))
          #:exists 'update))))
      
  (define (get-current-dll-dir dest)
    (with-input-from-file dest
      (lambda ()
        (unless (regexp-match label (current-input-port))
          (error 'get-current-dll-dir "cannot find DLL path in file: ~e" dest))
        (let ([p (make-limited-input-port (current-input-port) max-dir-len)])
          (let ([m (regexp-match #rx#"(?:[^\0].|.[^\0])*" p)])
            (bytes->path (utf-16-bytes->bytes (car m)))))))))
