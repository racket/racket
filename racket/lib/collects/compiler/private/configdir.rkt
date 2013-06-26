#lang racket/base
(require racket/port)

(provide update-config-dir
         get-current-config-dir)

(define label #rx#"coNFIg dIRECTORy:")
(define max-dir-len 1024)

(define (update-config-dir dest path)
  (let ([path-bytes (cond [(path? path) (path->bytes path)]
                          [else (string->bytes/locale path)])])
    (unless ((bytes-length path-bytes) . <= . max-dir-len)
      (error 'update-config-dir "path too long: ~e" path))
    (let ([m (with-input-from-file dest
               (lambda ()
                 (regexp-match-positions label (current-input-port))))])
      (unless m
        (error 'update-ddl-dir "cannot find config path in file: ~e" dest))
      (with-output-to-file dest
        #:exists 'update
        (lambda ()
          (file-position (current-output-port) (cdar m))
          (write-bytes path-bytes)
          (write-byte 0))))))
      
(define (get-current-config-dir dest)
  (with-input-from-file dest
    (lambda ()
      (unless (regexp-match label (current-input-port))
        (error 'get-current-config-dir "cannot find config path in file: ~e" dest))
      (let ([p (make-limited-input-port (current-input-port) max-dir-len)])
        (let ([m (regexp-match #rx#"[^\0]*" p)])
          (bytes->path (car m)))))))
