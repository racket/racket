#lang racket/base
(require racket/port
         (submod "collects-path.rkt" set-executable-tag))

(provide update-config-dir
         get-current-config-dir)

(define label #rx#"coNFIg dIRECTORy:")

(define (update-config-dir dest path)
  (set-executable-tag 'update-config-dir
                      label
                      "config"
                      dest
                      #f
                      path
                      (if (path? path)
                          (path->bytes path)
                          (string->bytes/locale path))))
      
(define (get-current-config-dir dest)
  (with-input-from-file dest
    (lambda ()
      (unless (regexp-match label (current-input-port))
        (error 'get-current-config-dir "cannot find config path in file: ~e" dest))
      (let ([p (make-limited-input-port (current-input-port) max-dir-len)])
        (let ([m (regexp-match #rx#"[^\0]*" p)])
          (bytes->path (car m)))))))
