(module windlldir racket/base
  (require racket/port
           racket/promise
           (submod "collects-path.rkt" set-executable-tag)
           "winutf16.rkt")

  (provide update-dll-dir
           get-current-dll-dir
	   current-no-dlls?)

  (define label (delay/sync (byte-regexp (bytes->utf-16-bytes #"dLl dIRECTORy:"))))

  (define (update-dll-dir dest path)
    (set-executable-tag 'update-dll-dir
                        (force label)
                        "DLL"
                        dest
                        #f
                        path
                        (bytes->utf-16-bytes
                         (cond [(eq? path #t) #"<system>"]
                               [(path? path) (path->bytes path)]
                               [else (string->bytes/locale path)]))))

  (define (get-current-dll-dir dest)
    (with-input-from-file dest
      (lambda ()
        (unless (regexp-match (force label) (current-input-port))
          (error 'get-current-dll-dir "cannot find DLL path in file: ~e" dest))
        (let ([p (make-limited-input-port (current-input-port) max-dir-len)])
          (let ([m (regexp-match #rx#"(?:[^\0].|.[^\0])*" p)])
            (bytes->path (utf-16-bytes->bytes (car m))))))))

  (define (current-no-dlls? dest)
    (regexp-match? #rx#"^<" (get-current-dll-dir dest))))
