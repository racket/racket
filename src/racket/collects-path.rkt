
;; This module is executed by the install process to update
;;  the embedded path to "collects" in an executable.

;; written in #%kernel because it's loaded with -c (ie, no compiled files)
(module collects-path '#%kernel  
  (define-values (label) #rx#"coLLECTs dIRECTORy:")
  
  (let-values ([(dest) (vector-ref (current-command-line-arguments) 0)]
               [(path) (vector-ref (current-command-line-arguments) 1)])
    (let-values ([(i o) (open-input-output-file dest 'update)])
      (let-values ([(m) (regexp-match-positions label i)]
                   [(path) (if (string? path)
                               (string->path path)
                               path)])        
        (if m
            (void)
            (error 'set-collects-path
                   "cannot find collection-path label in executable file"))
        (file-position o (cdar m))
        (write-bytes (path->bytes path) o)
        (write-byte 0 o)
        (write-byte 0 o)
        (close-input-port i)
        (close-output-port o)))))
