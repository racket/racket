;; A fast-loading script to delete "cstartup.inc" if it's not cify output
(module compile-startup '#%kernel
  (define-values (dest) (vector-ref (current-command-line-arguments) 0))
  (if (file-exists? dest)
      (if (call-with-input-file dest (lambda (i)
                                       (let-values ([(line) (read-line i)])
                                         (if (string? line)
                                             (regexp-match? #rx"^/[*] version" line)
                                             #f))))
          (void)
          (delete-file dest))
      (void)))
