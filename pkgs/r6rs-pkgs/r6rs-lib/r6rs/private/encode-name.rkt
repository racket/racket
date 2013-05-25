#lang scheme/base

(provide encode-name)

(define (encode-name s)
  (let ([s (if (symbol? s)
               (symbol->string s)
               s)])
    (cond
     [(regexp-match #rx"(.*?)([^a-zA-Z0-9_+-]+)(.*)" s)
      => (lambda (m)
           (string-append
            (cadr m)
            (apply
             string-append
             (map (lambda (c)
                    (let ([s (format "0~x" c)])
                      (string-append
                       "%"
                       (substring s (- (string-length s) 2)))))
                  (bytes->list (string->bytes/utf-8 (caddr m)))))
            (encode-name (cadddr m))))]
     [else s])))

