#lang racket/base
(require racket/pretty
         "../host/linklet.rkt"
         (prefix-in bootstrap: "../run/linklet.rkt")
         "../run/status.rkt"
         "c-encode.rkt")

(provide save-and-report-flattened!)

(define (save-and-report-flattened! flattened-linklet-expr
                                    print-extracted-to
                                    #:as-c? as-c?)
  (when print-extracted-to
    (log-status "Writing combined linklet to ~a" print-extracted-to)
    (call-with-output-file
     print-extracted-to
     #:exists 'truncate
     (lambda (o)
       (unless as-c?
         (displayln ";; This is not the original source code. Instead, this is the code after" o)
         (displayln ";; fully expanding and flattening into a single linklet." o))
       (define s-expr-o (if as-c?
                            (open-output-bytes)
                            o))
       (parameterize ([pretty-print-columns 120])
         (pretty-write flattened-linklet-expr s-expr-o))
       (when as-c?
         (encode-to-c (open-input-bytes (get-output-bytes s-expr-o)) o)))))

  ;; Tentatively compile and report size and time
  (log-status "Compiling flattened, just as a sanity check...")
  (define linklet
    (parameterize ([bootstrap:linklet-compile-to-s-expr #f])
      (compile-linklet flattened-linklet-expr)))

  (define code-bytes 
    (let ([o (open-output-bytes)])
      (write linklet o)
      (get-output-bytes o)))

  (log-status "Flattened code is ~s bytes" (bytes-length code-bytes))
  (log-status "Reading compiled code...")
  (time (let ([i (open-input-bytes code-bytes)])
          (parameterize ([read-accept-compiled #t])
            (void (read i))))))
