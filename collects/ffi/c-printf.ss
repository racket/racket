(module c-printf mzscheme

(require (lib "foreign.ss")) (unsafe!)

;; This code demonstrates how to interface `printf' which can be used with
;; different arities and types.  Also, `printf' is unsafe unless this code will
;; parse the format string and make sure that all the types match, instead,
;; this code demonstrates how to provide unsafe bindings in a way that forces
;; users to admit that `(c-printf-is-dangerous!)'.

;; It's not too useful, since the C printf will obviously ignore
;; `current-output-port'.

(provide* (unsafe c-printf))

(define interfaces (make-hash-table 'equal))

(define (c-printf fmt . args)
  (define itypes
    (cons _string
          (map (lambda (x)
                 (cond [(and (integer? x) (exact? x)) _int]
                       [(and (number? x) (real? x))   _double*]
                       [(string? x)  _string]
                       [(bytes? x)   _bytes]
                       [(symbol? x)  _symbol]
                       [else (error 'c-printf
                                    "don't know how to deal with ~e" x)]))
               args)))
  (let ([printf (hash-table-get interfaces itypes
                  (lambda ()
                    ;; Note: throws away the return value of printf
                    (let ([i (get-ffi-obj "printf" #f
                                          (_cprocedure itypes _void))])
                      (hash-table-put! interfaces itypes i)
                      i)))])
    (apply printf fmt args)))

(define-unsafer c-printf-is-dangerous!)

)
