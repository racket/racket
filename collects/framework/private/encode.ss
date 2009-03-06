#lang scheme/base
(require mzlib/deflate
         mzlib/match
         mzlib/pretty)
(require (for-syntax mzlib/inflate
                     mzlib/string))

(provide encode-sexp
         encode-module)

(define (encode-module in-filename out-filename)
  (call-with-input-file in-filename
    (λ (port)
      (let ([mod (read port)])
        (unless (eof-object? (read port))
          (error 'encode-module "found an extra expression"))
        (match mod 
          [`(module ,m mzscheme ,@(bodies ...))
           (call-with-output-file out-filename
             (λ (oport)
               (let ([chopped (chop-up (encode-sexp `(begin ,@bodies)))])
                 (fprintf oport "(module ~a mzscheme\n" m)
                 (fprintf oport "  (require framework/private/decode)\n")
                 (fprintf oport "  (decode ~a" (car chopped))
                 (for-each (lambda (chopped)
                             (fprintf oport "\n          ~a" chopped))
                           (cdr chopped))
                 (fprintf oport "))\n")))
             'truncate 'text)]
          [else (error 'encode-module "cannot parse module")])))))

(define (chop-up sym)
  (let ([chopping-point 50])
    (let loop ([str (symbol->string sym)])
      (cond
        [(<= (string-length str) chopping-point)
         (list (string->symbol str))]
        [else
         (cons (string->symbol (substring str 0 chopping-point))
               (loop (substring str chopping-point (string-length str))))]))))

(define (encode-sexp sexp)
  (define (str->sym string)
    (string->symbol
     (apply 
      string-append
      (map
       (λ (x) 
         (to-hex x))
       (bytes->list string)))))
  
  (define (to-hex n)
    (let ([digit->hex
           (λ (d)
             (cond
               [(<= d 9) d]
               [else (integer->char (+ d -10 (char->integer #\a)))]))])
      (cond
        [(< n 16) (format "0~a" (digit->hex n))]
        [else (format "~a~a" 
                      (digit->hex (quotient n 16))
                      (digit->hex (modulo n 16)))])))
  
  (let ([in (open-input-string (format "~s" sexp))]
        [out (open-output-bytes)])
    (deflate in out)
    (str->sym (get-output-bytes out))))
