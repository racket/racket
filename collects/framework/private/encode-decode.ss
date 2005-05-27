(module encode-decode mzscheme
  (require (lib "deflate.ss")
           (lib "match.ss"))
  (require-for-syntax (lib "inflate.ss")
                      (lib "string.ss"))
  
  (provide encode-sexp
           decode
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
                 (write `(module ,m mzscheme
                           (require (lib "encode-decode.ss" "framework" "private"))
                           (decode ,(encode-sexp `(begin ,@bodies))))
                        oport))
               'truncate 'text)]
            [else (error 'encode-module "cannot parse module")])))))
  
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
  
  (define-syntax (decode stx)
    (syntax-case stx ()
      [(_ arg ...)
       (andmap identifier? (syntax->list (syntax (arg ...))))
       (let ()
         (define (decode-sexp str)
           (let* ([loc 
                   (let loop ([chars (string->list str)])
                     (cond
                       [(null? chars) '()]
                       [(null? (cdr chars)) (error 'to-sexp "missing digit somewhere")]
                       [else (let ([fst (to-digit (car chars))]
                                   [snd (to-digit (cadr chars))])
                               (cons
                                (+ (* fst 16) snd)
                                (loop (cddr chars))))]))])
             (let-values ([(p-in p-out) (make-pipe)])
               (inflate (open-input-bytes (apply bytes loc)) p-out)
               (read p-in))))
         
         (define (to-digit char)
           (cond
             [(char<=? #\0 char #\9) 
              (- (char->integer char)
                 (char->integer #\0))]
             [(char<=? #\a char #\f) 
              (+ 10 (- (char->integer char)
                       (char->integer #\a)))]))

         (datum->syntax-object
          stx
          (decode-sexp 
           (apply 
            string-append
            (map (λ (x) (symbol->string (syntax-e x)))
                 (syntax->list (syntax (arg ...))))))
          stx))])))