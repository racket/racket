#lang scheme/base
(require scheme/cmdline scheme/string scheme/match scheme/pretty
         file/gzip file/gunzip net/base64)

(define (encode-exprs exprs)
  (define in
    (open-input-string
     (string-join (map (lambda (x) (format "~s" x)) exprs) " ")))
  (define out (open-output-bytes))
  (deflate in out)
  (base64-encode (get-output-bytes out)))

(define (encode-module)
  (define mod (parameterize ([read-accept-reader #t]) (read)))
  (when (eof-object? mod) (error 'encode-module "missing module"))
  (match mod
    [(list 'module m 'scheme/base (list '#%module-begin exprs ...))
     (write-bytes #"#lang s-exp framework/private/decode\n")
     (write-bytes (regexp-replace* #rx"\r\n" (encode-exprs exprs) #"\n"))]
    [else (error 'encode-module "cannot parse module, must use scheme/base")]))

(define (decode-module)
  (define mod (parameterize ([read-accept-reader #t]) (read)))
  (when (eof-object? mod) (error 'encode-module "missing module"))
  (match mod
    [(list 'module m 'framework/private/decode
           (list '#%module-begin exprs ...))
     (write-bytes #"#lang scheme/base\n")
     (let* ([data  (format "~a" exprs)]
            [data  (substring data 1 (sub1 (string-length data)))]
            [data  (string->bytes/utf-8 data)]
            [in    (open-input-bytes (base64-decode data))]
            [out   (open-output-string)]
            [out   (begin (inflate in out) (get-output-string out))]
            [exprs (read (open-input-string (string-append "(" out ")")))])
       (for ([expr (in-list exprs)])
         (pretty-print expr)))]
    [else (error 'decode-module "cannot parse module, must use scheme/base")]))

(command-line #:once-any
              ["-e" "encode" (encode-module) (exit)]
              ["-d" "decode" (decode-module) (exit)])
(printf "Use `-h' for help\n")
