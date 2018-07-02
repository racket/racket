#lang racket/base
(require racket/file
         file/gzip
         file/gunzip)

(provide enable-compress!
         compress-enabled?
         adjust-compress)

(define compress? #f)

(define (enable-compress!)
  (set! compress? #t))

(define (compress-enabled?)
  compress?)

(define (reencode bstr encode)
  (let ([o (open-output-bytes)]
        [i (open-input-bytes bstr)])
    (let loop ()
      (unless (eof-object? (peek-byte i))
        (encode i o)
        (loop)))
    (get-output-bytes o)))

(define (adjust-compress bstr)
  (if (bytes=? #"\0\0\0\0chez" (subbytes bstr 0 8))
      (if compress?
          (reencode bstr (lambda (i o) (gzip-through-ports i o #f 0)))
          bstr)
      (if compress?
          bstr
          (reencode bstr gunzip-through-ports))))

(module+ main
  (require racket/cmdline)
      
  (command-line
   #:once-each
   [("--compress") "Leave compiled code files as compressed"
    (enable-compress!)]
   #:args path
   (for ([path (in-list path)])
     (define bstr (file->bytes path))
     (define bstr2 (adjust-compress bstr))
     (printf "~s ~s\n" path (equal? bstr bstr2))
     (unless (equal? bstr bstr2)
       (call-with-output-file*
        path
        #:exists 'truncate/replace
        (lambda (o)
          (write-bytes bstr2 o)))))))
