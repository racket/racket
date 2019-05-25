#lang racket/base
(require racket/file
         racket/system
         file/gzip
         file/gunzip)

(provide enable-compress!
         compress-enabled?
         set-compress-format!
         get-compress-format
         adjust-compress)

(define compress? #f)
(define compress-format 'lz4)

(define (enable-compress!)
  (set! compress? #t))

(define (compress-enabled?)
  compress?)

(define (set-compress-format! fmt)
  (unless (memq fmt '(lz4 gzip))
    (error 'set-compress-format! "bad format: ~v" fmt))
  (set! compress-format fmt))

(define (get-compress-format)
  compress-format)

(define (reencode bstr encode)
  (let ([o (open-output-bytes)]
        [i (open-input-bytes bstr)])
    (let loop ()
      (unless (eof-object? (peek-byte i))
        (encode i o)
        (loop)))
    (get-output-bytes o)))

(define (lz4 flag bstr)
  (define o (open-output-bytes))
  (unless (parameterize ([current-input-port (open-input-bytes bstr)]
                         [current-output-port o])
            (system* (find-executable-path "lz4") flag))
    (error "lz4 failed"))
  (get-output-bytes o))

(define (adjust-compress bstr)
  (cond
    [(bytes=? #"\0\0\0\0chez" (subbytes bstr 0 8))
     ;; source is not compressed
     (cond
       [(and compress? (eq? compress-format 'gzip))
        (reencode bstr (lambda (i o) (gzip-through-ports i o #f 0)))]
       [(and compress? (eq? compress-format 'lz4))
        (lz4 "-z" bstr)]
       [else bstr])]
    [(bytes=? #"\4\"M\30" (subbytes bstr 0 4))
     ;; source is LZ4
     (cond
       [(and compress? (eq? compress-format 'gzip))
        (reencode (lz4 "-d" bstr) (lambda (i o) (gzip-through-ports i o #f 0)))]
       [(and compress? (eq? compress-format 'lz4))
        bstr]
       [else (lz4 "-d" bstr)])]
    [(bytes=? #"\37\213\b" (subbytes bstr 0 3))
     ;; source is gzip
     (cond
       [(and compress? (eq? compress-format 'gzip))
        bstr]
       [(and compress? (eq? compress-format 'lz4))
        (lz4 "-z" (reencode bstr gunzip-through-ports))]
       [else
        (reencode bstr gunzip-through-ports)])]
    [else (error 'adjust-compress "unrecognized format ~s" (subbytes bstr 0 3))]))
       
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
