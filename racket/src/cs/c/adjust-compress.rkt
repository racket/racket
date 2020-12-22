#lang racket/base
(require racket/file
         racket/system
         file/gzip
         file/gunzip
         ffi/unsafe)

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

(define (lz4-d result-size bstr)
  (define lz4 (ffi-lib "liblz4"))
  (define LZ4_decompress_safe (get-ffi-obj 'LZ4_decompress_safe lz4 (_fun _pointer _pointer _int _int -> _int)))
  (define r-bstr (make-bytes result-size))
  (when (negative? (LZ4_decompress_safe bstr r-bstr (bytes-length bstr) (bytes-length r-bstr)))
    (error 'lz4 "decompression failed"))
  r-bstr)

(define (lz4-z bstr)
  (define lz4 (ffi-lib "liblz4"))
  (define LZ4_compressBound (get-ffi-obj 'LZ4_compressBound lz4 (_fun _int -> _int)))
  (define LZ4_compress_default (get-ffi-obj 'LZ4_compress_default lz4 (_fun _pointer _pointer _int _int -> _int)))
  (define max-len (LZ4_compressBound (bytes-length bstr)))
  (define r-bstr (make-bytes max-len))
  (define len (LZ4_compress_default bstr r-bstr (bytes-length bstr) (bytes-length r-bstr)))
  (subbytes r-bstr 0 len))

(define (adler32 bstr)
  (define BASE 65521)
  (define-values (s1 s2)
    (for/fold ([prev-s1 1] [s2 0]) ([b (in-bytes bstr)])
      (define s1 (modulo (+ prev-s1 b) BASE))
      (values s1 (modulo (+ s2 s1) BASE))))
  (integer->integer-bytes (+ s1 (arithmetic-shift s2 16)) 4 #f #t))

(define fasl-type-uncompressed 43)
(define fasl-type-gzip         44)
(define fasl-type-lz4          45)

(define (read-byte/not-eof in)
  (define b (read-byte in))
  (when (eof-object? b)
    (error "unexpected eof"))
  b)

(define (read-uptr in)
  (let ([k (read-byte/not-eof in)])
    (let loop ([k k] [n 0])
      (let ([n (bitwise-ior n (bitwise-and k #x7F))])
        (if (zero? (bitwise-and k #x80))
            n
            (loop (read-byte/not-eof in) (arithmetic-shift n 7)))))))

(define (write-uptr n out)
  (let loop ([n n] [cbit 0])
    (unless (n . <= . #x7F)
      (loop (arithmetic-shift n -7) #x80))
    (write-byte (bitwise-ior (bitwise-and n #x7F) cbit) out)))

(define (uptr-bytes n)
  (if (n . <= . #x7F)
      1
      (add1 (uptr-bytes (arithmetic-shift n -7)))))

(define (adjust-compress bstr)
  (define in (open-input-bytes bstr))
  (define out (open-output-bytes bstr))
  (let loop ([saw-header? #f])
    (cond
      [(regexp-try-match #"^\0\0\0\0chez" in)
       ;; copy header
       (define vers (read-uptr in))
       (define mach (read-uptr in))
       (define s (regexp-try-match	#rx"^[(][^)]*[)]" in))
       (unless s
         (error 'adjust-compress "did not find (...)" (file-position in)))
       (write-bytes #"\0\0\0\0chez" out)
       (write-uptr vers out)
       (write-uptr mach out)
       (write-bytes (car s) out)
       (loop #t)]
      [(not saw-header?)
       (error 'adjust-compress "did not find leading header")]
      [else
       (let ([situation (read-byte in)])
         (unless (eof-object? situation)
           (let ([size (- (read-uptr in) 2)] ; size in fasl includes compression + kind
                 [compression (read-byte/not-eof in)]
                 [kind (read-byte/not-eof in)])
             (define-values (bstr compressed dest-size)
               (cond
                 [(eqv? compression fasl-type-uncompressed)
                  ;; source is not compressed
                  (values (read-bytes size in) #f #f)]
                 [else
                  (define dest-size (read-uptr in))
                  (define c-bstr (read-bytes (- size (uptr-bytes dest-size)) in))
                  (cond
                    [(eqv? compression fasl-type-gzip)
                     (if (and compress? (eq? compress-format 'gzip))
                         (values c-bstr 'gzip dest-size)
                         (let ([c-bstr (subbytes c-bstr 2 (- (bytes-length c-bstr 4)))])
                           (values (reencode c-bstr inflate) #f #f)))]
                    [(eqv? compression fasl-type-lz4)
                     (if (and compress? (eq? compress-format 'lz4))
                         (values c-bstr 'lz4 dest-size)
                         (values (lz4-d dest-size c-bstr) #f #f))]
                    [else
                     (error 'adjust-compress "unrecognized compression ~s" compression)])]))
             (cond
               [(or (not compress?)
                    (and (not compressed)
                         ((bytes-length bstr) . < . 100)))
                (write-byte situation out)
                (write-uptr (+ (bytes-length bstr) 2) out)
                (write-byte fasl-type-uncompressed out)
                (write-byte kind out)
                (write-bytes bstr out)]
               [else
                (define d-size (or dest-size
                                    (bytes-length bstr)))
                (define c-bstr
                  (if compressed
                      bstr
                      (cond
                        [(eq? compress-format 'gzip)
                         (bytes-append #"\x78\x5e"
                                       (reencode bstr (lambda (i o) (deflate i o)))
                                       (adler32 bstr))]
                        [(eq? compress-format 'lz4)
                         (lz4-z bstr)]
                        [else
                         (error 'adjust-compress "unsupported ~s" compress-format)])))
                (write-byte situation out)
                (write-uptr (+ (bytes-length c-bstr) (uptr-bytes d-size) 2) out)
                (write-byte (cond
                              [(eq? compress-format 'gzip) fasl-type-gzip]
                              [(eq? compress-format 'lz4) fasl-type-lz4]
                              [else (error 'adjust-compress "unsupported ~s" compress-format)])
                            out)
                (write-byte kind out)
                (write-uptr d-size out)
                (write-bytes c-bstr out)]))
           (loop #t)))]))
  (get-output-bytes out))
       
(module+ main
  (require racket/cmdline)
      
  (command-line
   #:once-any
   [("--uncompressed") "Uncompress compiled code"
                       (void)]
   [("--gzip") "Compress using gzip"
               (enable-compress!)
               (set-compress-format! 'gzip)]
   [("--lz4") "Compress using lz4"
              (enable-compress!)
              (set-compress-format! 'lz4)]
   #:args path
   (for ([path (in-list path)])
     (define bstr (file->bytes path))
     (define bstr2 (adjust-compress bstr))
     (unless (equal? bstr bstr2)
       (call-with-output-file*
        path
        #:exists 'truncate/replace
        (lambda (o)
          (write-bytes bstr2 o)))))))
