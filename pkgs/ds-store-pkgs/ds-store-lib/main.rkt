#lang racket/base

(provide read-ds-store
         write-ds-store
         (struct-out ds)
         (struct-out iloc)
         (struct-out fwind))

;; Based on 
;;   http://search.cpan.org/~wiml/Mac-Finder-DSStore/DSStoreFormat.pod
;; by Wim Lewis and Mark Mentovai

(struct ds (path id type data) #:transparent)
(struct iloc (x y) #:transparent)
(struct fwind (t l b r mode sideview?) #:transparent)

(define HEAD 4)

(define (swap-bytes bstr)
  (let ([s (make-bytes (bytes-length bstr))])
    (for ([i (in-range 0 (bytes-length bstr) 2)])
      (bytes-set! s i (bytes-ref bstr (add1 i)))
      (bytes-set! s (add1 i) (bytes-ref bstr i)))
    s))

(define (bytes->string/utf-16 bstr)
  (define bstr2 (if (system-big-endian?)
                    bstr
                    (swap-bytes bstr)))
  (define c (bytes-open-converter "platform-UTF-16" "platform-UTF-8"))
  (define-values (utf-8 got status) (bytes-convert c bstr2))
  (bytes-close-converter c)
  (bytes->string/utf-8 utf-8))

(define (string->bytes/utf-16 s)
  (define c (bytes-open-converter "platform-UTF-8" "platform-UTF-16"))
  (define-values (utf-16 got status) (bytes-convert c (string->bytes/utf-8 s)))
  (bytes-close-converter c)
  (if (system-big-endian?)
      utf-16
      (swap-bytes utf-16)))

(define (mac-path<? a b)
  ;; A guess: alphabetic case-insensitively, but ties
  ;; can be determined by case:
  (define as (bytes->string/utf-16 a))
  (define bs (bytes->string/utf-16 b))
  (or (string-ci<? as bs)
      (and (string-ci=? as bs)
           (string<? as bs))))

(define (read-ds-store path
                       #:verbose? [verbose? #f])
  (call-with-input-file*
   path
   (lambda (p)
     (define (check b)
       (unless (equal? b (read-bytes (bytes-length b) p))
         (error "mismatch")))

     (define (read-int)
       (integer-bytes->integer (read-bytes 4 p) #f #t))
     (define (read-short)
       (integer-bytes->integer (read-bytes 2 p) #f #t))

     (define (read-utf-16 len)
       (define bstr (read-bytes (* 2 len) p))
       (bytes->string/utf-16 bstr))

     (define (read-four)
       (string->symbol (bytes->string/utf-8 (read-bytes 4 p) #\?)))

     (define (addr-offset o) (- o (bitwise-and o #x1F)))
     (define (addr-size o) (expt 2 (bitwise-and o #x1F)))

     (check #"\0\0\0\1")
     (check #"Bud1")

     (define bookkeeping-offset (read-int))
     (define bookkeeping-size (read-int))

     (when verbose?
       (printf "Bookkeeping at ~a, size ~a\n"
               bookkeeping-offset
               bookkeeping-size))

     (file-position p (+ bookkeeping-offset HEAD))
     (define count (read-int))
     (when verbose?
       (printf "File has ~a blocks\n" count))
     (void (read-int))
     (define block-addresses
       (for/vector ([i (in-range count)])
         (read-int)))
     (when verbose?
       (printf "Block addresses:\n")
       (for ([a (in-vector block-addresses)]
             [i (in-naturals)])
         (printf " ~a: ~a = ~a\n" 
                 i
                 (addr-offset a)
                 (addr-size a))))
     (void (read-bytes (* 4 (- 256 count)) p))
     (define directory-count (read-int))
     (define dirs (for/list ([i (in-range directory-count)])
                    (cons (read-bytes (read-byte p) p)
                          (read-int))))
     (define free-lists (for/list ([i 32])
                          (define c (read-int))
                          (for/list ([i c])
                            (read-int))))
     (when verbose?
       (printf "Free list:\n")
       (for/list ([i 32]
                  [l (in-list free-lists)])
         (printf "~a: ~a\n" (expt 2 i) l)))

     (define header-block (cdr (assoc #"DSDB" dirs)))

     (define header-addr (vector-ref block-addresses header-block))

     (when verbose?
       (printf "Header block is ~a at ~a (size ~a)\n" 
               header-block 
               (addr-offset header-addr)
               (addr-size header-addr)))

     (file-position p (+ (addr-offset header-addr) HEAD))

     (define root-block-number (read-int)) ; root node
     (define more-root-data
       (list
        (read-int)   ; levels
        (read-int)   ; records
        (read-int))) ; nodes
     (unless (equal? (read-int) #x1000)
       (error "mismatch"))

     (when verbose?
       (printf "Root block is ~a ~s\n" root-block-number more-root-data))

     (define (show-tree n accum)
       (define addr (vector-ref block-addresses n))
       (file-position p (+ (addr-offset addr) HEAD))
       (define P (read-int))
       (define count (read-int))
       (when verbose?
         (printf "block ~s ~s\n" P count))
       (cond
        [(zero? P)
         (for/fold ([accum accum]) ([i (in-range count)])
           (show-record accum))]
        [else
         (define a3
           (for/fold ([accum accum]) ([i (in-range count)])
             (define bn (read-int))
             (define pos (file-position p))
             (define a2 (show-tree bn accum))
             (file-position p pos)
             (show-record a2)))
         (show-tree P a3)]))

     (define (show-record accum)
       (define len (read-int))
       (define name (read-utf-16 len))
       (define id (read-four))
       (define type (read-four))
       (define data
         (case type
           [(long shor) (read-int)]
           [(bool) (positive? (read-byte p))]
           [(blob) 
            (define len (read-int))
            (case id
              [(fwi0) (begin0
                       (fwind (read-short)
                              (read-short)
                              (read-short)
                              (read-short)
                              (read-four)
                              (begin
                                (read-byte p)
                                (not (zero? (read-byte p)))))
                       (read-bytes (- len 14) p))]
              [(Iloc) (begin0
                       (iloc (read-int)
                             (read-int))
                       (read-bytes (- len 8) p))]
              [else (read-bytes len p)])]
           [(type) (read-four)]
           [(ustr) (read-utf-16 (read-int))]))
       (when verbose?
         (printf "~a '~a' '~a':\n  ~s\n" name id type data))
       (cons (ds (if (equal? name ".")
                     'same
                     (string->path-element name))
                 id
                 type
                 data)
             accum))

     (reverse (show-tree root-block-number null)))))

(define (write-ds-store path dses)
  (struct record (filename id type data))

  (define (record<? a b)
    (define af (record-filename a))
    (define bf (record-filename b))
    (if (equal? af bf)
        (mac-path<? (record-id a) (record-id b))
        (mac-path<? af bf)))

  (define (record-size r)
    (+ 4 ; filename length
       (bytes-length (record-filename r))
       4 ; id
       4 ; type
       (bytes-length (record-data r))))

  (define (int->bytes i)
    (integer->integer-bytes i 4 #t #t))
  (define (short->bytes i)
    (integer->integer-bytes i 2 #t #t))
  
  (define records
    (sort (for/list ([ds (in-list dses)])
            (define data (ds-data ds))
            (record (if (eq? (ds-path ds) 'same)
                        (string->bytes/utf-16 ".")
                        (string->bytes/utf-16
                         (path-element->string (ds-path ds))))
                    (string->bytes/utf-8 (symbol->string (ds-id ds)))
                    (string->bytes/utf-8 (symbol->string (ds-type ds)))
                    (case (ds-type ds)
                      [(long shor) (int->bytes data)]
                      [(bool) (if data #"\1" #"\0")]
                      [(blob)
                       (define bstr
                         (cond
                          [(bytes? data) data]
                          [(fwind? data) (bytes-append 
                                          (short->bytes (fwind-t data))
                                          (short->bytes (fwind-l data))
                                          (short->bytes (fwind-b data))
                                          (short->bytes (fwind-r data))
                                          (string->bytes/utf-8 (symbol->string (fwind-mode data)))
                                          (bytes 0
                                                 (if (fwind-sideview? data) 1 0)
                                                 0 
                                                 0))]
                          [(iloc? data) (bytes-append
                                         (int->bytes (iloc-x data))
                                         (int->bytes (iloc-y data))
                                         (bytes #xff #xff #xff #xff #xff #xff 0 0))]
                          [else (error "unrecognized block variant: ~s" data)]))
                       (bytes-append
                        (int->bytes (bytes-length bstr))
                        bstr)]
                      [(type)
                       (string->bytes/utf-8 (symbol->string data))]
                      [(ustr)
                       (define bstr (string->bytes/utf-16 data))
                       (string-append (int->bytes (quotient (bytes-length bstr) 2))
                                      bstr)]
                      [else (error "unrecognized data: ~s" (ds-type ds))])))
          record<?))

  (define records-block-size
    (+ 4 ; P = 0
       4 ; count 
       (apply + (for/list ([r (in-list records)])
                  (record-size r)))))

  (define alloc-size (let ([v (max 64
                                   (expt 2 (integer-length (add1 records-block-size))))])
                       (if (= v 2048)
                           4096 ; avoid collision with bookkeeping block
                           v)))

  (call-with-output-file*
   path
   #:exists 'truncate/replace
   (lambda (p)
     (define (write-records sz)
       (write-int 0)
       (write-int (length records))
       (for ([r (in-list records)])
         (write-int (quotient (bytes-length (record-filename r)) 2))
         (write-bytes (record-filename r) p)
         (write-bytes (record-id r) p)
         (write-bytes (record-type r) p)
         (write-bytes (record-data r) p))
       (write-bytes (make-bytes (- sz records-block-size) 0) p))

     (define (write-int i)
       (write-bytes (int->bytes i) p))
     (define (write-addr pos sz)
       (write-int (bitwise-ior pos (sub1 (integer-length sz)))))

     (write-bytes #"\0\0\0\1" p)

     (write-bytes #"Bud1" p)     
     ;; Bookeeping block always at 2048, since
     ;; it needs 2048 bytes:
     (write-int 2048) ; offset
     (write-int 2048) ; size
     (write-int 2048) ; offset, again
     (write-bytes (make-bytes 16 0) p)

     ;; Next 32-byte block (at offset 32) is used for the header block:
     (write-int 2) ; block number for root
     (write-int 0) ; level
     (write-int (length records)) ; records
     (write-int 1) ; nodes
     (write-int #x1000)
     (write-bytes (make-bytes 12 0) p)

     ;; Starting with 64, need blocks up to size
     ;; 1024. If any of those fit the records, then
     ;; use it.
     (let loop ([sz 64])
       (if (= sz alloc-size)
           (write-records sz)
           (write-bytes (make-bytes sz 0) p))
       (unless (= sz 1024)
         (loop (* sz 2))))

     ;; Write bookkeeping block
     (write-int 3)     ; 3 blocks
     (write-int 0)     ; unknown
     (write-addr 2048 2048) ; bookkeeping
     (write-addr 32 32) ; header block
     ;; records block always lands at second buddy:
     (write-addr alloc-size alloc-size)
     (write-bytes (make-bytes (* 4 (- 256 3)) 0) p)
     
     ;; Single directory entry:
     (write-int 1)
     (write-byte 4 p)
     (write-bytes #"DSDB" p)
     (write-int 1) ; block 1 is header

     ;; free lists:
     (for/list ([i 32])
       (define sz (expt 2 i))
       (cond
        [(= i 31) (write-int 0)]
        [(or (sz . <= . 32) 
             (sz . = . 2048))
         (write-int 0)]
        [(= sz alloc-size) (write-int 0)]
        [else (write-int 1)
              (write-int sz)]))

     (file-truncate p (+ 4096 HEAD))
     (file-position p (+ 4096 HEAD))

     ;; write bytes as needed to reach records data
     (let loop ([sz 4096])
       (when (alloc-size . > . sz)
         (write-bytes (make-bytes sz 0) p)
         (loop (* 2 sz))))

     (when (alloc-size . > . 2048)
       (write-records alloc-size))

     (void))))
