#lang racket/base
(require racket/serialize)
(provide (all-defined-out))

;; SQL Data
;; Datatypes for things that have no appropriate corresponding Scheme datatype

;; ----------------------------------------

;; NULL

(define sql-null
  (let ()
    (struct sql-null ()
            ;; must deserialize to singleton, so can't just use serializable-struct
            #:property prop:serializable
            (make-serialize-info (lambda _ '#())
                                 #'deserialize-info:sql-null-v0
                                 #f
                                 (or (current-load-relative-directory)
                                     (current-directory))))
    (sql-null)))

(define (sql-null? x)
  (eq? x sql-null))

(define (sql-null->false x)
  (if (eq? x sql-null)
      #f
      x))

(define (false->sql-null x)
  (if (eq? x #f)
      sql-null
      x))

(define deserialize-info:sql-null-v0
  (make-deserialize-info
   (lambda _ sql-null)
   (lambda () (error 'deserialize-sql-null "cannot have cycles"))))

;; ----------------------------------------

;; Dates and times

#|
** problems with Racket date:

 - fields in wrong order
 - too many fields (not checked for correctness?)
 - timezone, dst? field too limited (?)
 - no fractional seconds

** problems with SRFI date:

 - fields in wrong order
 - timezone offset too limited
|#

(define-serializable-struct sql-date (year month day) #:transparent)
(define-serializable-struct sql-time (hour minute second nanosecond tz) #:transparent)
(define-serializable-struct sql-timestamp
  (year month day hour minute second nanosecond tz) 
  #:transparent)

;; Intervals must be "pre-multiplied" rather than carry extra sign field.
;; Rationale: postgresql, at least, allows mixture of signs, eg "1 month - 30 days"
(define-serializable-struct sql-interval
  (years months days hours minutes seconds nanoseconds)
  #:transparent
  #:guard (lambda (years months days hours minutes seconds nanoseconds _name)
            ;; Normalize years/months, days/hours/minutes/seconds/nanoseconds
            ;; Recall: quotient, remainder results have sign of first arg
            ;;   (if second arg is positive)
            (let ([total-months (+ months (* years 12))]
                  ;; FIXME: avoid overflow to bignums
                  [total-nsecs (+ nanoseconds
                                  (* (+ seconds
                                        (* minutes 60)
                                        (* hours 60 60)
                                        (* days 60 60 24))
                                     #e1e9))])
              (let*-values ([(years months) (quotient/remainder total-months 12)]
                            [(left-secs nsecs) (quotient/remainder total-nsecs #e1e9)]
                            [(left-mins secs) (quotient/remainder left-secs 60)]
                            [(left-hours mins) (quotient/remainder left-mins 60)]
                            [(days hours) (quotient/remainder left-hours 24)])
                (values years months days hours mins secs nsecs)))))

;; ----

(define (sql-day-time-interval? x)
  (and (sql-interval? x)
       (zero? (sql-interval-years x))
       (zero? (sql-interval-months x))))

(define (sql-year-month-interval? x)
  (and (sql-interval? x)
       (zero? (sql-interval-days x))
       (zero? (sql-interval-hours x))
       (zero? (sql-interval-minutes x))
       (zero? (sql-interval-seconds x))
       (zero? (sql-interval-nanoseconds x))))

(define no-arg (gensym))

(define (sql-interval->sql-time x [default no-arg])
  (let ([d (sql-interval-days x)]
        [h (sql-interval-hours x)]
        [m (sql-interval-minutes x)]
        [s (sql-interval-seconds x)]
        [ns (sql-interval-nanoseconds x)])
    (cond [(and (sql-day-time-interval? x)
                (zero? d)
                (<= 0 h 23)
                (<= 0 m 59)
                (<= 0 s 59)
                (<= 0 ns (sub1 #e1e9)))
           (sql-time h m s ns #f)]
          [else
           (cond [(eq? default no-arg)
                  (error 'sql-day-time-interval->sql-time
                         "cannot convert interval to time: ~e" x)]
                 [(procedure? default) (default)]
                 [else default])])))

(define (sql-time->sql-interval x)
  (sql-interval 0 0 0
                (sql-time-hour x)
                (sql-time-minute x)
                (sql-time-second x)
                (sql-time-nanosecond x)))

;; ----------------------------------------

;; Bits

#|
A sql-bits is (sql-bits len bv offset)
where len is the number of bits, and bv is a bytes, offset is nat.

Bit order is little-endian wrt bytes, but big-endian wrt bits within a
byte. (Because that's PostgreSQL's binary format.) For example:

  (bytes 128 3) represents 1000000 0000011
|#
(serializable-struct sql-bits (length bv offset))

(define (make-sql-bits len)
  (sql-bits len (make-bytes (/ceiling len 8) 0) 0))

(define (make-sql-bits/bytes len bv offset)
  (sql-bits len bv offset))

(define (check-index fsym b index)
  (let ([len (sql-bits-length b)])
    (unless (< index len)
      (if (zero? len)
          (error fsym "index ~e out of range for empty sql-bits" index)
          (error fsym "index ~e out of range: [0, ~a]" index (+ len -1))))))

(define (sql-bits-ref b i)
  (check-index 'sql-bits-ref b i)
  (bv-ref (sql-bits-bv b) (+ i (sql-bits-offset b))))
(define (bv-ref bv i)
  (let-values ([(bytei biti) (quotient/remainder i 8)])
    (bitwise-bit-set? (bytes-ref bv bytei) (- 7 biti))))

(define (sql-bits-set! b i v)
  (check-index 'sql-bits-set! b i)
  (bv-set! (sql-bits-bv b) (+ i (sql-bits-offset b)) v))
(define (bv-set! bv i v)
  (let-values ([(bytei biti) (quotient/remainder i 8)])
    (let* ([oldbyte (bytes-ref bv bytei)]
           [mask (arithmetic-shift 1 (- 7 biti))]
           [newbyte (bitwise-ior (bitwise-and oldbyte (bitwise-xor 255 mask)) (if v mask 0))])
      (unless (= oldbyte newbyte)
        (bytes-set! bv bytei newbyte)))))

(define (sql-bits->list b)
  (let ([l (sql-bits-length b)]
        [bv (sql-bits-bv b)]
        [offset (sql-bits-offset b)])
    (for/list ([i (in-range l)])
      (bv-ref bv (+ offset i)))))

(define (sql-bits->string b)
  (let* ([l (sql-bits-length b)]
         [bv (sql-bits-bv b)]
         [offset (sql-bits-offset b)]
         [s (make-string l)])
    (for ([i (in-range l)])
      (string-set! s i (if (bv-ref bv (+ offset i)) #\1 #\0)))
    s))

(define (list->sql-bits lst)
  (let* ([b (make-sql-bits (length lst))]
         [bv (sql-bits-bv b)])
    (for ([v (in-list lst)]
          [i (in-naturals)])
      (bv-set! bv i v))
    b))

(define (string->sql-bits s)
  (let* ([b (make-sql-bits (string-length s))]
         [bv (sql-bits-bv b)])
    (for ([i (in-range (string-length s))])
      (case (string-ref s i)
        ((#\0) (bv-set! bv i #f))
        ((#\1) (bv-set! bv i #t))
        (else (raise-type-error 'string->sql-bits "string over {0,1}" 0 s))))
    b))

(define (/ceiling x y)
  (let-values ([(q r) (quotient/remainder x y)])
    (+ q (if (zero? r) 0 1))))

(define (align-sql-bits b dir)
  (let* ([len (sql-bits-length b)]
         [bv (sql-bits-bv b)]
         [offset (sql-bits-offset b)]
         [offset* (case dir
                    ((left) 0)
                    ((right) (- 8 (remainder len 8))))])
    (cond [(= (remainder offset 8) offset*)
           (values len bv (quotient offset 8))]
          [else
           (let ([b* (copy-sql-bits b offset*)])
             (values len (sql-bits-bv b*) 0))])))

(define (copy-sql-bits b [offset* 0])
  (let* ([len (sql-bits-length b)]
         [bv0 (sql-bits-bv b)]
         [offset0 (sql-bits-offset b)]
         [bytelen* (/ceiling (+ len offset*) 8)]
         [bv* (make-bytes bytelen* 0)])
    (for ([i (in-range len)])
      (bv-set! bv* (+ i offset*) (bv-ref bv0 (+ offset0 i))))
    (sql-bits len bv* offset*)))

;; ----------------------------------------

;; Predicates

(define (mkintN? n)
  (let ([hi  (- (expt 2 (sub1 n)) 1)]
        [low (- (expt 2 (sub1 n)))])
    (lambda (x) (and (exact-integer? x) (<= low x hi)))))

(define (mkuintN? n)
  (let ([hi (- (expt 2 n) 1)])
    (lambda (x) (and (exact-integer? x) (<= 0 x hi)))))

(define int8?  (mkintN? 8))
(define int16? (mkintN? 16))
(define int24? (mkintN? 24))
(define int32? (mkintN? 32))
(define int64? (mkintN? 64))

(define uint8?  (mkuintN? 8))
(define uint16? (mkuintN? 16))
(define uint24? (mkuintN? 24))
(define uint32? (mkuintN? 32))
(define uint64? (mkuintN? 64))

(provide int8? int16? int24? int32? int64?
         uint8?)
