#lang racket/base
(require racket/serialize
         data/bit-vector)
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

(module+ deserialize-info
  (provide deserialize-info:sql-null-v0))

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
                         "cannot convert given interval to time: ~e" x)]
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

;; A sql-bits is now just a bit-vector (see data/bit-vector).

(define (sql-bits? v)
  (bit-vector? v))
(define (sql-bits-length b)
  (bit-vector-length b))
(define (make-sql-bits len)
  (make-bit-vector len #f))
(define (sql-bits-ref b i)
  (bit-vector-ref b i))
(define (sql-bits-set! b i v)
  (bit-vector-set! b i v))
(define (sql-bits->list b)
  (bit-vector->list b))
(define (list->sql-bits l)
  (list->bit-vector l))
(define (sql-bits->string b)
  (bit-vector->string b))
(define (string->sql-bits s)
  (string->bit-vector s))

;; ----

#|
Formerly, a sql-bits was (sql-bits len bv offset)
where len is the number of bits, and bv is a bytes, offset is nat.

Bit order is little-endian wrt bytes, but big-endian wrt bits within a
byte. (Because that's PostgreSQL's binary format.) For example:

  (bytes 128 3) represents 1000000 0000011
|#
;;(serializable-struct sql-bits (length bv offset))

(define (make-sql-bits/bytes len bs offset)
  (define bv (make-bit-vector len #f))
  (for ([i (in-range len)])
    (let ([n (+ i offset)])
      (let-values ([(bytei biti) (quotient/remainder n 8)])
        (when (bitwise-bit-set? (bytes-ref bs bytei) (- 7 biti))
          (bit-vector-set! bv i #t)))))
  bv)

(define deserialize-info:sql-bits-v0
  (make-deserialize-info
   make-sql-bits/bytes
   (lambda () (error 'deserialize-info:sql-bits-v0 "cycles not allowed"))))

(module+ deserialize-info
  (provide deserialize-info:sql-bits-v0))

;; align-sql-bits : bit-vector 'left/'right -> (values nat bytes 0)
;; Formats a bit-vector in postgresql ('left) or mysql ('right) binary format.
;; Returns number of bits, byte buffer, and starting point of data in buffer
;; (as byte index, now always 0).
(define (align-sql-bits bv dir)
  (let* ([len (bit-vector-length bv)]
         [offset (case dir
                   ((left) 0)
                   ((right) (- 8 (remainder len 8))))]
         [bs (make-bytes (/ceiling len 8) 0)])
    (for ([bvi (in-range len)]
          [bsi (in-naturals offset)])
      (when (bit-vector-ref bv bvi)
        (let-values ([(bytei biti) (quotient/remainder bsi 8)])
          (bytes-set! bs bytei (+ (bytes-ref bs bytei) (arithmetic-shift 1 (- 7 biti)))))))
    (values len bs 0)))

(define (/ceiling x y)
  (let-values ([(q r) (quotient/remainder x y)])
    (+ q (if (zero? r) 0 1))))

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
