#lang racket/base
(require racket/class
         racket/list
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt"
         "../../util/geometry.rkt"
         "../../util/postgresql.rkt"
         (only-in "message.rkt" field-dvec->typeid))
(provide dbsystem
         typeid->type-reader
         typeid->format)

(define postgresql-dbsystem%
  (class* object% (dbsystem<%>)

    (define/public (get-short-name) 'postgresql)
    (define/public (get-known-types) supported-types)

    (define/public (has-support? option)
      (case option
        ((real-infinities) #t)
        ((numeric-infinities) #t)
        (else #f)))

    (define/public (get-parameter-handlers param-typeids)
      (map (lambda (param-typeid)
             (typeid->type-writer param-typeid))
           param-typeids))

    (define/public (field-dvecs->typeids dvecs)
      (map field-dvec->typeid dvecs))

    (define/public (describe-typeids typeids)
      (map describe-typeid typeids))

    (super-new)))

(define dbsystem
  (new postgresql-dbsystem%))

;; ========================================

;; Derived from 
;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html

(define-type-table (supported-types
                    type-alias->type
                    typeid->type
                    type->typeid
                    describe-typeid)
  (16   boolean    (bool)    #t)
  (17   bytea      ()        #t)
  (18   char1      ()        #t)
  (19   name       ()        #t)
  (20   bigint     (int8)    #t)
  (21   smallint   (int2)    #t)
  (23   integer    (int4)    #t)
  (25   text       ()        #t)
  (26   oid        ()        #t)
  (700  real       (float4)  #t)
  (701  double     (float8)  #t)
  (1042 character  (bpchar)  #t)
  (1043 varchar    ()        #t)
  (1082 date       ()        #t)
  (1083 time       ()        #t)
  (1114 timestamp  ()        #t)
  (1184 timestamptz()        #t)
  (1186 interval   ()        #t)
  (1266 timetz     ()        #t)
  (1700 decimal    (numeric) #t)

  (1560 bit      () #t)
  (1562 varbit   () #t)

  (600 point     () #t)
  (601 lseg      () #t)
  (602 path      () #t)
  (603 box       () #t)
  (604 polygon   () #t)
  (718 circle    () #t)

  ;; "string" literals have type unknown; just treat as string
  (705 unknown     ()        #t)

  ;; The following types are not supported.
  ;; (But putting their names here yields better not-supported errors.)

  (2249 record   () #t)

  (628 line      () #f)
  (142 xml       () #f)
  (702 abstime   () #f)
  (703 reltime   () #f)
  (704 tinterval () #f)
  (790 money     () #f)
  (829 macaddr   () #f)
  (869 inet      () #f)
  (650 cidr      () #f))

;; ============================================================

#|
BINARY VS TEXT FORMAT

For most types, we send and receive data in binary format
only. However, datetime types are tricky enough that binary format
isn't worth it (yet).

Domain typeids never seem to appear as result typeids, but do appear
as parameter typeids.

----

bit, varbit = len:int4 byte* (0-padded on *left*)

date = int4 (days since 2000-01-01)
timestamp = (int8 or float8)
timestamptz = (int8 or float8)
time = (int8 or float8)
timetz = (int8 or float8) zone-secs:int4
interval = (usecs:int8 or secs:float8) days:int4 months:int4

  (time*, timestamp* depend on "SHOW integer_datetimes")

inet, cidr = family:byte bits:byte is_cidr:byte addrlen:byte addr:be-integer
  is_cidr is ignored

record = cols:int4 (typeoid:int4 len/-1:int4 data:byte^len)^cols 

|#

;; Binary readers

(define (recv-bits x)
  (let* ([len (integer-bytes->integer x #t #t 0 4)])
    (make-sql-bits/bytes len (subbytes x 4) 0)))

(define (recv-boolean x)
  (case (bytes-ref x 0)
    ((0) #f)
    ((1) #t)
    (else (error/internal 'recv-boolean "bad value: ~e" x))))

(define (recv-char1 x)
  (integer->char (bytes-ref x 0)))

(define (recv-bytea x)
  x)

(define (recv-string x)
  (bytes->string/utf-8 x))

(define (recv-integer x)
  (integer-bytes->integer x #t #t))

(define (recv-float x)
  (floating-point-bytes->real x #t))

(define (get-double bs offset)
  (floating-point-bytes->real bs #t offset (+ 8 offset)))
(define (recv-point x [offset 0])
  (point (get-double x (+ offset 0)) (get-double x (+ offset 8))))
(define (recv-box x)
  (pg-box (recv-point x 0) (recv-point x 16)))
(define (recv-circle x)
  (pg-circle (recv-point x 0) (get-double x 16)))
(define (recv-lseg x)
  (line-string (list (recv-point x 0) (recv-point x 16))))
(define (recv-path x)
  (pg-path (not (zero? (bytes-ref x 0)))
           (for/list ([i (integer-bytes->integer x #t #t 1 5)])
             (recv-point x (+ 5 (* 16 i))))))
(define (recv-polygon x)
  (let* ([points0
          (for/list ([i (in-range (integer-bytes->integer x #t #t 0 4))])
            (recv-point x (+ 4 (* 16 i))))]
         [points (append points0 (list (car points0)))])
    (polygon (line-string points)
             null)))

(define (recv-record x)
  (let ([start 0])
    (define (get-int signed?)
      (begin0 (integer-bytes->integer x signed? #t start (+ start 4))
        (set! start (+ start 4))))
    (define (get-bytes len)
      (begin0 (subbytes x start (+ start len))
        (set! start (+ start len))))
    (define (recv-col)
      (let* ([typeid (get-int #t)]
             [len (get-int #t)])
        (if (= len -1)
            sql-null
            (let* ([bin? (= (typeid->format typeid) 1)] ;; binary reader available
                   [reader (and bin? (typeid->type-reader 'recv-record typeid))])
              (if reader
                  (reader (get-bytes len))
                  'unreadable)))))
    (let ([columns (get-int #t)])
      (build-vector columns (lambda (i) (recv-col))))))

#|
(define (recv-numeric x)
  (define (get-int2 start) (integer-bytes->integer x #t #t start (+ 2 start)))
  (let* ([NBASE  #e1e4]
         [NUMERIC_POS #x0000]
         [NUMERIC_NEG #x4000]
         [NUMERIC_NAN #xC000]
         [digits (get-int2 0)]
         [weight (get-int2 2)]
         [sign   (get-int2 4)]
         [dscale (get-int2 6)]
         [unscaled-digits
          (for/list ([offset (in-range 8 (+ 8 (* 2 digits)) 2)])
            (get-int2 offset))]
         [scaled-digits
          (for/list ([unscaled-digit (in-list unscaled-digits)]
                     [i (in-naturals)])
            (* unscaled-digit (expt NBASE (- weight i))))]
         [abs-number (apply + scaled-digits)])
    (cond [(= sign NUMERIC_POS)
           abs-number]
          [(= sign NUMERIC_NEG)
           (- abs-number)]
          [(= sign NUMERIC_NAN)
           +nan.0])))
|#

(define-values (c-parse-char1
                c-parse-date
                c-parse-time
                c-parse-time-tz
                c-parse-timestamp
                c-parse-timestamp-tz
                c-parse-interval
                c-parse-decimal)
  (let ([c (lambda (f) (lambda (x) (f (bytes->string/utf-8 x))))])
    (values (c parse-char1)
            (c parse-date)
            (c parse-time)
            (c parse-time-tz)
            (c parse-timestamp)
            (c parse-timestamp-tz)
            (c parse-interval)
            (c parse-decimal))))

;; Binary writers

(define (send-boolean f i x)
  (case x
    ((#t) (bytes 1))
    ((#f) (bytes 0))
    (else (send-error f i "boolean" x))))

(define (send-bits f i x)
  (unless (sql-bits? x) (send-error f i "bits" x))
  (let-values ([(len bv start) (align-sql-bits x 'left)])
    (bytes-append (integer->integer-bytes len 4 #t #t)
                  (if (zero? start) bv (subbytes bv start)))))

(define (send-char1 f i x)
  (let ([n (if (char? x) (char->integer x) x)])
    (unless (uint8? n) (send-error f i "char1" x))
    (bytes n)))

(define (send-bytea f i x)
  (unless (bytes? x) (send-error f i "bytea" x))
  x)

(define (send-string f i x)
  (unless (string? x) (send-error f i "string" x))
  (string->bytes/utf-8 x))

(define (send-int2 f i n)
  (unless (int16? n) (send-error f i "int2" n))
  (integer->integer-bytes n 2 #t #t))

(define (send-int4 f i n)
  (unless (int32? n) (send-error f i "int4" n))
  (integer->integer-bytes n 4 #t #t))

(define (send-int8 f i n)
  (unless (int64? n) (send-error f i "int8" n))
  (integer->integer-bytes n 8 #t #t))

(define (send-float* f i n type size)
  (unless (real? n) (send-error f i type n))
  (real->floating-point-bytes n size #t))

(define (send-float4 f i n)
  (send-float* f i n "float4" 4))

(define (send-float8 f i n)
  (send-float* f i n "float8" 8))

(define (float8 x)
  (real->floating-point-bytes x 8 #t))
(define (send-point f i x)
  (unless (point? x) (send-error f i "point" x))
  (bytes-append (float8 (point-x x)) (float8 (point-y x))))
(define (send-box f i x)
  (unless (pg-box? x) (send-error f i "box" x))
  (bytes-append (send-point f #f (pg-box-ne x))
                (send-point f #f (pg-box-sw x))))
(define (send-circle f i x)
  (unless (pg-circle? x) (send-error f i "circle" x))
  (bytes-append (send-point f #f (pg-circle-center x))
                (float8 (pg-circle-radius x))))
(define (send-lseg f i x)
  (unless (line? x) (send-error f i "lseg" x))
  (let ([points (line-string-points x)])
    (bytes-append (send-point f #f (car points))
                  (send-point f #f (cadr points)))))
(define (send-path f i x)
  (unless (pg-path? x) (send-error f i "path" x))
  (apply bytes-append
         (bytes (if (pg-path-closed? x) 1 0))
         (integer->integer-bytes (length (pg-path-points x)) 4 #t #t)
         (for/list ([p (in-list (pg-path-points x))])
           (send-point f #f p))))
(define (send-polygon f i x)
  (unless (polygon? x) (send-error f i "polygon" x))
  (let* ([points0 (line-string-points (polygon-exterior x))]
         [points (drop-right points0 1)]) ;; drop closing copy of first point
    (apply bytes-append
           (integer->integer-bytes (length points) 4 #t #t)
           (for/list ([p (in-list points)])
             (send-point f #f p)))))

;; send-error : string datum -> (raises error)
(define (send-error f i type datum)
  (error/no-convert f "PostgreSQL" type datum))

;; == Readers and writers ==

(define (typeid->type-reader fsym typeid)
  (case typeid
    ((16)   recv-boolean)
    ((17)   recv-bytea)
    ((18)   recv-char1)
    ((19)   recv-string)
    ((20)   recv-integer)
    ((21)   recv-integer)
    ((23)   recv-integer)
    ((25)   recv-string)
    ((26)   recv-integer)
    ((700)  recv-float)
    ((701)  recv-float)
    ((1042) recv-string)
    ((1043) recv-string)
    ((600)  recv-point)
    ((601)  recv-lseg)
    ((602)  recv-path)
    ((603)  recv-box)
    ((604)  recv-polygon)
    ((718)  recv-circle)
    ((1560) recv-bits)
    ((1562) recv-bits)
    ((1082) c-parse-date)
    ((1083) c-parse-time)
    ((1114) c-parse-timestamp)
    ((1184) c-parse-timestamp-tz)
    ((1186) c-parse-interval)
    ((1266) c-parse-time-tz)
    ((1700) c-parse-decimal)
    ((2249) recv-record)

    ;; "string" literals have type unknown; just treat as string
    ((705) recv-string)
    (else (error/unsupported-type fsym typeid (typeid->type typeid)))))

(define (typeid->type-writer typeid)
  (case typeid
    ((16)   send-boolean)
    ((17)   send-bytea)
    ((18)   send-char1)
    ((19)   send-string)
    ((20)   send-int8)
    ((21)   send-int2)
    ((23)   send-int4)
    ((25)   send-string)
    ((26)   send-int4)
    ((700)  send-float4)
    ((701)  send-float8)
    ((1042) send-string)
    ((1043) send-string)
    ((600)  send-point)
    ((601)  send-lseg)
    ((602)  send-path)
    ((603)  send-box)
    ((604)  send-polygon)
    ((718)  send-circle)
    ((1560) send-bits)
    ((1562) send-bits)
    ((1082) marshal-date)
    ((1083) marshal-time)
    ((1114) marshal-timestamp)
    ((1184) marshal-timestamp-tz)
    ((1186) marshal-interval)
    ((1266) marshal-time-tz)
    ((1700) marshal-decimal)

    ;; "string" literals have type unknown; just treat as string
    ((705)  send-string)
    (else (make-unsupported-writer typeid (typeid->type typeid)))))

(define (typeid->format x)
  (case x
    ((16 17 18 19 20 21 23 25 26 700 701 1042 1043 705) 1)
    ((600 601 602 603 604 718 1560 1562 2249) 1)
    (else 0)))

(define (make-unsupported-writer x t)
  (lambda (fsym . args)
    (error/unsupported-type fsym x t)))
