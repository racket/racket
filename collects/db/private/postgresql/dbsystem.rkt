#lang racket/base
(require racket/class
         racket/list
         racket/string
         racket/match
         (prefix-in srfi: srfi/19)
         "../generic/interfaces.rkt"
         "../generic/sql-data.rkt"
         "../generic/sql-convert.rkt"
         "../../util/datetime.rkt"
         "../../util/geometry.rkt"
         "../../util/postgresql.rkt"
         (only-in "message.rkt" field-dvec->typeid))
(provide dbsystem
         typeid->type-reader
         typeid->format
         classify-pg-sql)

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

;; SQL "parsing"
;; We just care about detecting commands that affect transaction status.

;; classify-pg-sql : string [nat] -> symbol/#f
(define classify-pg-sql
  ;; Source: http://www.postgresql.org/docs/current/static/sql-commands.html
  (make-sql-classifier
   `(("ABORT"                        rollback)
     ("BEGIN"                        start)
     ;; COMMIT PREPARED itself is harmless.
     ("COMMIT PREPARED"              #f) ;; Note: before COMMIT
     ("COMMIT"                       commit)
     ("DO"                           *do) ;; can do anything
     ("END"                          commit)
     ("EXECUTE"                      *execute) ;; can do anything
     ;; PREPARE TRANSACTION is like shift: it saves and aborts current transaction.
     ;; Perhaps all we care about is that it ends transaction, treat like commit/rollback.
     ("PREPARE TRANSACTION"          prepare-transaction) ;; Note: before PREPARE
     ("RELEASE SAVEPOINT"            release-savepoint)
     ;; For ROLLBACK variants, ordered carefully and expanded optional words
     ;; ROLLBACK PREPARED just deletes saved transaction
     ("ROLLBACK PREPARED"            #f)
     ("ROLLBACK WORK TO"             rollback-savepoint)
     ("ROLLBACK TRANSACTION TO"      rollback-savepoint)
     ("ROLLBACK TO"                  rollback-savepoint)
     ("ROLLBACK"                     rollback)
     ("SAVEPOINT"                    savepoint)
     ("START TRANSACTION"            start)
     )))

;; ========================================

;; Derived from 
;; http://www.us.postgresql.org/users-lounge/docs/7.2/postgres/datatype.html
;; and
;; result of "SELECT oid, typname, typelem FROM pg_type;"

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

  (600  point    () #t)
  (601  lseg     () #t)
  (602  path     () #t)
  (603  box      () #t)
  (604  polygon  () #t)
  (718  circle   () #t)

  ;; "string" literals have type unknown; just treat as string
  (705 unknown     ()        #t)

  (1000 boolean-array   () #t)
  (1001 bytea-array     () #t)
  (1002 char1-array     () #t)
  (1003 name-array      () #t)
  (1005 smallint-array  (int2-array) #t)
  (1007 integer-array   (int4-array) #t)
  (1009 text-array      () #t)
  (1028 oid-array       () #t)
  (1014 character-array (bpchar-array) #t)
  (1015 varchar-array   () #t)
  (1016 bigint-array    (int8-array) #t)
  (1017 point-array     () #t)
  (1018 lseg-array      () #t)
  (1019 path-array      () #t)
  (1020 box-array       () #t)
  (1021 real-array      (float4-array) #t)
  (1022 double-array    (float8-array) #t)
  (1027 polygon-array   () #t)
  (1561 bit-array       () #t)
  (1563 varbit-array    () #t)
  (719  circle-array    () #t)

  (1115 timestamp-array   () #t)
  (1182 date-array        () #t)
  (1183 time-array        () #t)
  (1185 timestamptz-array () #t)
  (1187 interval-array    () #t)
  (1231 decimal-array     (numeric-array) #t)
  (1270 timetz-array      () #t)

  ;; The following types are not supported.
  ;; (But putting their names here yields better not-supported errors.)

  (142  xml             () #f)
  (143  xml-array       () #f)
  (628  line            () #f)
  (629  line-array      () #f)
  (650  cidr            () #f)
  (651  cidr-array      () #f)
  (702  abstime         () #f)
  (703  reltime         () #f)
  (704  tinterval       () #f)
  (790  money           () #f)
  (829  macaddr         () #f)
  (869  inet            () #f)
  (791  money-array     () #f)
  (1023 abstime-array   () #f)
  (1024 reltime-array   () #f)
  (1025 tinterval-array () #f)
  (1040 macaddr-array   () #f)
  (1041 inet-array      () #f)
  (2249 record          () #f)
  (2287 record-array    () #f)
  (2950 uuid            () #f)
  (2951 uuid-array      () #f))

;; ============================================================

#|
BINARY VS TEXT FORMAT

For most types, we send and receive data in binary format
only. However, datetime types are tricky enough that binary format
isn't worth it (yet). Also decimal/numeric.

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

;; Text readers

(define (parse-date d)
  (srfi-date->sql-date
   (srfi:string->date d "~Y-~m-~d")))

(define time/ns-rx #rx"^[0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")
(define timestamp/ns-rx #rx"^.* [0-9]*:[0-9]*:[0-9]*\\.([0-9]*)")

(define (ns-of t rx)
  (let ([m (regexp-match rx t)])
    (if m
        (* #e1e9 (parse-exact-fraction (cadr m)))
        0)))

(define (parse-time t)
  (srfi-date->sql-time
   (srfi:string->date t "~k:~M:~S")
   (ns-of t time/ns-rx)))

(define (parse-time-tz t)
  (srfi-date->sql-time-tz
   (srfi:string->date t "~k:~M:~S~z")
   (ns-of t time/ns-rx)))

(define (parse-timestamp t)
  (srfi-date->sql-timestamp
   (srfi:string->date t "~Y-~m-~d ~k:~M:~S")
   (ns-of t timestamp/ns-rx)))

(define (parse-timestamp-tz t)
  (srfi-date->sql-timestamp-tz
   (srfi:string->date t "~Y-~m-~d ~k:~M:~S~z")
   (ns-of t timestamp/ns-rx)))

(define interval-rx
  (regexp
   (string-append "^"
                  "[\"]?" ;; when in array
                  "(?:(-?[0-9]*) years? *)?"
                  "(?:(-?[0-9]*) mons? *)?"
                  "(?:(-?[0-9]*) days? *)?"
                  "(?:(-?)([0-9]*):([0-9]*):([0-9]*)(?:\\.([0-9]*))?)?"
                  "[\"]?" ;; when in array
                  "$")))

(define (parse-interval s)
  (define (to-num m)
    (if m (string->number m) 0))
  (define match-result (regexp-match interval-rx s))
  (match match-result
    [(list _whole years months days tsign hours mins secs fsec)
     (let* ([years (to-num years)]
            [months (to-num months)]
            [days (to-num days)]
            [sg (if (equal? tsign "-") - +)]
            [hours (sg (to-num hours))]
            [mins (sg (to-num mins))]
            [secs (sg (to-num secs))]
            [nsecs (if fsec
                       (let ([flen (string-length fsec)])
                         (* (string->number (substring fsec 0 (min flen 9)))
                            (expt 10 (- 9 (min flen 9)))))
                       0)])
       (sql-interval years months days hours mins secs nsecs))]))

(define ((c-parse-array parse-elt) x)
  ;; NOTE: we assume that array enclosed with "{" and "}", and separator is ","
  (let* ([s (bytes->string/utf-8 x)]
         [vals
          (let loop ([s s])
            (cond [(equal? s "{}") '#()]
                  [(regexp-match? #rx"^{.*}$" s)
                   (let ([parts (regexp-split #rx"," s 1 (sub1 (string-length s)))])
                     (list->vector (map loop parts)))]
                  [(equal? s "NULL") sql-null]
                  [else (parse-elt s)]))]
         [lengths
          ;; NOTE: we assume array is well-formed (dimension lengths consistent)
          (cond [(zero? (vector-length vals)) null]
                [else
                 (let loop ([x vals])
                   (cond [(vector? x) (cons (vector-length x) (loop (vector-ref x 0)))]
                         [else null]))])])
    (pg-array (length lengths) lengths (map (lambda (_) 1) lengths) vals)))

;; Text writers

(define (marshal-date f i d)
  (unless (sql-date? d)
    (marshal-error f i "date" d))
  (srfi:date->string (sql-datetime->srfi-date d) "~Y-~m-~d"))

(define (marshal-time f i t)
  (unless (sql-time? t)
    (marshal-error f i "time" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N"))

(define (marshal-time-tz f i t)
  (unless (sql-time? t)
    (marshal-error f i "time" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~k:~M:~S.~N~z"))

(define (marshal-timestamp f i t)
  (unless (sql-timestamp? t)
    (marshal-error f i "timestamp" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N"))

(define (marshal-timestamp-tz f i t)
  (unless (sql-timestamp? t)
    (marshal-error f i "timestamp" t))
  (srfi:date->string (sql-datetime->srfi-date t) "~Y-~m-~d ~k:~M:~S.~N~z"))

(define (marshal-interval f i t)
  (define (tag num unit)
    (if (zero? num) "" (format "~a ~a " num unit)))
  (match t
    [(sql-interval years months days hours minutes seconds nanoseconds)
     ;; Note: we take advantage of PostgreSQL's liberal interval parser
     ;; and we acknowledge its micro-second precision.
     (string-append (tag years "years")
                    (tag months "months")
                    (tag days "days")
                    (tag hours "hours")
                    (tag minutes "minutes")
                    (tag seconds "seconds")
                    (tag (quotient nanoseconds 1000) "microseconds"))]
    [else
     (marshal-error f i "interval" t)]))

(define ((marshal-array marshal-elt) f i x0)
  (define x
    (cond [(pg-array? x0) x0]
          [(list? x0) (list->pg-array x0)]
          [else (marshal-error f i "pg-array" x0)]))
  (match x
    [(pg-array dims lengths lbounds vals)
     (cond [(zero? dims) "{}"]
           [else
            (let loop ([dims dims] [v vals])
              (cond [(zero? dims)
                     (if (sql-null? v)
                         "NULL"
                         (marshal-elt f #f v))]
                    [else
                     (string-append "{"
                                    (string-join (for/list ([v* (in-vector v)])
                                                   (loop (sub1 dims) v*))
                                                 ",")
                                    "}")]))])]))

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

(define (recv-unsigned-integer x)
  (integer-bytes->integer x #f #t))

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

#|
(require srfi/19)
(define (recv-date x)
  (let* ([POSTGRESQL-JD-ADJUST 2451545] ;; from $PG/src/include/utils/datetime.h
         [jd (+ (integer-bytes->integer x #t #t 0 4) POSTGRESQL-JD-ADJUST)]
         [t (julian-day->date jd 0)]) ;; gives noon on the designated day
    (srfi-date->sql-date t)))
(define-values (recv-time recv-timetz)
  (let ()
    (define (usec->time t tz)
      (let*-values ([(t usec) (quotient/remainder t #e1e6)]
                    [(t sec)  (quotient/remainder t 60)]
                    [(hr min)  (quotient/remainder t 60)])
        (make-sql-time hr min sec (* 1000 usec) tz)))
    (define (recv-time x)
      (usec->time (integer-bytes->integer x #t #t 0 8) #f))
    (define (recv-timetz x)
      (let* ([t (integer-bytes->integer x #t #t 0 8)]
             [tz (integer-bytes->integer x #t #t 8 12)])
        ;; FIXME: seem to need to invert timezone... why?
        (usec->time t (- tz))))
    (values recv-time recv-timetz)))
|#

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

(define (recv-array x)
  (let ([start 0])
    (define (get-int signed?)
      (begin0 (integer-bytes->integer x signed? #t start (+ start 4))
        (set! start (+ start 4))))
    (define (get-bytes len)
      (begin0 (subbytes x start (+ start len))
        (set! start (+ start len))))
    (let* ([ndim (get-int #t)]
           [flags (get-int #f)]
           [elttype (get-int #f)]
           [reader (typeid->type-reader 'recv-array elttype)]
           [bounds
            (for/list ([i (in-range ndim)])
              (let* ([dim (get-int #t)]
                     [lbound (get-int #t)])
                (cons dim lbound)))]
           [vals ;; (vector^ndim X)
            (cond [(zero? ndim) '#()]
                  [else
                   (let loop ([bounds bounds])
                     (cond [(pair? bounds)
                            (for/vector ([i (in-range (car (car bounds)))])
                                        (loop (cdr bounds)))]
                           [else
                            (let* ([len (get-int #t)])
                              (cond [(= len -1) sql-null]
                                    [else (reader (get-bytes len))]))]))])])
      (pg-array ndim (map car bounds) (map cdr bounds) vals))))

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

(define-values (c-parse-date
                c-parse-time
                c-parse-time-tz
                c-parse-timestamp
                c-parse-timestamp-tz
                c-parse-interval
                c-parse-decimal)
  (let ([c (lambda (f) (lambda (x) (f (bytes->string/utf-8 x))))])
    (values (c parse-date)
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

(define ((send-array elttype) f i x0)
  ;; NOTE: elttype must have binary writer
  (define writer (typeid->type-writer elttype))
  (define x
    (cond [(pg-array? x0) x0]
          [(list? x0) (list->pg-array x0)]
          [else (send-error f i "pg-array" x0)]))
  (match x
    [(pg-array ndim counts lbounds vals)
     (let ([out (open-output-bytes)])
       (write-bytes (integer->integer-bytes ndim 4 #t #t) out)
       (write-bytes (integer->integer-bytes 0 4 #t #t) out)
       (write-bytes (integer->integer-bytes elttype 4 #t #t) out)
       (for ([count (in-list counts)]
             [lbound (in-list lbounds)])
         (write-bytes (integer->integer-bytes count 4 #t #t) out)
         (write-bytes (integer->integer-bytes lbound 4 #t #t) out))
       (unless (zero? ndim)
         (let loop ([n ndim] [vals vals])
           (cond [(zero? n)
                  (cond [(sql-null? vals)
                         (write-bytes (integer->integer-bytes -1 4 #t #t) out)]
                        [else
                         (let ([b (writer f #f vals)])
                           (write-bytes (integer->integer-bytes (bytes-length b) 4 #t #t) out)
                           (write-bytes b out))])]
                 [else
                  (for ([v (in-vector vals)])
                    (loop (sub1 n) v))])))
       (get-output-bytes out))]))

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
    ((2249) recv-record)

    ((1000) recv-array) ;; _bool
    ((1001) recv-array) ;; _bytea
    ((1002) recv-array) ;; _char
    ((1003) recv-array) ;; _name
    ((1005) recv-array) ;; _int2
    ((1007) recv-array) ;; _int4
    ((1009) recv-array) ;; _text
    ((1028) recv-array) ;; _oid
    ((1014) recv-array) ;; _bpchar
    ((1015) recv-array) ;; _varchar
    ((1016) recv-array) ;; _int8
    ((1017) recv-array) ;; _point
    ((1018) recv-array) ;; _lseg
    ((1019) recv-array) ;; _path
    ((1020) recv-array) ;; _box
    ((1021) recv-array) ;; _float4
    ((1022) recv-array) ;; _float8
    ((1027) recv-array) ;; _polygon
    ((1561) recv-array) ;; _bit
    ((1563) recv-array) ;; _varbit
    ((2287) recv-array) ;; _record
    ((719)  recv-array) ;; _circle

    ((1082) c-parse-date)
    ((1083) c-parse-time)
    ((1114) c-parse-timestamp)
    ((1184) c-parse-timestamp-tz)
    ((1186) c-parse-interval)
    ((1266) c-parse-time-tz)
    ((1700) c-parse-decimal)

    ((1115) (c-parse-array parse-timestamp))
    ((1182) (c-parse-array parse-date))
    ((1183) (c-parse-array parse-time))
    ((1185) (c-parse-array parse-timestamp-tz))
    ((1187) (c-parse-array parse-interval))
    ((1231) (c-parse-array parse-decimal))
    ((1270) (c-parse-array parse-time-tz))

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

    ((1000) (send-array      16)) ;; _bool
    ((1001) (send-array      17)) ;; _bytea
    ((1002) (send-array      18)) ;; _char
    ((1003) (send-array      19)) ;; _name
    ((1005) (send-array      21)) ;; _int2
    ((1007) (send-array      23)) ;; _int4
    ((1009) (send-array      25)) ;; _text
    ((1028) (send-array      26)) ;; _oid
    ((1014) (send-array    1042)) ;; _bpchar
    ((1015) (send-array    1043)) ;; _varchar
    ((1016) (send-array      20)) ;; _int8
    ((1017) (send-array     600)) ;; _point
    ((1018) (send-array     601)) ;; _lseg
    ((1019) (send-array     602)) ;; _path
    ((1020) (send-array     603)) ;; _box
    ((1021) (send-array     700)) ;; _float4
    ((1022) (send-array     701)) ;; _float8
    ((1027) (send-array     604)) ;; _polygon
    ((1263) (send-array    2275)) ;; _cstring
    ((1561) (send-array    1560)) ;; _bit
    ((1563) (send-array    1562)) ;; _varbit
    ((719)  (send-array     718)) ;; _circle

    ((1082) marshal-date)
    ((1083) marshal-time)
    ((1114) marshal-timestamp)
    ((1184) marshal-timestamp-tz)
    ((1186) marshal-interval)
    ((1266) marshal-time-tz)
    ((1700) marshal-decimal)

    ((1115) (marshal-array marshal-timestamp))
    ((1182) (marshal-array marshal-date))
    ((1183) (marshal-array marshal-time))
    ((1185) (marshal-array marshal-timestamp-tz))
    ((1187) (marshal-array marshal-interval))
    ((1231) (marshal-array marshal-decimal))
    ((1270) (marshal-array marshal-time-tz))

    ;; "string" literals have type unknown; just treat as string
    ((705)  send-string)
    (else (make-unsupported-writer typeid (typeid->type typeid)))))

(define (typeid->format x)
  (case x
    ((16 17 18 19 20 21 23 25 26 700 701 1042 1043 705) 1)
    ((600 601 602 603 604 718 1560 1562 2249) 1)
    ((1000 1001 1002 1003 1005 1007 1009 1028 1010
      1011 1012 1014 1015 1016 1017 1018 1019 1020
      1021 1022 1027 1561 1563 2287) 1)
    (else 0)))

(define (make-unsupported-writer x t)
  (lambda (fsym . args)
    (error/unsupported-type fsym x t)))
