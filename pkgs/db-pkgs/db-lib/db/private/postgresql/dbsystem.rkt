#lang racket/base
(require racket/class
         racket/list
         racket/match
         (prefix-in srfi: srfi/19)
         json
         db/private/generic/interfaces
         db/private/generic/common
         db/private/generic/sql-data
         db/private/generic/sql-convert
         "../../util/datetime.rkt"
         "../../util/geometry.rkt"
         "../../util/postgresql.rkt"
         (only-in "message.rkt" field-dvec->typeid))
(provide dbsystem/integer-datetimes
         dbsystem/floating-point-datetimes
         typeid->type-reader
         typeid->format
         use-integer-datetimes?
         classify-pg-sql)

(define postgresql-dbsystem%
  (class* dbsystem-base% (dbsystem<%>)
    (init-field integer-datetimes?)

    (define/public (get-short-name) 'postgresql)
    (define/override (get-type-list) type-list)

    (define/public (has-support? option)
      (case option
        ((real-infinities) #t)
        ((numeric-infinities) #t)
        (else #f)))

    (define/public (get-parameter-handlers param-typeids)
      (let ([writers (map typeid->type-writer param-typeids)])
        (if integer-datetimes?
            writers
            (map (lambda (w)
                   (lambda (f x)
                     (parameterize ((use-integer-datetimes? #f))
                       (w f x))))
                 writers))))

    (define/public (field-dvecs->typeids dvecs)
      (map field-dvec->typeid dvecs))

    (define/public (describe-params typeids)
      (map describe-typeid typeids))

    (define/public (describe-fields field-dvecs)
      (for/list ([dvec (in-list field-dvecs)])
        (describe-typeid (field-dvec->typeid dvec))))

    (super-new)))

(define dbsystem/integer-datetimes
  (new postgresql-dbsystem% (integer-datetimes? #t)))
(define dbsystem/floating-point-datetimes
  (new postgresql-dbsystem% (integer-datetimes? #f)))

;; ========================================

;; SQL "parsing"

;; We care about detecting:
;;  - statements that affect transaction status
;;  - statements that are safe for (vs invalidate) the statement cache

;; classify-pg-sql : string [nat] -> symbol/#f
(define classify-pg-sql
  ;; Source: http://www.postgresql.org/docs/current/static/sql-commands.html
  (make-sql-classifier
   `(;; Statements that do not invalidate previously prepared statements
     ("SELECT" select)
     ("INSERT" insert)
     ("UPDATE" update)
     ("DELETE" delete)
     ("WITH"   with)

     ;; Transactional statements
     ("ABORT"                        rollback)
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

;; ============================================================

(define-syntax-rule (type-table (type-list typeid->type describe-typeid)
                                (typeid->format typeid->type-reader typeid->type-writer)
                      (typeid type since-version fmt reader writer) ...)
  (begin (define-type-table (type-list typeid->type describe-typeid)
           (typeid type since-version) ...)
         (define (typeid->type-reader fsym tid)
           (let ([result
                  (case tid
                    ((typeid) reader) ...
                    (else #f))])
             (or result (error/unsupported-type fsym tid (typeid->type tid)))))
         (define (typeid->type-writer tid)
           (let ([result
                  (case tid
                    ((typeid) writer) ...
                    (else #f))])
             (or result (make-unsupported-writer tid (typeid->type tid)))))
         (define (typeid->format tid)
           (case tid
             ((typeid) fmt) ...
             (else 0)))))

(define (make-unsupported-writer x t)
  (lambda (fsym . args)
    (error/unsupported-type fsym x t)))

;; ============================================================

;; Derived from 
;; http://www.postgresql.org/docs/current/static/datatype.html
;; and
;; result of "SELECT oid, typname, typelem FROM pg_type;"

(type-table (type-list typeid->type describe-typeid)
            (typeid->format typeid->type-reader typeid->type-writer)
  (16   boolean      0   1  recv-boolean      send-boolean)
  (17   bytea        0   1  recv-bytea        send-bytea)
  (18   char1        0   1  recv-char1        send-char1)
  (19   name         0   1  recv-string       send-string)
  (20   bigint       0   1  recv-integer      send-int8)
  (21   smallint     0   1  recv-integer      send-int2)
  (23   integer      0   1  recv-integer      send-int4)
  (25   text         0   1  recv-string       send-string)
  (26   oid          0   1  recv-integer      send-int4)
  (700  real         0   1  recv-float        send-float4)
  (701  double       0   1  recv-float        send-float8)
  (1042 character    0   1  recv-string       send-string)
  (1043 varchar      0   1  recv-string       send-string)
  (1082 date         0   1  recv-date         send-date)
  (1083 time         0   1  recv-time         send-time)
  (1114 timestamp    0   1  recv-timestamp    send-timestamp)
  (1184 timestamptz  0   1  recv-timestamptz  send-timestamptz)
  (1186 interval     0   1  recv-interval     send-interval)
  (1266 timetz       0   1  recv-timetz       send-timetz)
  (1700 decimal      0   1  recv-numeric      send-numeric)
  (1560 bit          0   1  recv-bits         send-bits)
  (1562 varbit       0   1  recv-bits         send-bits)
  (114  json         9.2 1  recv-json         send-json)

  (600  point        0   1  recv-point        send-point)
  (601  lseg         0   1  recv-lseg         send-lseg)
  (602  path         0   1  recv-path         send-path)
  (603  box          0   1  recv-box          send-box)
  (604  polygon      0   1  recv-polygon      send-polygon)
  (718  circle       0   1  recv-circle       send-circle)

  (3904 int4range    9.2 1  (recv-range 23)   (send-range 23))
  (3926 int8range    9.2 1  (recv-range 20)   (send-range 20))
  (3906 numrange     9.2 1  (recv-range 1700) (send-range 1700))
  (3908 tsrange      9.2 1  (recv-range 1114) (send-range 1114))
  (3910 tstzrange    9.2 1  (recv-range 1184) (send-range 1184))
  (3912 daterange    9.2 1  (recv-range 1082) (send-range 1082))

  ;; "string" literals have type unknown; just treat as string
  (705 unknown       0   1  recv-string       send-string)

  ;; Array types

  (1000 boolean-array     0   1  recv-array (send-array 16))
  (1001 bytea-array       0   1  recv-array (send-array 17))
  (1002 char1-array       0   1  recv-array (send-array 18))
  (1003 name-array        0   1  recv-array (send-array 19))
  (1005 smallint-array    0   1  recv-array (send-array 21))
  (1007 integer-array     0   1  recv-array (send-array 23))
  (1009 text-array        0   1  recv-array (send-array 25))
  (1028 oid-array         0   1  recv-array (send-array 26))
  (1014 character-array   0   1  recv-array (send-array 1042))
  (1015 varchar-array     0   1  recv-array (send-array 1043))
  (1016 bigint-array      0   1  recv-array (send-array 20))
  (1017 point-array       0   1  recv-array (send-array 600))
  (1018 lseg-array        0   1  recv-array (send-array 601))
  (1019 path-array        0   1  recv-array (send-array 602))
  (1020 box-array         0   1  recv-array (send-array 603))
  (1021 real-array        0   1  recv-array (send-array 700))
  (1022 double-array      0   1  recv-array (send-array 701))
  (1027 polygon-array     0   1  recv-array (send-array 604))
  (719  circle-array      0   1  recv-array (send-array 718))
  (1561 bit-array         0   1  recv-array (send-array 1560))
  (1563 varbit-array      0   1  recv-array (send-array 1562))
  (199  json-array        9.2 1  recv-array (send-array 114))

  (1115 timestamp-array   0   1  recv-array (send-array 1114))
  (1182 date-array        0   1  recv-array (send-array 1082))
  (1183 time-array        0   1  recv-array (send-array 1083))
  (1185 timestamptz-array 0   1  recv-array (send-array 1184))
  (1187 interval-array    0   1  recv-array (send-array 1186))
  (1231 decimal-array     0   1  recv-array (send-array 1700))
  (1270 timetz-array      0   1  recv-array (send-array 1266))

  (3905 int4range-array   9.2 1  recv-array (send-array 3904))
  (3927 int8range-array   9.2 1  recv-array (send-array 3926))
  (3907 numrange-array    9.2 1  recv-array (send-array 3906))
  (3909 tsrange-array     9.2 1  recv-array (send-array 3908))
  (3911 tstzrange-array   9.2 1  recv-array (send-array 3910))
  (3913 daterange-array   9.2 1  recv-array (send-array 3912))

  (2275 cstring           0   1 recv-string send-string)
  ;; Receive but do not send
  (2249 record            #f  1 recv-record #f)
  (2287 record-array      #f  1 recv-array  #f)

  ;; The following types are not supported.
  ;; (But putting their names here yields better not-supported errors.)

  (142  xml               #f 0 #f #f)
  (143  xml-array         #f 0 #f #f)

  (628  line              #f 0 #f #f)
  (629  line-array        #f 0 #f #f)
  (650  cidr              #f 0 #f #f)
  (651  cidr-array        #f 0 #f #f)
  (702  abstime           #f 0 #f #f)
  (703  reltime           #f 0 #f #f)
  (704  tinterval         #f 0 #f #f)
  (790  money             #f 0 #f #f)
  (829  macaddr           #f 0 #f #f)
  (869  inet              #f 0 #f #f)
  (791  money-array       #f 0 #f #f)
  (1023 abstime-array     #f 0 #f #f)
  (1024 reltime-array     #f 0 #f #f)
  (1025 tinterval-array   #f 0 #f #f)
  (1040 macaddr-array     #f 0 #f #f)
  (1041 inet-array        #f 0 #f #f)
  (2950 uuid              #f 0 #f #f)
  (2951 uuid-array        #f 0 #f #f))

;; ----------------------------------------

#|
BINARY DATA FORMAT

We send and receive data in binary format only.

Some datetime types (see below) have two binary formats: integer
microseconds vs floating-point seconds since a particular point in
time. The integer format is indicated by the server
variable "integer_datetimes=on", the default since about 2008. If the
variable is off or not present, use floats. The relevant send/recv
functions depend on use-integer-datetimes? parameter.

Domain typeids never seem to appear as result typeids, but do appear
as parameter typeids.

----

Notes on binary formats, mostly from $PG/src/include/utils/*.[ch]

bit, varbit = len:int4 byte* (0-padded on *left*)

date = int4 (days since 2000-01-01)
timestamp = (int8 or float8)
timestamptz = (int8 or float8)
time = (int8 or float8)
timetz = (int8 or float8) zone-secs:int4
interval = (usecs:int8 or secs:float8) days:int4 months:int4

  (time*, timestamp*, interval depend on "integer_datetimes" parameter, but date does not)

inet, cidr = family:byte bits:byte is_cidr:byte addrlen:byte addr:be-integer
  is_cidr is ignored

record = cols:int4 (typeoid:int4 len/-1:int4 data:byte^len)^cols 

range = flags:byte (len:int4 data:byte^len)^{0..2}

|#

(define POSTGRESQL-JD-ADJUST 2451545) ;; from $PG/src/include/utils/datetime.h

(define use-integer-datetimes? (make-parameter #t))

;; ----------------------------------------

;; Binary readers
;; Take bytes, start offset, end offset (but most ignore end)

(define (recv-bits buf start end)
  (let* ([bitslen (integer-bytes->integer buf #t #t start (+ start 4))])
    (make-sql-bits/bytes bitslen (subbytes buf (+ start 4) end) 0)))

(define (recv-boolean buf start end)
  (case (bytes-ref buf start)
    ((0) #f)
    ((1) #t)
    (else (error/internal* 'recv-boolean "bad value"
                           '("value" value) (bytes-ref buf start)))))

(define (recv-char1 buf start end)
  (integer->char (bytes-ref buf start)))

(define (recv-bytea buf start end)
  (subbytes buf start end))

(define (recv-string buf start end)
  (bytes->string/utf-8 buf #f start end))

(define (recv-integer buf start end)
  (integer-bytes->integer buf #t #t start end))

(define (recv-unsigned-integer buf start end)
  (integer-bytes->integer buf #f #t start end))

(define (recv-float buf start end)
  (floating-point-bytes->real buf #t start end))

(define (get-double bs offset)
  (floating-point-bytes->real bs #t offset (+ 8 offset)))
(define (recv-point buf start end)
  (point (get-double buf start) (get-double buf (+ start 8))))
(define (recv-box buf start end)
  (pg-box (recv-point buf start #f) (recv-point buf (+ start 16) #f)))
(define (recv-circle buf start end)
  (pg-circle (recv-point buf start #f) (get-double buf (+ start 16))))
(define (recv-lseg buf start end)
  (line-string (list (recv-point buf start #f) (recv-point buf (+ start 16) #f))))
(define (recv-path buf start end)
  (pg-path (not (zero? (bytes-ref buf start)))
           (for/list ([i (integer-bytes->integer buf #t #t (+ start 1) (+ start 5))])
             (recv-point buf (+ start 5 (* 16 i)) #f))))
(define (recv-polygon buf start end)
  (let* ([points0
          (for/list ([i (in-range (integer-bytes->integer buf #t #t start (+ start 4)))])
            (recv-point buf (+ start 4 (* 16 i)) #f))]
         [points (append points0 (list (car points0)))])
    (polygon (line-string points)
             null)))

(define (recv-date buf start end)
  (let* ([jd (+ (integer-bytes->integer buf #t #t start (+ start 4)) POSTGRESQL-JD-ADJUST)]
         [t (srfi:julian-day->date jd 0)]) ;; gives noon on the designated day
    (srfi-date->sql-date t)))

(define (get-usec buf start)
  (cond [(use-integer-datetimes?)
         (let ([usec (integer-bytes->integer buf #t #t start (+ start 8))])
           (cond [(= usec (sub1 (expt 2 63))) +inf.0]
                 [(= usec (- (expt 2 63))) -inf.0]
                 [else usec]))]
        [else
         (let ([sec (floating-point-bytes->real buf #t start (+ start 8))])
           (if (rational? sec)
               (inexact->exact (round (* sec #i1e6)))
               sec))]))

(define (usec->hmsn usec)
  (let*-values ([(sec usec) (quotient/remainder usec #e1e6)]
                [(min sec) (quotient/remainder sec 60)]
                [(hr min) (quotient/remainder min 60)])
    (values hr min sec (* #e1e3 usec))))

(define (recv-time buf start end)
  (let-values ([(hr min sec nsec) (usec->hmsn (get-usec buf start))])
    (make-sql-time hr min sec nsec #f)))

(define (recv-timetz buf start end)
  (let-values ([(hr min sec nsec) (usec->hmsn (get-usec buf start))]
               [(tz) (integer-bytes->integer buf #t #t (+ start 8) (+ start 12))])
    ;; FIXME: seem to need to invert timezone... why?
    (make-sql-time hr min sec nsec (- tz))))

(define (recv-timestamp* buf start end tz)
  (define usec-in-day (* #e1e6 60 60 24))
  (let ([usec (get-usec buf start)])
    (cond [(rational? usec)
           (let*-values ([(day usec) (quotient/remainder usec usec-in-day)]
                         [(day usec) (if (negative? usec)
                                         (values (sub1 day) (+ usec-in-day usec))
                                         (values day usec))]
                         [(jd) (+ day POSTGRESQL-JD-ADJUST)]
                         [(hr min sec nsec) (usec->hmsn usec)]
                         [(sd) (srfi:julian-day->date jd 0)])
             (make-sql-timestamp (srfi:date-year sd)
                                 (srfi:date-month sd)
                                 (srfi:date-day sd)
                                 hr min sec nsec tz))]
          ;; Else +/-inf.0
          [else usec])))

(define (recv-timestamp buf start end)
  (recv-timestamp* buf start end #f))
(define (recv-timestamptz buf start end)
  (recv-timestamp* buf start end 0))

(define (recv-interval buf start end)
  (let*-values ([(hr min sec nsec) (usec->hmsn (get-usec buf start))]
                [(day) (integer-bytes->integer buf #t #t (+ start 8) (+ start 12))]
                [(mon) (integer-bytes->integer buf #t #t (+ start 12) (+ start 16))]
                [(yr mon) (quotient/remainder mon 12)])
    (make-sql-interval yr mon day hr min sec nsec)))

(define (recv-record buf start end)
  (define (get-int signed?)
    (begin0 (integer-bytes->integer buf signed? #t start (+ start 4))
      (set! start (+ start 4))))
  (define (recv-col)
    (let* ([typeid (get-int #t)]
           [len (get-int #t)])
      (if (= len -1)
          sql-null
          (let* ([bin? (= (typeid->format typeid) 1)] ;; binary reader available
                 [reader (and bin? (typeid->type-reader 'recv-record typeid))])
            (if reader
                (reader buf start (+ start (max 0 len)))
                'unreadable)))))
  (let* ([columns (get-int #t)]
         [result (make-vector columns #f)])
    (for ([i (in-range columns)])
      (vector-set! result i (recv-col)))
    result))

(define (recv-array buf start end)
  (define (get-int signed?)
    (begin0 (integer-bytes->integer buf signed? #t start (+ start 4))
      (set! start (+ start 4))))
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
                                  [else
                                   (begin0 (reader buf start (+ start len))
                                     (set! start (+ start len)))]))]))])])
    (pg-array ndim (map car bounds) (map cdr bounds) vals)))

(define (recv-numeric buf start end)
  (define (get-int2 offset [signed? #t])
    (integer-bytes->integer buf signed? #t (+ start offset) (+ start offset 2)))
  (let* ([NBASE  #e1e4]
         [NUMERIC_POS #x0000]
         [NUMERIC_NEG #x4000]
         [NUMERIC_NAN #xC000]
         [digits (get-int2 0)]
         [weight (get-int2 2)]
         [sign   (get-int2 4 #f)]
         [dscale (get-int2 6)]  ;; "display scale", can ignore
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
           +nan.0]
          [else (error/internal 'recv-numeric "bad sign: ~e" sign)])))

(define (recv-json buf start end)
  (bytes->jsexpr (subbytes buf start end)))

(define (recv-range elttype)
  (define EMPTY  #x01)
  (define LB_INC #x02)
  (define UB_INC #x04)
  (define LB_INF #x08)
  (define UB_INF #x10)
  (define reader (typeid->type-reader 'recv-range elttype))
  (lambda (buf start end)
    (let* ([flags (bytes-ref buf start)]
           [is-empty? (not (zero? (bitwise-and flags EMPTY)))]
           [has-lb? (and (not is-empty?) (zero? (bitwise-and flags LB_INF)))]
           [has-ub? (and (not is-empty?) (zero? (bitwise-and flags UB_INF)))]
           [includes-lb? (not (zero? (bitwise-and flags LB_INC)))]
           [includes-ub? (not (zero? (bitwise-and flags UB_INC)))])
      ;; get-bound : boolean nat -> (values datum nat)
      (define (get-bound has? start)
        (cond [has?
               (let* ([len (integer-bytes->integer buf #t #t start (+ start 4))]
                      [data-start (+ start 4)]
                      [data-end (+ data-start len)]
                      [data (reader buf data-start data-end)])
                 (values data data-end))]
              [else
               (values #f start)]))
      (let*-values ([(next-start) (+ start 1)]
                    [(lb next-start) (get-bound has-lb? next-start)]
                    [(ub next-start) (get-bound has-ub? next-start)])
        ;; expect next-start = end
        (if is-empty?
            (pg-empty-range)
            (pg-range lb includes-lb? ub includes-ub?))))))

;; Binary writers

(define (send-boolean f x)
  (case x
    ((#t) (bytes 1))
    ((#f) (bytes 0))
    (else (send-error f "boolean" x #:contract 'boolean?))))

(define (send-bits f x)
  (unless (sql-bits? x) (send-error f "bits" x #:contract 'sql-bits?))
  (let-values ([(len bv start) (align-sql-bits x 'left)])
    (bytes-append (integer->integer-bytes len 4 #t #t)
                  (if (zero? start) bv (subbytes bv start)))))

(define (send-char1 f x)
  (let ([n (if (char? x) (char->integer x) x)])
    (unless (uint8? n) (send-error f "char1" x))
    (bytes n)))

(define (send-bytea f x)
  (unless (bytes? x) (send-error f "bytea" x #:contract 'bytes?))
  x)

(define (send-string f x)
  (unless (string? x) (send-error f "string" x #:contract 'string?))
  (string->bytes/utf-8 x))

(define (send-int2 f n)
  (unless (int16? n) (send-error f "int2" n #:contract 'int16?))
  (integer->integer-bytes n 2 #t #t))

(define (send-int4 f n)
  (unless (int32? n) (send-error f "int4" n #:contract 'int32?))
  (integer->integer-bytes n 4 #t #t))

(define (send-int8 f n)
  (unless (int64? n) (send-error f "int8" n #:contract 'int64?))
  (integer->integer-bytes n 8 #t #t))

(define (send-float* f n type size)
  (unless (real? n) (send-error f type n #:contract 'real?))
  (real->floating-point-bytes n size #t))

(define (send-float4 f n)
  (send-float* f n "float4" 4))

(define (send-float8 f n)
  (send-float* f n "float8" 8))

(define (float8 x)
  (real->floating-point-bytes x 8 #t))
(define (send-point f x)
  (unless (point? x) (send-error f "point" x #:contract 'point?))
  (bytes-append (float8 (point-x x)) (float8 (point-y x))))
(define (send-box f x)
  (unless (pg-box? x) (send-error f "box" x #:contract 'pg-box?))
  (bytes-append (send-point f (pg-box-ne x))
                (send-point f (pg-box-sw x))))
(define (send-circle f x)
  (unless (pg-circle? x) (send-error f "circle" x #:contract 'pg-circle?))
  (bytes-append (send-point f (pg-circle-center x))
                (float8 (pg-circle-radius x))))
(define (send-lseg f x)
  (unless (line? x) (send-error f "lseg" x #:contract 'line?))
  (let ([points (line-string-points x)])
    (bytes-append (send-point f (car points))
                  (send-point f (cadr points)))))
(define (send-path f x)
  (unless (pg-path? x) (send-error f "path" x #:contract 'pg-path?))
  (apply bytes-append
         (bytes (if (pg-path-closed? x) 1 0))
         (integer->integer-bytes (length (pg-path-points x)) 4 #t #t)
         (for/list ([p (in-list (pg-path-points x))])
           (send-point f p))))
(define (send-polygon f x)
  (unless (polygon? x) (send-error f "polygon" x #:contract 'polygon?))
  (let* ([points0 (line-string-points (polygon-exterior x))]
         [points (drop-right points0 1)]) ;; drop closing copy of first point
    (apply bytes-append
           (integer->integer-bytes (length points) 4 #t #t)
           (for/list ([p (in-list points)])
             (send-point f p)))))

(define (send-date f x)
  (cond [(sql-date? x)
         (let* ([d (sql-datetime->srfi-date x)]
                ;; julian day starts at noon of date, so round up (ceiling)
                [jd (ceiling (srfi:date->julian-day d))]
                [jd* (- jd POSTGRESQL-JD-ADJUST)])
           (integer->integer-bytes jd* 4 #t #t))]
        [else (send-error f "date" x #:contract 'sql-date?)]))

(define (hmsn->usec-bytes hr min sec nsec)
  (let* ([min (+ min (* hr 60))]
         [sec (+ sec (* min 60))]
         [usec (+ (quotient nsec #e1e3) (* sec #e1e6))])
    (cond [(use-integer-datetimes?)
           (integer->integer-bytes usec 8 #t #t)]
          [else
           (let ([sec (/ usec #i1e6)])
             (real->floating-point-bytes sec 8 #t))])))

(define (send-time f x)
  (match x
    [(sql-time h m s ns _tz)
     (hmsn->usec-bytes h m s ns)]
    [_ (send-error f "time" x #:contract 'sql-time?)]))

(define (send-timetz f x)
  (match x
    [(sql-time h m s ns (? values tz))
     (bytes-append (hmsn->usec-bytes h m s ns)
                   ;; FIXME: seem to need to invert timezone (see also recv-timetz)
                   (integer->integer-bytes (- tz) 4 #t #t))]
    [_ (send-error f "time with time zone" x
                   #:contract '(and/c sql-time? sql-time-tz))]))

(define (send-timestamp* f x tz?)
  (match x
    [(sql-timestamp yr mon day hr min sec nsec (? (if tz? values void) tz))
     (let* ([sd (srfi:make-date 0 0 0 12 day mon yr 0)]
            [jd (srfi:date->julian-day sd)]
            [jd* (- jd POSTGRESQL-JD-ADJUST)]
            [hr (+ hr (* jd* 24))]
            [sec (- sec (or tz 0))])
       (hmsn->usec-bytes hr min sec nsec))]
    [+inf.0
     (if (use-integer-datetimes?)
         (integer->integer-bytes (sub1 (expt 2 63)) 8 #t #t)
         (real->floating-point-bytes +inf.0 8 #t))]
    [-inf.0
     (if (use-integer-datetimes?)
         (integer->integer-bytes (- (expt 2 63)) 8 #t #t)
         (real->floating-point-bytes -inf.0 8 #t))]
    [_
     (let ([type (if tz? "timestamp with time zone" "timestamp without time zone")]
           [ctc (if tz? '(and/c sql-timestamp? sql-timestamp-tz) 'sql-timestamp?)])
       (send-error f type x #:contract ctc))]))

(define (send-timestamp f x) (send-timestamp* f x #f))
(define (send-timestamptz f x) (send-timestamp* f x #t))

(define (send-interval f x)
  (match x
    [(sql-interval yr mon day hr min sec nsec)
     (let ([mon (+ mon (* yr 12))])
       (bytes-append (hmsn->usec-bytes hr min sec nsec)
                     (integer->integer-bytes day 4 #t #t)
                     (integer->integer-bytes mon 4 #t #t)))]
    [_ (send-error f "interval" x #:contract 'sql-interval?)]))

;; round-up-to : nat nat>0 -> nat
;; round n up to the nearest multiple of m
(define (round-up-to n m)
  (let ([rem (remainder n m)])
    (if (zero? rem) n (+ n (- m rem)))))

;; numeric: digits:int2 weight:int2 sign:int2 dscale:int2 {digit:int2}^digits
;;   (abs x) = <integer-from-digits> * NBASE^(digits-(weight+1))
;;           = SUM {i in 0..} { digit_i * NBASE^(weight-i) }
(define (send-numeric f x)
  (define NBASE #e1e4)
  (define NBASE_MAGN 4)
  (define NUMERIC_POS #x0000)
  (define NUMERIC_NEG #x4000)
  (define NUMERIC_NAN #xC000)
  (define (mkint2 n [signed? #t])
    (integer->integer-bytes n 2 signed? #t))
  (define (nat->bigits n)
    (let loop ([n n] [tail null])
      (cond [(zero? n) tail]
            [else (let-values ([(rest bigit) (quotient/remainder n NBASE)])
                    (loop rest (cons bigit tail)))])))
  (define (make-numeric bigit-count weight neg? dscale bigits)
    (let ([sign-part (if neg? NUMERIC_NEG NUMERIC_POS)])
      (apply bytes-append
             (map mkint2 (list* bigit-count weight sign-part dscale bigits)))))

  (cond [(rational? x)
         (let* ([orig-x x]
                [neg? (negative? x)]
                [x (abs x)]
                [M+E
                 (or (and (exact? x) (exact->scaled-integer x #t))
                     (inexact->scaled-integer (exact->inexact x)))])
           ;; x = M*10^-E
           (let* ([M (car M+E)]
                  [E (cdr M+E)]
                  ;; round E up to multiple of NBASE_MAGN
                  [E* (round-up-to E NBASE_MAGN)]
                  [M* (* M (expt 10 (- E* E)))]
                  [bigits (nat->bigits M*)]
                  ;;    E* = #bigits - weight - 1
                  ;; so weight = #bigits - E* - 1
                  [bigit-count (length bigits)]
                  [weight (- bigit-count (quotient E* NBASE_MAGN) 1)]
                  [dscale (max 0 E)])
             (make-numeric bigit-count weight neg? dscale bigits)))]
        [(eqv? x +nan.0)
         (let ([NUMERIC_NAN #xC000])
           (bytes-append (mkint2 0)
                         (mkint2 0)
                         (mkint2 NUMERIC_NAN #f)
                         (mkint2 0)))]
        [else
         (send-error f "numeric" x #:contract '(or/c rational? +nan.0))]))

(define (send-array elttype)
  ;; NOTE: elttype must have binary writer
  (define writer (typeid->type-writer elttype))
  (lambda (f x0)
    (define x
      (cond [(pg-array? x0) x0]
            [(list? x0) (list->pg-array x0)]
            [else (send-error f "pg-array" x0 #:contract '(or/c list? pg-array?))]))
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
                           (let ([b (writer f vals)])
                             (write-bytes (integer->integer-bytes (bytes-length b) 4 #t #t) out)
                             (write-bytes b out))])]
                   [else
                    (for ([v (in-vector vals)])
                      (loop (sub1 n) v))])))
         (get-output-bytes out))])))

(define (send-json f x)
  (unless (jsexpr? x)
    (send-error f "json" x #:contract 'jsexpr?))
  (jsexpr->bytes x))

(define (send-range elttype)
  (define EMPTY  #x01)
  (define LB_INC #x02)
  (define UB_INC #x04)
  (define LB_INF #x08)
  (define UB_INF #x10)
  (define writer (typeid->type-writer elttype))
  (lambda (f x)
    (match x
      [(pg-range lb includes-lb? ub includes-ub?)
       (let* ([flags (+ (if lb 0 LB_INF)
                        (if ub 0 UB_INF)
                        (if includes-lb? LB_INC 0)
                        (if includes-ub? UB_INC 0))]
              [lb-bytes (and lb (writer f lb))]
              [ub-bytes (and ub (writer f ub))])
         (bytes-append (bytes flags)
                       (if lb-bytes
                           (integer->integer-bytes (bytes-length lb-bytes) 4 #t #t)
                           #"")
                       (or lb-bytes #"")
                       (if ub-bytes
                           (integer->integer-bytes (bytes-length ub-bytes) 4 #t #t)
                           #"")
                       (or ub-bytes #"")))]
      [(pg-empty-range)
       (bytes 1)])))

;; send-error : string datum -> (raises error)
(define (send-error f type datum #:contract [ctc #f])
  (error/no-convert f "PostgreSQL" type datum #:contract ctc))
