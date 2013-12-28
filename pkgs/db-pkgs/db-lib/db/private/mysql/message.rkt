#|
Based on protocol documentation here:
  http://forge.mysql.com/wiki/MySQL_Internals_ClientServer_Protocol
|#

#lang racket/base
(require racket/match
         racket/port
         db/private/generic/sql-data
         db/private/generic/interfaces
         "../../util/private/geometry.rkt")
(provide write-packet
         parse-packet

         packet?
         (struct-out handshake-packet)
         (struct-out change-plugin-packet)
         (struct-out client-auth-packet)
         (struct-out abbrev-client-auth-packet)
         (struct-out auth-followup-packet)
         (struct-out command-packet)
         (struct-out command:statement-packet)
         (struct-out command:change-user-packet)
         (struct-out ok-packet)
         (struct-out error-packet)
         (struct-out result-set-header-packet)
         (struct-out field-packet)
         (struct-out eof-packet)
         (struct-out row-data-packet)
         (struct-out binary-row-data-packet)
         (struct-out ok-prepared-statement-packet)
         (struct-out long-data-packet)
         (struct-out execute-packet)
         (struct-out fetch-packet)
         (struct-out unknown-packet)

         supported-result-typeid?
         parse-field-dvec
         field-dvec->name
         field-dvec->typeid
         field-dvec->flags
         field-dvec->field-info)

;; MAX-PACKET-SIZE is the largest packet that the MySQL protocol can
;; transmit. Longer data row "packets" can be split into multiple actual
;; packets (see comments on data rows, below).
(define MAX-PACKET-SIZE (sub1 (expt 2 24)))

#|
Note: There is also a max_allowed_packet server variable (and client connection
variable???) that controls things like how long a data row can be and how long a
computed string on the server can be. See also:

  - http://bugs.mysql.com/bug.php?id=22853
    (note, the "workaround" didn't work for me; data was still truncated)
  - http://www.perlmonks.org/?node_id=150255
|#

;; WRITING FUNCTIONS

(define (io:write-byte port byte)
  (write-byte byte port))

(define (io:write-bytes port bytes)
  (write-bytes bytes port))

(define (io:write-null-terminated-bytes port bytes)
  (write-bytes bytes port)
  (write-byte 0 port))

(define (io:write-null-terminated-string port string)
  (write-string string port)
  (write-byte 0 port))

(define (io:write-le-int16 port n [signed? #f])
  (write-bytes (integer->integer-bytes n 2 signed? #f) port))

(define (io:write-le-int24 port n)
  (write-bytes (subbytes (integer->integer-bytes n 4 #f #f) 0 3)
                       port))

(define (io:write-le-int32 port n [signed? #f])
  (write-bytes (integer->integer-bytes n 4 signed? #f) port))

(define (io:write-le-int64 port n [signed? #f])
  (write-bytes (integer->integer-bytes n 8 signed? #f) port))

(define (io:write-le-intN port count n)
  (let loop ([count count] [n n])
    (when (positive? count)
      (io:write-byte port (bitwise-and #xFF n))
      (loop (sub1 count) (arithmetic-shift n -8)))))

(define (io:write-length-code port n)
  (cond [(<= n 250) (io:write-byte port n)]
        [(<= n #xFFFF)
         (io:write-byte port 252)
         (io:write-le-int16 port n)]
        [(<= n #xFFFFFF)
         (io:write-byte port 253)
         (io:write-le-int24 port n)]
        [(<= n #xFFFFFFFF)
         (io:write-byte port 253)
         (io:write-le-int32 port n)]
        [else
         (io:write-byte port 254)
         (io:write-le-int64 port n)]))

(define (io:write-length-coded-bytes port b)
  (io:write-length-code port (bytes-length b))
  (io:write-bytes port b))

(define (io:write-length-coded-string port s)
  (io:write-length-coded-bytes port (string->bytes/utf-8 s)))

;; READING

(define (io:read-null-terminated-bytes port)
  (let [(strport (open-output-bytes))]
    (let loop ()
      (let ([next (read-byte port)])
        (cond [(eof-object? next)
               (error/comm 'io:read-null-terminated-bytes "(unexpected EOF)")]
              [(zero? next)
               (get-output-bytes strport)]
              [else
               (write-byte next strport)
               (loop)])))))

(define (io:read-null-terminated-string port)
  (bytes->string/utf-8 (io:read-null-terminated-bytes port)))

(define (io:read-byte port)
  (read-byte port))

(define (io:read-bytes-as-bytes port n)
  (read-bytes n port))

(define (io:read-bytes-as-string port n)
  (bytes->string/utf-8 (read-bytes n port)))

(define (io:read-le-int16 port [signed? #f])
  (integer-bytes->integer (read-bytes 2 port) signed? #f))

(define (io:read-le-int24 port)
  (io:read-le-intN port 3))

(define (io:read-le-int32 port [signed? #f])
  (integer-bytes->integer (read-bytes 4 port) signed? #f))

(define (io:read-le-int64 port [signed? #f])
  (integer-bytes->integer (read-bytes 8 port) signed? #f))

(define (io:read-le-intN port count)
  (case count
    ((2) (io:read-le-int16 port))
    ((4) (io:read-le-int32 port))
    (else
     (let ([b (read-bytes count port)])
       (unless (and (bytes? b) (= count (bytes-length b)))
         (error/internal 'io:read-le-intN "unexpected eof; got ~s" b))
       (let loop ([pos 0])
         (if (< pos count)
             (+ (arithmetic-shift (loop (add1 pos)) 8)
                (bytes-ref b pos))
             0))))))

(define (io:read-length-code port)
  (let ([first (read-byte port)])
    (cond [(<= first 250)
           first]
          [(= first 251)
           ;; Indicates NULL record
           #f]
          [(= first 252)
           (io:read-le-int16 port)]
          [(= first 253)
           (io:read-le-int24 port)]
          [(= first 254)
           (io:read-le-intN port 8)])))

(define (io:read-length-coded-bytes port)
  (let ([len (io:read-length-code port)])
    (and len (read-bytes len port))))

(define (io:read-length-coded-string port)
  (let ([b (io:read-length-coded-bytes port)])
    (and b (bytes->string/utf-8 b))))

(define (io:read-bytes-to-eof port)
  (let loop ([acc null])
    (let ([next (read-bytes 1024 port)])
      (if (eof-object? next)
          (apply bytes-append (reverse acc))
          (loop (cons next acc))))))

;; ========================================

(define-struct packet () #:transparent)

(define-struct (handshake-packet packet)
  (protocol-version
   server-version
   thread-id
   scramble
   server-capabilities
   charset
   server-status
   auth)
  #:transparent)

(define-struct (client-auth-packet packet)
  (client-flags
   max-packet-length
   charset
   user
   scramble
   database
   plugin)
  #:transparent)

(define-struct (abbrev-client-auth-packet packet)
  (client-flags)
  #:transparent)

(define-struct (auth-followup-packet packet)
  (data)
  #:transparent)

(define-struct (command-packet packet)
  (command
   argument)
  #:transparent)

(define-struct (command:statement-packet packet)
  (command
   argument)
  #:transparent)

(define-struct (command:change-user-packet packet)
  (user
   password
   database
   charset)
  #:transparent)

(define-struct (ok-packet packet)
  (affected-rows
   insert-id
   server-status
   warning-count
   message)
  #:transparent)

(define-struct (error-packet packet)
  (errno
   sqlstate
   message)
  #:transparent)

(define-struct (result-set-header-packet packet)
  (field-count
   extra)
  #:transparent)

(define-struct (field-packet packet)
  (catalog
   db
   table
   org-table
   name
   org-name
   charset
   length
   type
   flags
   decimals
   default)
  #:transparent)

(define-struct (eof-packet packet)
  (warning-count
   server-status)
  #:transparent)

(define-struct (row-data-packet packet)
  (data)
  #:transparent)

(define-struct (binary-row-data-packet packet)
  (data)
  #:transparent)

(define-struct (ok-prepared-statement-packet packet)
  (statement-handler-id
   result-count
   parameter-count)
  #:transparent)

(define-struct (long-data-packet packet)
  (statement-id
   parameter-number
   data)
  #:transparent)

(define-struct (execute-packet packet)
  (statement-id
   flags
   null-map
   params)
  #:transparent)

(define-struct (fetch-packet packet)
  (statement-id
   count)
  #:transparent)

(define-struct (change-plugin-packet packet)
  (plugin
   data)
  #:transparent)

(define-struct (unknown-packet packet)
  (expected
   contents)
  #:transparent)

(define (write-packet out p number)
  (let ([o (open-output-bytes)])
    (write-packet* o p)
    (let ([b (get-output-bytes o)])
      #| (printf "writing packet #~s, length ~s\n" number (bytes-length b)) |#
      (io:write-le-int24 out (bytes-length b))
      (io:write-byte out number)
      (io:write-bytes out b))))

(define (write-packet* out p)
  (match p
    [(struct abbrev-client-auth-packet (client-flags))
     (io:write-le-int32 out (encode-server-flags client-flags))]
    [(struct client-auth-packet (client-flags max-length charset user scramble database plugin))
     (io:write-le-int32 out (encode-server-flags client-flags))
     (io:write-le-int32 out max-length)
     (io:write-byte out (encode-charset charset))
     (io:write-bytes out (make-bytes 23 0))
     (io:write-null-terminated-string out user)
     (cond [(memq 'secure-connection client-flags)
            (io:write-length-coded-bytes out (or scramble #""))]
           [else ;; old-style scramble is *not* length-coded, but \0-terminated
            (io:write-bytes out (or scramble (bytes 0)))])
     (when (memq 'connect-with-db client-flags)
       (io:write-null-terminated-string out database))
     (when (memq 'plugin-auth client-flags)
       (io:write-null-terminated-string out plugin))]
    [(struct auth-followup-packet (data))
     (io:write-bytes out data)]
    [(struct command-packet (command arg))
     (io:write-byte out (encode-command command))
     (io:write-null-terminated-bytes out (string->bytes/utf-8 arg))]
    [(struct command:statement-packet (command arg))
     (io:write-byte out (encode-command command))
     (io:write-le-int32 out arg)]
    [(struct long-data-packet (statement-id parameter-number data))
     (io:write-byte out (encode-command 'statement-send-long-data))
     (io:write-le-int32 out statement-id)
     (io:write-le-int16 out parameter-number)
     (io:write-bytes out data)]
    [(struct execute-packet
             (statement-id flags null-map params))
     (io:write-byte out (encode-command 'statement-execute))
     (io:write-le-int32 out statement-id)
     (io:write-byte out (encode-execute-flags flags))
     (io:write-le-int32 out 1) ;; iterations = 1
     (io:write-le-intN out
                       (null-map-length null-map)
                       (null-map->integer null-map))
     (io:write-byte out 1) ;; first? = 1
     (let ([param-types (map choose-param-type params)])
       (for-each (lambda (pt) (io:write-le-int16 out (encode-type pt)))
                 param-types)
       (for-each (lambda (type param)
                   (cond [(sql-null? param) (void)]
                         [(eq? param 'long-string) (void)]
                         [(eq? param 'long-binary) (void)]
                         [else (write-binary-datum out type param)]))
                 param-types params))]
    [(struct fetch-packet (statement-id count))
     (io:write-byte out (encode-command 'statement-fetch))
     (io:write-le-int32 out statement-id)
     (io:write-le-int32 out count)]))


;; ----

;; parse-packet : ... -> (values nat packet)
;; In case of binary data row split over multiple packets, reads all packets for
;; the data row, returns packet number of last packet as first return value.
(define (parse-packet in expect field-dvecs)
  (let* ([len (io:read-le-int24 in)]
         [num (io:read-byte in)]
         [bs (read-bytes len in)]
         [inp (open-input-bytes bs)])
    (let-values ([(msg-num msg) (parse-packet/1 num in inp expect len field-dvecs)])
      (when (and (not (port-closed? inp)) (port-has-bytes? inp))
        (error/internal* 'parse-packet "bytes left over after parsing packet"
                         '("packet" value) msg
                         '("leftover" value) (io:read-bytes-to-eof inp)))
      (close-input-port inp)
      (values num msg))))

(define (port-has-bytes? p)
  (not (eof-object? (peek-byte p))))

(define (parse-packet/1 msg-num real-in in expect len field-dvecs)
  (let ([first (peek-byte in)])
    (if (eq? first #xFF)
        (values msg-num (parse-error-packet in len))
        (parse-packet/2 msg-num real-in in expect len field-dvecs))))

(define (parse-packet/2 msg-num real-in in expect len field-dvecs)
  (case expect
    ((binary-data)
     (if (and (eq? (peek-byte in) #xFE) (< len 9))
         (values msg-num (parse-eof-packet in len))
         (parse-binary-row-data-packet msg-num real-in in len field-dvecs)))
    (else
     (values msg-num
             (case expect
               ((handshake)
                (parse-handshake-packet in len))
               ((auth)
                (case (peek-byte in)
                  ((#x00) (parse-ok-packet in len))
                  ((#xFE) (parse-change-plugin-packet in len))
                  (else (parse-unknown-packet in len "(expected authentication ok packet)"))))
               ((ok)
                (case (peek-byte in)
                  ((#x00) (parse-ok-packet in len))
                  (else (parse-unknown-packet in len "(expected ok packet)"))))
               ((result)
                (case (peek-byte in)
                  ((#x00) (parse-ok-packet in len))
                  (else (parse-result-set-header-packet in len))))
               ((field)
                (if (and (eq? (peek-byte in) #xFE) (< len 9))
                    (parse-eof-packet in len)
                    (parse-field-packet in len)))
               ((data)
                (if (and (eq? (peek-byte in) #xFE) (< len 9))
                    (parse-eof-packet in len)
                    (parse-row-data-packet in len)))
               ((prep-ok)
                (case (peek-byte in)
                  ((#x00) (parse-ok-prepared-statement-packet in len))
                  (else (parse-unknown-packet in len "(expected ok for prepared statement packet)"))))
               (else
                (error/comm 'parse-packet (format "(bad expected packet type: ~s)" expect))))))))

;; Individual parsers

(define (parse-unknown-packet in len expected)
  (make-unknown-packet expected (io:read-bytes-to-eof in)))

(define (parse-handshake-packet in len)
  (let* ([protocol-version (io:read-byte in)]
         [server-version (io:read-null-terminated-string in)]
         [thread-id (io:read-le-int32 in)]
         [scramble1 (io:read-bytes-as-bytes in 8)]
         [_ (io:read-byte in)] ;; always \0
         [server-capabilities-lo (io:read-le-int16 in)]
         [charset (decode-charset (io:read-byte in))]
         [server-status (io:read-le-int16 in)]
         [server-capabilities-hi (io:read-le-int16 in)]
         [scramble-len
          ;; total scramble size (both parts), including null terminator
          ;;  - in 5.1.58, this byte is always 0 (so adjust to 21)
          ;;  - in 5.5.12, usually 21 for mysql_native_password auth
          ;; always >= 20 bytes
          (let ([len (io:read-byte in)])
            (cond [(zero? len) 21]
                  [(>= len 21) len]
                  [else (error/comm 'parse-handshake-packet
                                    (format "(bad scramble length: ~s)" len))]))]
         [_ (io:read-bytes-as-bytes in 10)] ;; always \0
         [scramble2
          (let* (;; subtract 8 for earlier part, subtract 1 for null-terminator byte
                 [len (- scramble-len 8 1)]
                 [scramble2 (io:read-bytes-as-bytes in len)])
            (io:read-byte in) ;; always \0, at least for supported auth types
            scramble2)]
         [server-capabilities
          (decode-server-flags (+ server-capabilities-lo
                                  (arithmetic-shift server-capabilities-hi 16)))]
         [auth
          ;; IIUC, present iff (memq 'plugin-auth server-capabilities)
          ;; (alternative: do peek-byte, test for eof)
          ;;  - in 5.1.58, absent
          ;;  - in 5.5.12, a null-terminated auth string
          (cond [(memq 'plugin-auth server-capabilities)
                 (io:read-null-terminated-string in)]
                [else #f])]) ;; implicit "mysql_native_password"
    (make-handshake-packet protocol-version
                           server-version
                           thread-id
                           (bytes-append scramble1 scramble2)
                           server-capabilities
                           charset
                           server-status
                           auth)))

(define (parse-ok-packet in len)
  (let* ([_ (io:read-byte in)]
         [affected-rows (io:read-length-code in)]
         [insert-id (io:read-length-code in)]
         [server-status (io:read-le-int16 in)]
         [warning-count (io:read-le-int16 in)]
         [message (io:read-bytes-to-eof in)])
    (make-ok-packet affected-rows
                    insert-id
                    server-status
                    warning-count
                    (bytes->string/utf-8 message))))

(define (parse-change-plugin-packet in len)
  (let* ([_ (io:read-byte in)]
         [plugin (and (port-has-bytes? in)
                      (io:read-null-terminated-string in))]
         [data (and (port-has-bytes? in)
                    (io:read-bytes-to-eof in))])
    ;; If plugin = #f, then changing to old password plugin.
    (make-change-plugin-packet plugin data)))

(define (parse-error-packet in len)
  (let* ([_ (io:read-byte in)]
         [errno (io:read-le-int16 in)]
         [marker (peek-char in)]
         [sqlstate
          (and (eq? marker #\#)
               (begin (io:read-byte in)
                      (io:read-bytes-as-string in 5)))]
         [message (io:read-bytes-to-eof in)])
    (make-error-packet errno
                       sqlstate
                       (bytes->string/utf-8 message))))

(define (parse-result-set-header-packet in len)
  (let* ([field-count (io:read-length-code in)]
         [extra (and (port-has-bytes? in)
                     (io:read-length-code in))])
    (make-result-set-header-packet field-count extra)))

(define (parse-field-packet in len)
  (let* ([catalog (io:read-length-coded-string in)]
         [db (io:read-length-coded-string in)]
         [table (io:read-length-coded-string in)]
         [org-table (io:read-length-coded-string in)]
         [name (io:read-length-coded-string in)]
         [org-name (io:read-length-coded-string in)]
         [_ (io:read-byte in)]
         [charset (io:read-le-int16 in)]
         [len (io:read-le-int32 in)]
         [type (io:read-byte in)]
         [flags (io:read-le-int16 in)]
         [decimals (io:read-byte in)]
         [_ (io:read-bytes-as-bytes in 2)]
         [default (and (port-has-bytes? in)  (io:read-length-code in))])
    (make-field-packet catalog
                       db
                       table
                       org-table
                       name
                       org-name
                       charset
                       len
                       (decode-type type)
                       (decode-field-flags flags)
                       decimals
                       default)))

(define (parse-eof-packet in len)
  (let* ([_ (io:read-byte in)]
         [warnings (io:read-le-int16 in)]
         [status (io:read-le-int16 in)])
    (make-eof-packet warnings status)))

(define (parse-row-data-packet in len)
  (make-row-data-packet
   (list->vector
    (let loop ()
      (if (at-eof? in)
          null
          (let* ([datum (io:read-length-coded-string in)])
            (cons (or datum sql-null)
                  (loop))))))))

(define (parse-ok-prepared-statement-packet in len)
  (let* ([ok (io:read-byte in)]
         [statement-handler-id (io:read-le-int32 in)]
         [columns (io:read-le-int16 in)]
         [params (io:read-le-int16 in)]
         [warnings (and (>= len 12) (io:read-le-int16 in))]
         [_ (io:read-bytes-to-eof in)])
    (unless (zero? ok)
      (error/comm 'parse-ok-prepared-statement-packet (format "(first byte was ~s)" ok)))
    (make-ok-prepared-statement-packet statement-handler-id columns params)))

(define (parse-binary-row-data-packet msg-num real-in in len field-dvecs)
  (let* ([first (io:read-byte in)] ;; SKIP? seems to be always zero
         [result-count (length field-dvecs)]
         [null-map-length (quotient (+ 9 result-count) 8)]
         [null-map (io:read-bytes-as-bytes in null-map-length)]
         [is-null? (lambda (i)
                     (let* ([i* (+ 2 i)] ;; skip first two bits
                            [bytei (quotient i* 8)]
                            [biti (remainder i* 8)])
                       (bitwise-bit-set? (bytes-ref null-map bytei)
                                         (if (system-big-endian?)
                                             (- 7 biti)
                                             biti))))]
         [field-v (make-vector result-count)])
    (let-values ([(msg-num* in*)
                  (let loop ([len len] [ins null])
                    ;; A data row is sent as zero or more packets of max length (2^24-1)
                    ;; followed by one packet of less than max length.
                    (cond [(= len MAX-PACKET-SIZE) ;; maximum packet length
                           (let* ([next-len (io:read-le-int24 real-in)]
                                  [next-num (io:read-byte real-in)]
                                  [next-bs (read-bytes next-len real-in)]
                                  [next-in (open-input-bytes next-bs)])
                             (loop next-len (cons next-in ins)))]
                          [else
                           (values (+ msg-num (length ins))
                                   (if (null? ins)
                                       in
                                       (apply input-port-append #t in (reverse ins))))]))])
      (for ([i (in-range result-count)]
            [field-dvec (in-list field-dvecs)])
        (vector-set! field-v i
                     (if (is-null? i)
                         sql-null
                         (read-binary-datum in* field-dvec))))
      (when (port-has-bytes? in*)
        (error/internal* 'parse-binary-row-data-packet "bytes left over after parsing packet"
                         '("leftover" value) (io:read-bytes-to-eof in*)))
      (close-input-port in*)
      (values msg-num* (make-binary-row-data-packet field-v)))))

(define (read-binary-datum in field-dvec)

  ;; How to distinguish between character data and binary data?
  ;; (Both are given type var-string.)
  ;; (Also true for blob vs text; both given as type blob.)

  ;; There seem to be two differences:
  ;;  1) character data has charset 33 (utf8_general_ci)
  ;;     binary data has charset 63 (binary)
  ;;  2) binary data has binary flag, character data does not

  ;; We'll try using #2.

  (define type (field-dvec->typeid field-dvec))
  (define flags (field-dvec->flags field-dvec))

  (case type

    ((tiny) (io:read-byte in)) ;; FIXME signed/unsigned
    ((short) (io:read-le-int16 in (not (memq 'unsigned flags))))
    ((int24) (io:read-le-int24 in)) ;; FIXME signed/unsigned
    ((long) (io:read-le-int32 in (not (memq 'unsigned flags))))
    ((longlong) (io:read-le-int64 in (not (memq 'unsigned flags))))
    ((varchar string var-string blob tiny-blob medium-blob long-blob)
     (if (memq 'binary flags)
         (io:read-length-coded-bytes in)
         (io:read-length-coded-string in)))

    ((float)
     (floating-point-bytes->real (io:read-bytes-as-bytes in 4) #f))
    ((double)
     (floating-point-bytes->real (io:read-bytes-as-bytes in 8) #f))

    ((date datetime timestamp newdate) ;; ???
     (let* ([bs (io:read-length-coded-bytes in)])
       ;; format is YYMDhmsUUUU (U = microseconds)
       ;; but trailing zeros can be dropped
       ;; (Apparently, docs lie; get microseconds, not nanoseconds)
       (define (get-int start len)
         (if (<= (+ start len) (bytes-length bs))
             (cond [(= len 1) (bytes-ref bs start)]
                   [else (integer-bytes->integer bs #t #f start (+ start len))])
             0))
       (let ([year  (get-int 0 2)]
             [month (get-int 2 1)]
             [day   (get-int 3 1)]
             [hour  (get-int 4 1)]
             [min   (get-int 5 1)]
             [sec   (get-int 6 1)]
             [nsec  (* 1000 (get-int 7 4))])
         (case type
           ((date newdate)
            (sql-date year month day))
           ((datetime timestamp)
            (sql-timestamp year month day hour min sec nsec #f))
           ((time)
            (sql-time hour min sec nsec #f))))))

    ((time)
     (let* ([bs (io:read-length-coded-bytes in)])
       (define (get-int start len)
         (if (<= (+ start len) (bytes-length bs))
             (cond [(= len 1) (bytes-ref bs start)]
                   [else (integer-bytes->integer bs #t #f start (+ start len))])
             0))
       ;; format is gDDDDhmsUUUU (g = sign, 0=pos, 1=neg; U = microseconds)
       ;; (Apparently, docs lie; get microseconds, not nanoseconds)
       (let* ([sg   (if (zero? (get-int 0 1)) + -)]
              [days (sg (get-int 1 4))]
              [hour (sg (get-int 5 1))]
              [min  (sg (get-int 6 1))]
              [sec  (sg (get-int 7 1))]
              [nsec (* 1000 (sg (get-int 8 4)))])
         (let ([iv (sql-interval 0 0 days hour min sec nsec)])
           (sql-interval->sql-time iv iv)))))

    ((year) (io:read-le-int16 in))

    ((newdecimal)
     (parse-decimal (io:read-length-coded-string in)))

    ((bit)
     (let ([l (field-dvec->length field-dvec)]
           [bv (io:read-length-coded-bytes in)])
       (make-sql-bits/bytes l bv (- 8 (modulo l 8)))))

    ((geometry)
     (bytes->geometry 'mysql-bytes->geometry
                      (io:read-length-coded-bytes in)
                      #:srid? #t))

    ((decimal)
     (error/internal* 'get-result "unimplemented decimal type" "type" type))
    ((enum set)
     (error/internal 'get-result "unimplemented type" "type" type))
    (else
     (error/internal 'get-result "unknown type" "type" type))))

(define (parse-decimal s)
  (cond [(equal? s "NaN") +nan.0]
        [(regexp-match #rx"^-?([0-9]*)$" s)
         ;; big integer
         => (lambda (m)
              (string->number s))]
        [(regexp-match #rx"^(-)?([0-9]*)\\.([0-9]*)$" s)
         => (lambda (m)
              (* (if (cadr m) -1 1)
                 (+ (string->number (caddr m))
                    (parse-exact-fraction (cadddr m)))))]
        [else
         (error/internal* 'parse-decimal "cannot parse as decimal"
                          '("string" value) s)]))

(define (parse-exact-fraction s)
  ;; eg: (parse-exact-fraction "12") = 12/100
  (/ (string->number s)
     (expt 10 (string-length s))))

(define (supported-result-typeid? typeid)
  (case typeid
    ((tiny short int24 long longlong float double) #t)
    ((varchar string var-string blob tiny-blob medium-blob long-blob) #t)
    ((date datetime timestamp newdate time year) #t)
    ((newdecimal bit geometry) #t)
    ((null) #t)
    (else #f)))

(define (choose-param-type param)
  (cond [(or (string? param)
             (sql-null? param)
             (eq? param 'long-string))
         'var-string]
        [(int64? param)
         'longlong]
        [(rational? param)
         'double]
        [(sql-date? param)
         'date]
        [(sql-timestamp? param)
         'timestamp]
        [(or (sql-time? param) (sql-day-time-interval? param))
         'time]
        [(or (bytes? param)
             (eq? param 'long-binary))
         'blob]
        [(sql-bits? param)
         'bit]
        [(geometry2d? param)
         'geometry]
        [else
         (error/internal* 'choose-param-type "bad parameter value"
                          '("value" value) param)]))

(define (write-binary-datum out type param)
  (case type
    ((var-string)
     (io:write-length-coded-string out param))
    ((longlong) (io:write-le-int64 out param #t))
    ((double)
     (io:write-bytes out (real->floating-point-bytes (exact->inexact param) 8)))
    ((date)
     (let ([bs (bytes-append (integer->integer-bytes (sql-date-year param) 2 #t #f)
                             (bytes (sql-date-month param))
                             (bytes (sql-date-day param)))])
       (io:write-length-coded-bytes out bs)))
    ((timestamp)
     (let ([bs (bytes-append (integer->integer-bytes (sql-timestamp-year param) 2 #t #f)
                             (bytes (sql-timestamp-month param))
                             (bytes (sql-timestamp-day param))
                             (bytes (sql-timestamp-hour param))
                             (bytes (sql-timestamp-minute param))
                             (bytes (sql-timestamp-second param))
                             (integer->integer-bytes
                              (quotient (sql-timestamp-nanosecond param) 1000)
                              4 #t #f))])
       (io:write-length-coded-bytes out bs)))
    ((time)
     (let* ([param (if (sql-time? param) (sql-time->sql-interval param) param)]
            [days (sql-interval-days param)]
            [hours (sql-interval-hours param)]
            [minutes (sql-interval-minutes param)]
            [seconds (sql-interval-seconds param)]
            [nanoseconds (sql-interval-nanoseconds param)]
            [neg? (ormap negative? (list days hours minutes seconds nanoseconds))]
            [bs (bytes-append (bytes (if neg? 1 0))
                              (integer->integer-bytes (abs days) 4 #t #f)
                              (bytes (abs hours))
                              (bytes (abs minutes))
                              (bytes (abs seconds))
                              (integer->integer-bytes
                               (quotient (abs nanoseconds) 1000)
                               4 #t #f))])
       (io:write-length-coded-bytes out bs)))
    ((blob)
     (io:write-length-coded-bytes out param))
    ((bit)
     (let-values ([(len bv start) (align-sql-bits param 'right)])
       (io:write-length-code out (- (bytes-length bv) start))
       (write-bytes bv out start)))

    ((geometry)
     (io:write-length-coded-bytes
      out
      (geometry->bytes 'mysql-geometry->bytes param #:big-endian? #f #:srid? #t)))))

;; ----

(define (fetch key table function)
  (let ([val (assq key table)])
    (if val
        (cdr val)
        (error/internal* function "not found" '("key" value) key))))

(define (encode-flags flags table function)
  (apply bitwise-ior
         (map (lambda (f) (fetch f table function))
              flags)))

(define (decode-flags n table function)
  (let loop ([table table])
    (cond [(null? table)
           null]
          [(positive? (bitwise-and (caar table) n))
           (cons (cdar table) (loop (cdr table)))]
          [else
           (loop (cdr table))])))

(define (invert-alist alist)
  (map (lambda (p) (cons (cdr p) (car p))) alist))

(define server-flags/decoding
  '((#x1     . long-password)
    (#x2     . found-rows)
    (#x4     . long-flag)
    (#x8     . connect-with-db)
    (#x10    . no-schema)
    (#x20    . compress)
    (#x40    . odbc)
    (#x80    . local-files)
    (#x100   . ignore-space)
    (#x200   . protocol-41)
    (#x400   . interactive)
    (#x800   . ssl)
    (#x1000  . ignore-sigpipe)
    (#x2000  . transactions)
    (#x4000  . protocol-41-OLD)
    (#x8000  . secure-connection)
    (#x10000 . multi-statements)
    (#x20000 . multi-results)
    (#x40000 . ps-multi-statements) ;; ???
    (#x80000 . plugin-auth)))
(define server-flags/encoding
  (invert-alist server-flags/decoding))

(define server-status-flags/decoding
  '((#x1     . in-transaction)
    (#x2     . auto-commit)
    (64      . cursor-exists)
    (128     . last-row-sent)))

(define commands/decoding
  '((#x00 . sleep)
    (#x01 . quit)
    (#x02 . init-db)
    (#x03 . query)
    (#x04 . field-list)
    (#x05 . create-db)      ;; deprecated
    (#x06 . drop-db)        ;; deprecated
    (#x07 . refresh)
    (#x08 . shutdown)
    (#x09 . statistics)
    (#x0A . process-info)
    (#x0B . connect)
    (#x0C . process-kill)
    (#x0D . debug)
    (#x0E . ping)
    (#x0F . time)
    (#x10 . delayed-insert)
    (#x11 . change-user)
    (#x16 . statement-prepare)
    (#x17 . statement-execute)
    (#x18 . statement-send-long-data)
    (#x19 . statement-close)
    (#x1A . statement-reset)
    (#x1B . set-option)
    (#x1C . statement-fetch)))
(define commands/encoding
  (invert-alist commands/decoding))

(define types/decoding
  '((#x00 . decimal)
    (#x01 . tiny)
    (#x02 . short)
    (#x03 . long)
    (#x04 . float)
    (#x05 . double)
    (#x06 . null)
    (#x07 . timestamp)
    (#x08 . longlong)
    (#x09 . int24)
    (#x0A . date)
    (#x0B . time)
    (#x0C . datetime)
    (#x0D . year)
    (#x0E . newdate)
    (#x0F . varchar)
    (#x10 . bit)
    (#xF6 . newdecimal)
    (#xF7 . enum)
    (#xF8 . set)
    (#xF9 . tiny-blob)
    (#xFA . medium-blob)
    (#xFB . long-blob)
    (#xFC . blob)
    (#xFD . var-string)
    (#xFE . string)
    (#xFF . geometry)))
(define types/encoding
  (invert-alist types/decoding))

(define field-flags/decoding
  '((#x001 . not-null)
    (#x002 . primary-key)
    (#x004 . unique-key)
    (#x008 . multiple-key)
    (#x010 . blob)
    (#x020 . unsigned)
    (#x040 . zero-fill)
    (#x080 . binary)
    (#x100 . enum)
    (#x200 . auto-increment)
    (#x400 . timestamp)
    (#x800 . set)))
(define field-flags/encoding
  (invert-alist field-flags/decoding))

(define execute-flags/decoding
  '((#x0 . cursor/no-cursor)
    (#x1 . cursor/read-only)
    (#x2 . cursor/for-update)
    (#x4 . cursor/scrollable)))
(define execute-flags/encoding
  (invert-alist execute-flags/decoding))


(define (encode-server-flags flags)
  (encode-flags flags server-flags/encoding 'encode-server-flags))
(define (decode-server-flags n)
  (decode-flags n server-flags/decoding 'decode-server-flags))

(define (decode-server-status-flags n)
  (decode-flags n server-status-flags/decoding 'decode-server-status-flags))

(define (encode-field-flags flags)
  (encode-flags flags field-flags/encoding 'encode-field-flags))
(define (decode-field-flags n)
  (decode-flags n field-flags/decoding 'decode-field-flags))

(define (encode-charset charset)
  (case charset
    ((utf8-general-ci) 33)
    (else (error/internal* 'encode-charset "unknown charset" "charset" charset))))
(define (decode-charset n)
  (case n
    ((33) 'utf8-general-ci)
    (else 'unknown)))

(define (encode-type type)
  (fetch type types/encoding 'encode-type))
(define (decode-type type)
  (fetch type types/decoding 'decode-type))

(define (encode-command command)
  (fetch command commands/encoding 'encode-command))

(define (encode-execute-flags flags)
  (encode-flags flags execute-flags/encoding 'encode-execute-flags))
(define (decode-execute-flags n)
  (decode-flags n execute-flags/decoding 'decode-execute-flags))

;; null-map-length : (list-of boolean) -> integer
(define (null-map-length null-map)
  (ceiling (/ (length null-map) 8)))

;; null-map->integer : (list-of boolean) -> integer
;; Least significant bit represents first boolean in list, etc
(define (null-map->integer null-map)
  (cond [(null? null-map)
         0]
        [(car null-map)
         (+ 1 (arithmetic-shift (null-map->integer (cdr null-map)) 1))]
        [else
         (arithmetic-shift (null-map->integer (cdr null-map)) 1)]))

(define (at-eof? in)
  (eof-object? (peek-byte in)))

(define (parse-field-dvec fp)
  (match fp
    [(struct field-packet (cat db tab otab name oname _ len type flags _ _))
     (vector cat db tab otab name oname len type flags)]))

(define (field-dvec->typeid dvec)
  (vector-ref dvec 7))

(define (field-dvec->name dvec)
  (vector-ref dvec 4))

(define (field-dvec->flags dvec)
  (vector-ref dvec 8))

(define (field-dvec->length dvec)
  (vector-ref dvec 6))

(define (field-dvec->field-info dvec)
  (match dvec
    [(vector cat db tab otab name oname len type flags)
     `((catalog . ,cat)
       (database . ,db)
       (table . ,tab)
       (original-table . ,otab)
       (name . ,name)
       (original-name . ,oname)
       (length . ,len)
       (typeid . ,type)
       (flags . ,flags))]))

(define (parse-field-info fp)
  (field-dvec->field-info (parse-field-dvec fp)))
