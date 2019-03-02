#lang racket/base

;; Roughly based on the PLaneT package by Dave Herman,
;;   Originally released under MIT license.

;; edited:
;; -- Matthias, organization in preparation for pretty-print
;; -- Matthias, contracts 

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

;; racket/contract must come before provide
(require syntax/readerr
         racket/contract)

;; -----------------------------------------------------------------------------
;; SERVICES

(provide
 ;; Parameter 
 json-null ;; Parameter 
 
 ;; Any -> Boolean 
 jsexpr?
 
 (contract-out
  [write-json
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (output-port? ;; (current-output-port)
         #:null any/c ;; (json-null)
         #:encode (or/c 'control 'all)) ;; 'control
        any)]
  [read-json
   (->* ()
        (input-port? #:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  [jsexpr->string
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (#:null any/c ;; (json-null)
         #:encode (or/c 'control 'all)) ;; 'control
        any)] ;; string?
  [jsexpr->bytes
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (#:null any/c ;; (json-null)
         #:encode (or/c 'control 'all)) ;; 'control
        any)] ;; bytes?
  [string->jsexpr
   (->* (string?)
        (#:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  [bytes->jsexpr
   (->* (bytes?)
        (#:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  ))

;; -----------------------------------------------------------------------------
;; CUSTOMIZATION

;; The default translation for a JSON `null' value
(define json-null (make-parameter 'null))

;; -----------------------------------------------------------------------------
;; PREDICATE

(define (jsexpr? x #:null [jsnull (json-null)])
  (let loop ([x x])
    (or (exact-integer? x)
        (inexact-rational? x)
        (boolean? x)
        (string? x)
        (eq? x jsnull)
        (and (list? x) (andmap loop x))
        (and (hash? x) (for/and ([(k v) (in-hash x)])
                         (and (symbol? k) (loop v)))))))

(define (inexact-rational? x) ; not nan or inf
  (and (inexact-real? x) (rational? x)))

;; -----------------------------------------------------------------------------
;; GENERATION  (from Racket to JSON)

(define (write-json x [o (current-output-port)]
                    #:null [jsnull (json-null)] #:encode [enc 'control])
  (write-json* 'write-json x o jsnull enc))

(define (write-json* who x o jsnull enc)
  (define (escape m)
    (define ch (string-ref m 0))
    (case ch
      [(#\backspace) "\\b"]
      [(#\newline) "\\n"]
      [(#\return) "\\r"]
      [(#\page) "\\f"]
      [(#\tab) "\\t"]
      [(#\\) "\\\\"]
      [(#\") "\\\""]
      [else 
       (define (u-esc n)
         (define str (number->string n 16))
         (define pad (case (string-length str)
                       [(1) "000"] [(2) "00"] [(3) "0"] [else ""]))
         (string-append "\\u" pad str))
       (define n
         (char->integer ch))
       (if (n . < . #x10000)
           (u-esc n)
           ;; use the (utf-16 surrogate pair) double \u-encoding
           (let ([n (- n #x10000)])
             (string-append (u-esc (+ #xD800 (arithmetic-shift n -10)))
                            (u-esc (+ #xDC00 (bitwise-and n #x3FF))))))]))
  (define rx-to-encode
    (case enc
      ;; FIXME: This should also encode (always) anything that is represented
      ;; with a \U in Racket (since the json thing should be two \u sequences,
      ;; so there should never be a \U in the output of this function); but I
      ;; don't know if there's a known specification to what gets a \U
      [(control) #rx"[\0-\37\\\"\177]"]
      [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]
      [else (raise-type-error who "encoding symbol" enc)]))
  (define (write-json-string str)
    (write-bytes #"\"" o)
    (write-string (regexp-replace* rx-to-encode str escape) o)
    (write-bytes #"\"" o))
  (let loop ([x x])
    (cond [(or (exact-integer? x) (inexact-rational? x)) (write x o)]
          [(eq? x #f)     (write-bytes #"false" o)]
          [(eq? x #t)     (write-bytes #"true" o)]
          [(eq? x jsnull) (write-bytes #"null" o)]
          [(string? x) (write-json-string x)]
          [(list? x)
           (write-bytes #"[" o)
           (when (pair? x)
             (loop (car x))
             (for ([x (in-list (cdr x))]) (write-bytes #"," o) (loop x)))
           (write-bytes #"]" o)]
          [(hash? x)
           (write-bytes #"{" o)
           (define first? #t)
           (for ([(k v) (in-hash x)])
             (unless (symbol? k)
               (raise-type-error who "legal JSON key value" k))
             (if first? (set! first? #f) (write-bytes #"," o))
             ;; use a string encoding so we get the same deal with
             ;; `rx-to-encode'
             (write-json-string (symbol->string k))
             (write-bytes #":" o)
             (loop v))
           (write-bytes #"}" o)]
          [else (raise-type-error who "legal JSON value" x)]))
  (void))

;; -----------------------------------------------------------------------------
;; PARSING (from JSON to Racket)

(define (read-json [i (current-input-port)] #:null [jsnull (json-null)])
  (read-json* 'read-json i jsnull))

(define (read-json* who i jsnull)
  ;; Follows the specification (eg, at json.org) -- no extensions.
  ;;
  (define (err fmt . args)
    (define-values [l c p] (port-next-location i))
    (raise-read-error (format "~a: ~a" who (apply format fmt args))
                      (object-name i) l c p #f))
  (define (skip-whitespace)
    (define ch (peek-char i))
    (cond
      [(and (char? ch) (char-whitespace? ch))
       (read-char i)
       (skip-whitespace)]
      [else ch]))
  (define (byte-char=? b ch)
    (eqv? b (char->integer ch)))
  ;;
  ;; Reading a string *could* have been nearly trivial using the racket
  ;; reader, except that it won't handle a "\/"...
  (define (read-a-string)
    ;; Using a string output port would make sense here, but managing
    ;; a string buffer directly is even faster
    (define result (make-string 16))
    (define (keep-char c old-result pos converter)
      (define result
        (cond
          [(= pos (string-length old-result))
           (define new (make-string (* pos 2)))
           (string-copy! new 0 old-result 0 pos)
           new]
          [else old-result]))
      (string-set! result pos c)
      (loop result (add1 pos) converter))
    (define (loop result pos converter)
      (define c (read-byte i))
      (cond
        [(eof-object? c) (err "unterminated string")]
        [(byte-char=? c #\") (substring result 0 pos)]
        [(byte-char=? c #\\) (read-escape (read-char i) result pos converter)]
        [(c . < . 128) (keep-char (integer->char c) result pos converter)]
        [else
         ;; need to decode, but we can't un-read the byte, and
         ;; also we want to report decoding errors
         (define cvtr (or converter
                          (bytes-open-converter "UTF-8" "UTF-8")))
         (define buf (make-bytes 6 c))
         (let utf8-loop ([start 0] [end 1])
           (define-values (wrote-n read-n state) (bytes-convert cvtr buf start end buf 0 6))
           (case state
             [(complete)
              (keep-char (bytes-utf-8-ref buf 0) result pos cvtr)]
             [(error)
              (err "UTF-8 decoding error at ~e" (subbytes buf 0 end))]
             [(aborts)
              (define c (read-byte i))
              (cond
                [(eof-object? c)
                 (err "unexpected end-of-file")]
                [else
                 (bytes-set! buf end c)
                 (utf8-loop (+ start read-n) (add1 end))])]))]))
    (define (read-escape esc result pos converter)
      (cond
        [(case esc
           [(#\b) "\b"]
           [(#\n) "\n"]
           [(#\r) "\r"]
           [(#\f) "\f"]
           [(#\t) "\t"]
           [(#\\) "\\"]
           [(#\") "\""]
           [(#\/) "/"]
           [else #f])
         => (λ (s) (keep-char (string-ref s 0) result pos converter))]
        [(eqv? esc #\u)
         (define (get-hex)
           (define (read-next)
             (define c (read-byte i))
             (when (eof-object? c) (error "unexpected end-of-file"))
             c)
           (define c1 (read-next))
           (define c2 (read-next))
           (define c3 (read-next))
           (define c4 (read-next))
           (define (hex-convert c)
             (cond
               [(<= (char->integer #\0) c (char->integer #\9))
                (- c (char->integer #\0))]
               [(<= (char->integer #\a) c (char->integer #\f))
                (- c (- (char->integer #\a) 10))]
               [(<= (char->integer #\A) c (char->integer #\F))
                (- c (- (char->integer #\A) 10))]
               [else (err "bad \\u escape ~e" (bytes c1 c2 c3 c4))]))
           (+ (arithmetic-shift (hex-convert c1) 12)
              (arithmetic-shift (hex-convert c2) 8)
              (arithmetic-shift (hex-convert c3) 4)
              (hex-convert c4)))
         (define e (get-hex))
         (define e*
           (cond
             [(<= #xD800 e #xDFFF)
              (define (err-missing)
                (err "bad string \\u escape, missing second half of a UTF-16 pair"))
              (unless (eqv? (read-byte i) (char->integer #\\)) (err-missing))
              (unless (eqv? (read-byte i) (char->integer #\u)) (err-missing))
              (define e2 (get-hex))
              (cond
                [(<= #xDC00 e2 #xDFFF)
                 (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000)]
                [else
                 (err "bad string \\u escape, bad second half of a UTF-16 pair")])]
             [else e]))
         (keep-char (integer->char e*) result pos converter)]
        [else (err "bad string escape: \"~a\"" esc)]))
    (loop result 0 #f))
  ;;
  (define (read-list what end read-one)
    (define ch (skip-whitespace))
    (cond
      [(eqv? end ch) (read-byte i)
                     '()]
      [else
       (let loop ([l (list (read-one))])
         (define ch (skip-whitespace))
         (cond
           [(eqv? ch end) (read-byte i)
                          (reverse l)]
           [(eqv? ch #\,) (read-byte i)
                          (loop (cons (read-one) l))]
           [else (err "error while parsing a json ~a" what)]))]))
  ;;
  (define (read-hash)
    (define (read-pair)
      (define k (read-json))
      (unless (string? k) (err "non-string value used for json object key"))
      (define ch (skip-whitespace))
      (unless (char=? #\: ch)
        (err "error while parsing a json object pair"))
      (read-byte i)
      (cons (string->symbol k) (read-json)))
    (for/hasheq ([p (in-list (read-list 'object #\} read-pair))])
      (values (car p) (cdr p))))
  ;;
  (define (read-literal bstr)
    (define len (bytes-length bstr))
    (read-byte i)
    (for ([j (in-range 1 len)])
      (define c (read-byte i))
      (unless (eqv? c (bytes-ref bstr j))
        (bad-input (bytes-append (subbytes bstr 0 j) (bytes c)))))
    ;; Check for delimiter, defined for our purposes as matching #rx"\\b":
    (define b (peek-byte i))
    (unless (eof-object? b)
      (when (or (<= (char->integer #\a) b (char->integer #\z))
                (<= (char->integer #\A) b (char->integer #\Z))
                (<= (char->integer #\0) b (char->integer #\9))
                (eqv? b (char->integer #\_)))
        (bad-input bstr))))
  ;;
  (define (read-number ch)
    ;; match #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?"
    (define (start)
      (cond
        [(eqv? ch #\-)
         (read-byte i)
         (read-integer -1)]
        [else
         (read-integer 1)]))
    (define (digit-byte? c)
      (and (not (eof-object? c))
           (<= (char->integer #\0) c (char->integer #\9))))
    (define (to-number c)
      (- c (char->integer #\0)))
    (define (maybe-bytes c)
      (if (eof-object? c) #"" (bytes c)))
    ;; used to reconstruct input for error reporting:
    (define (n->string n exp)
      (define s (number->string n))
      (string->bytes/utf-8 
       (cond
         [(zero? exp) s]
         [else
          (define m (+ (string-length s) exp))
          (string-append (substring s 0 m) "." (substring s m))])))
    ;; need at least one digit:
    (define (read-integer sgn)
      (define c (read-byte i))
      (cond
        [(digit-byte? c)
         (read-integer-rest sgn (to-number c)
                            #:more-digits? (not (eqv? c (char->integer #\0))))]
        [else (bad-input (bytes-append (if (sgn . < . 0) #"-" #"")
                                       (maybe-bytes c))
                         #:eof? (eof-object? c))]))
    ;; more digits:
    (define (read-integer-rest sgn n #:more-digits? more-digits?)
      (define c (peek-byte i))
      (cond
        [(and more-digits? (digit-byte? c))
         (read-byte i)
         (read-integer-rest sgn (+ (* n 10) (to-number c)) #:more-digits? #t)]
        [(eqv? c (char->integer #\.))
         (read-byte i)
         (read-fraction sgn n)]
        [(or (eqv? c (char->integer #\e))
             (eqv? c (char->integer #\E)))
         (read-byte i)
         (read-exponent (* sgn n) c 0)]
        [else (* sgn n)]))
    ;; need at least one digit:
    (define (read-fraction sgn n)
      (define c (read-byte i))
      (cond
        [(digit-byte? c)
         (read-fraction-rest sgn (+ (* n 10) (to-number c)) -1)]
        [else (bad-input (bytes-append (string->bytes/utf-8 (format "~a." (* sgn n)))
                                       (maybe-bytes c))
                         #:eof? (eof-object? c))]))
    ;; more digits:
    (define (read-fraction-rest sgn n exp)
      (define c (peek-byte i))
      (cond
        [(digit-byte? c)
         (read-byte i)
         (read-fraction-rest sgn (+ (* n 10) (to-number c)) (sub1 exp))]
        [(or (eqv? c (char->integer #\e))
             (eqv? c (char->integer #\E)))
         (read-byte i)
         (read-exponent (* sgn n) c exp)]
        [else (exact->inexact (* sgn n (expt 10 exp)))]))
    ;; need at least one digit, maybe after +/-:
    (define (read-exponent n mark exp)
      (define c (read-byte i))
      (cond
        [(digit-byte? c)
         (read-exponent-rest n exp (to-number c))]
        [(eqv? c (char->integer #\+))
         (read-exponent-more n mark #"+" exp 1)]
        [(eqv? c (char->integer #\-))
         (read-exponent-more n mark #"-" exp -1)]
        [else (bad-input (bytes-append (n->string n exp)
                                       (bytes mark)
                                       (maybe-bytes c))
                         #:eof? (eof-object? c))]))
    ;; need at least one digit, still:
    (define (read-exponent-more n mark mark2 exp sgn) 
      (define c (read-byte i))
      (cond
        [(digit-byte? c)
         (read-exponent-rest n exp (* sgn (to-number c)))]
        [else (bad-input (bytes-append (n->string n exp)
                                       (bytes mark)
                                       mark2
                                       (maybe-bytes c))
                         #:eof? (eof-object? c))]))
    ;; more digits:
    (define (read-exponent-rest n exp exp2)
      (define c (peek-byte i))
      (cond
        [(digit-byte? c)
         (read-byte i)
         (read-exponent-rest n exp (+ (* 10 exp2) (to-number c)))]
        [else (exact->inexact (* n (expt 10 (+ exp exp2))))]))
    (start))
  ;;
  (define (read-json [top? #f])
    (define ch (skip-whitespace))
    (cond
      [(eof-object? ch)
       (if top?
           eof
           (bad-input))]
      [(eqv? ch #\t) (read-literal #"true") #t]
      [(eqv? ch #\f) (read-literal #"false") #f]
      [(eqv? ch #\n) (read-literal #"null") jsnull]
      [(or (and ((char->integer ch) . <= . (char->integer #\9))
                ((char->integer ch) . >= . (char->integer #\0)))
           (eqv? ch #\-))
       (read-number ch)]
      [(eqv? ch #\") (read-byte i)
                     (read-a-string)]
      [(eqv? ch #\[) (read-byte i)
                     (read-list 'array #\] read-json)]
      [(eqv? ch #\{) (read-byte i)
                     (read-hash)]
      [else (bad-input)]))
  ;;
  (define (bad-input [prefix #""] #:eof? [eof? #f])
    (define bstr (peek-bytes (sub1 (error-print-width)) 0 i))
    (if (or (and (eof-object? bstr) (equal? prefix #""))
            eof?)
        (err (string-append "unexpected end-of-file"
                            (if (equal? prefix #"")
                                ""
                                (format "after ~e" prefix))))
        (err (format "bad input starting ~e" (bytes-append prefix (if (eof-object? bstr)
                                                                      #""
                                                                      bstr))))))
  ;;
  (read-json #t))

;; -----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS

(define (jsexpr->string x #:null [jsnull (json-null)] #:encode [enc 'control])
  (define o (open-output-string))
  (write-json* 'jsexpr->string x o jsnull enc)
  (get-output-string o))

(define (jsexpr->bytes x #:null [jsnull (json-null)] #:encode [enc 'control])
  (define o (open-output-bytes))
  (write-json* 'jsexpr->bytes x o jsnull enc)
  (get-output-bytes o))

(define (string->jsexpr str #:null [jsnull (json-null)])
  ;; str is protected by contract
  (read-json* 'string->jsexpr (open-input-string str) jsnull))

(define (bytes->jsexpr bs #:null [jsnull (json-null)])
  ;; bs is protected by contract
  (read-json* 'bytes->jsexpr (open-input-bytes bs) jsnull))
