#lang racket/base

;; Roughly based on the PLaneT package by Dave Herman,
;;   Originally released under MIT license.

;; edited:
;; -- Matthias, organization in preparation for pretty-print
;; -- Matthias, contracts

;; -----------------------------------------------------------------------------
;; DEPENDENCIES

(require syntax/readerr
         racket/symbol
         ;; racket/contract must come before provide
         racket/contract/base)

;; tests in:
;; ~plt/pkgs/racket-test/tests/json/

;; docs in:
;; ~plt/pkgs/racket-doc/json/

;; -----------------------------------------------------------------------------
;; SERVICES

(provide
 ;; Parameter
 json-null

 ;; Any -> Boolean
 jsexpr?

 (contract-out
  [write-json
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (output-port? ;; (current-output-port)
         #:null any/c ;; (json-null)
         #:encode (or/c 'control 'all) ;; 'control
         #:indent (or/c #f #\tab natural-number/c)) ;; #f
        any)] ;; void?
  [read-json
   (->* ()
        (input-port?
         #:replace-malformed-surrogate? any/c
         #:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  [jsexpr->string
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (#:null any/c ;; (json-null)
         #:encode (or/c 'control 'all) ;; 'control
         #:indent (or/c #f #\tab natural-number/c)) ;; #f
        any)] ;; string?
  [jsexpr->bytes
   (->* (any/c) ;; jsexpr? but dependent on #:null arg
        (#:null any/c ;; (json-null)
         #:encode (or/c 'control 'all) ;; 'control
         #:indent (or/c #f #\tab natural-number/c)) ;; #f
        any)] ;; bytes?
  [string->jsexpr
   (->* (string?)
        (#:replace-malformed-surrogate? any/c
         #:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  [bytes->jsexpr
   (->* (bytes?)
        (#:replace-malformed-surrogate? any/c
         #:null any/c) ;; (json-null)
        any)] ;; jsexpr?
  ))

(module* for-extension #f
  (provide write-json*
           read-json*))

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
                    #:null [jsnull (json-null)]
                    #:encode [enc 'control]
                    #:indent [indent #f])
  (write-json* 'write-json x o
               #:null jsnull
               #:encode enc
               #:indent indent
               #:object-rep? hash?
               #:object-rep->hash values
               #:list-rep? list?
               #:list-rep->list values
               #:key-rep? symbol?
               #:key-rep->string symbol->immutable-string
               #:string-rep? string?
               #:string-rep->string values))

(define (write-json* who x o
                     #:null jsnull
                     #:encode enc
                     #:indent indent
                     #:object-rep? object-rep?
                     #:object-rep->hash object-rep->hash
                     #:list-rep? list-rep?
                     #:list-rep->list list-rep->list
                     #:key-rep? key-rep?
                     #:key-rep->string key-rep->string
                     #:string-rep? string-rep?
                     #:string-rep->string string-rep->string)
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
  (define-values (indent-byte indent-count)
    (cond
      [(eqv? #\tab indent)
       (values #x9 1)]
      [(exact-nonnegative-integer? indent)
       (values #x20 indent)]
      [else
       (values #f #f)]))
  (define format/write-indent-bytes
    (if indent
        (λ ()
          (for ([i (in-range indent-count)])
            (write-byte indent-byte o)))
        void))
  (define format/write-whitespace
    (if indent
        (λ () (write-byte #x20 o))
        void))
  (let write-jsval ([x x] [layer 0])
    (define format/write-indented-newline
      (if indent
          (let ([n (* indent-count layer)])
            (λ ()
              (newline o)
              (for ([i (in-range n)])
                (write-byte indent-byte o))))
          void))
    (cond [(or (exact-integer? x) (inexact-rational? x)) (write x o)]
          [(eq? x #f)     (write-bytes #"false" o)]
          [(eq? x #t)     (write-bytes #"true" o)]
          [(eq? x jsnull) (write-bytes #"null" o)]
          [(string-rep? x) (write-json-string (string-rep->string x))]
          [(list-rep? x)
           (let ([x (list-rep->list x)])
             (write-bytes #"[" o)
             (when (pair? x)
               (for/fold ([first? #t])
                         ([x (in-list x)])
                 (unless first? (write-bytes #"," o))
                 (format/write-indented-newline)
                 (format/write-indent-bytes)
                 (write-jsval x (add1 layer))
                 #f)
               (format/write-indented-newline))
             (write-bytes #"]" o))]
          [(object-rep? x)
           (define write-hash-kv
             (let ([first? #t])
               (λ (k v)
                 (unless (key-rep? k)
                   (raise-type-error who "legal JSON key value" k))
                 (if first? (set! first? #f) (write-bytes #"," o))
                 (format/write-indented-newline)
                 (format/write-indent-bytes)
                 ;; use a string encoding so we get the same deal with
                 ;; `rx-to-encode'
                 (write-json-string (key-rep->string k))
                 (write-bytes #":" o)
                 (format/write-whitespace)
                 (write-jsval v (add1 layer)))))
           (let ([x (object-rep->hash x)])
             (write-bytes #"{" o)
             (unless (hash-empty? x)
               (hash-for-each x write-hash-kv #t)
               (format/write-indented-newline))
             (write-bytes #"}" o))]
          [else (raise-type-error who "legal JSON value" x)]))
  (void))

;; -----------------------------------------------------------------------------
;; PARSING (from JSON to Racket)

(define (read-json [i (current-input-port)]
                   #:null [jsnull (json-null)]
                   #:replace-malformed-surrogate? [replace-malformed-surrogate? #f])
  (read-json* 'read-json i
              #:replace-malformed-surrogate? replace-malformed-surrogate?
              #:null jsnull
              #:make-object make-immutable-hasheq
              #:make-list values
              #:make-key string->symbol
              #:make-string values))

(define (read-json* who i
                    #:replace-malformed-surrogate? replace-malformed-surrogate?
                    #:null jsnull
                    #:make-object make-object-rep
                    #:make-list make-list-rep
                    #:make-key make-key-rep
                    #:make-string make-string-rep)
  ;; Follows the specification (eg, at json.org) -- no extensions.
  ;;
  (define (err fmt . args)
    (define-values [l c p] (port-next-location i))
    (raise-read-error (format "~a: ~a" who (apply format fmt args))
                      (object-name i) l c p #f))
  (define (json-whitespace? ch)
    (or (eq? ch #\space)
        (eq? ch #\tab)
        (eq? ch #\newline)
        (eq? ch #\return)))
  (define (skip-whitespace)
    (define ch (peek-char i))
    (cond
      [(char? ch)
       (cond
         [(json-whitespace? ch)
          (read-char i)
          (skip-whitespace)]
         [(char-whitespace? ch)
          (err "found whitespace that is not allowed by the JSON specification\n  char: ~s"
               ch)]
         [else ch])]
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
    (define (save-char c old-result pos)
      (define result
        (cond
          [(= pos (string-length old-result))
           (define new (make-string (* pos 2)))
           (string-copy! new 0 old-result 0 pos)
           new]
          [else old-result]))
      (string-set! result pos c)
      (values result (add1 pos)))
    (define (keep-char c old-result old-pos converter)
      (define-values (result pos) (save-char c old-result old-pos))
      (loop result pos converter))
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
         (define-values (e* new-result new-pos)
           (let resync ([e e] [result result] [pos pos])
             (cond
               [(<= #xD800 e #xDBFF)
                (cond
                  [(equal? (peek-bytes 2 0 i) #"\\u")
                   (read-bytes 2 i)
                   (define e2 (get-hex))
                   (cond
                     [(<= #xDC00 e2 #xDFFF)
                      (define cp (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000))
                      (values cp result pos)]
                     [replace-malformed-surrogate?
                      (define-values (new-result new-pos) (save-char (integer->char #xFFFD) result pos))
                      (resync e2 new-result new-pos)]
                     [else
                      (err "bad string \\u escape, bad second half of a UTF-16 pair")])]
                  [replace-malformed-surrogate?
                   (values #xFFFD result pos)]
                  [else
                   (err "bad string \\u escape, missing second half of a UTF-16 pair")])]
               [(<= #xDC00 e #xDFFF)
                (if replace-malformed-surrogate?
                    (values #xFFFD result pos)
                    (err "bad string \\u escape, missing first half of a UTF-16 pair"))]
               [else (values e result pos)])))
         (keep-char (integer->char e*) new-result new-pos converter)]
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
           [else
            (read-byte i) ;; consume the eof
            (err "error while parsing a json ~a" what)]))]))
  ;;
  (define (read-hash)
    (define (read-pair)
      (define k (read-json))
      (unless (string? k) (err "non-string value used for json object key"))
      (define ch (skip-whitespace))
      (when (eof-object? ch)
        (read-byte i) ;; consume the eof
        (err "unexpected end-of-file while parsing a json object pair"))
      (unless (char=? #\: ch)
        (err "error while parsing a json object pair"))
      (read-byte i)
      (cons (make-key-rep k) (read-json)))
    (make-object-rep (read-list 'object #\} read-pair)))
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
    ;; evaluate n * 10^exp to inexact? without passing large arguments to expt
    ;; assumes n is an integer
    (define (safe-exponential->inexact n exp)
      (define result-exp
        (if (= n 0)
            exp
            (+ (log (abs n) 10) exp)))
      (cond
        [(< result-exp -400)
         (cond
           [(>= n 0) 0.0]
           [else -0.0])]
        [(> result-exp 400)
         (cond
           [(= n 0) 0.0]
           [(> n 0) +inf.0]
           [(< n 0) -inf.0])]
        [else
         (exact->inexact (* n (expt 10 exp)))]))
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
         (read-exponent-rest n exp (to-number c) 1)]
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
         (read-exponent-rest n exp (to-number c) sgn)]
        [else (bad-input (bytes-append (n->string n exp)
                                       (bytes mark)
                                       mark2
                                       (maybe-bytes c))
                         #:eof? (eof-object? c))]))
    ;; more digits:
    (define (read-exponent-rest n exp exp2 sgn)
      (define c (peek-byte i))
      (cond
        [(digit-byte? c)
         (read-byte i)
         (read-exponent-rest n exp (+ (* 10 exp2) (to-number c)) sgn)]
        [else (safe-exponential->inexact n (+ exp (* sgn exp2)))]))
    (start))
  ;;
  (define (read-json [top? #f])
    (define ch (skip-whitespace))
    (cond
      [(eof-object? ch)
       (read-byte i) ;; consume the eof
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
                     (make-string-rep (read-a-string))]
      [(eqv? ch #\[) (read-byte i)
                     (make-list-rep
                      (read-list 'array #\] read-json))]
      [(eqv? ch #\{) (read-byte i)
                     (read-hash)]
      [else (bad-input)]))
  ;;
  (define (bad-input [prefix #""] #:eof? [eof? #f])
    (define bstr (make-bytes (sub1 (error-print-width))))
    (define bytes-read (peek-bytes-avail!* bstr 0 #f i))
    (if (or (and (eof-object? bytes-read) (equal? prefix #""))
            eof?)
        (err (string-append "unexpected end-of-file"
                            (if (equal? prefix #"")
                                ""
                                (format "after ~e" prefix))))
        (err (format "bad input starting ~e" (bytes-append prefix (if (number? bytes-read)
                                                                      (subbytes bstr 0 bytes-read)
                                                                      #""))))))
  ;;
  (read-json #t))

;; -----------------------------------------------------------------------------
;; CONVENIENCE FUNCTIONS

(define (jsexpr->string x
                        #:null [jsnull (json-null)]
                        #:encode [enc 'control]
                        #:indent [indent #f])
  (define o (open-output-string))
  (write-json* 'jsexpr->string x o
               #:null jsnull
               #:encode enc
               #:indent indent
               #:object-rep? hash?
               #:object-rep->hash values
               #:list-rep? list?
               #:list-rep->list values
               #:key-rep? symbol?
               #:key-rep->string symbol->immutable-string
               #:string-rep? string?
               #:string-rep->string values)
  (get-output-string o))

(define (jsexpr->bytes x
                       #:null [jsnull (json-null)]
                       #:encode [enc 'control]
                       #:indent [indent #f])
  (define o (open-output-bytes))
  (write-json* 'jsexpr->bytes x o
               #:null jsnull
               #:encode enc
               #:indent indent
               #:object-rep? hash?
               #:object-rep->hash values
               #:list-rep? list?
               #:list-rep->list values
               #:key-rep? symbol?
               #:key-rep->string symbol->immutable-string
               #:string-rep? string?
               #:string-rep->string values)
  (get-output-bytes o))

(define (string->jsexpr str
                        #:replace-malformed-surrogate? [replace-malformed-surrogate? #f]
                        #:null [jsnull (json-null)])
  ;; str is protected by contract
  (read-json* 'string->jsexpr (open-input-string str)
              #:replace-malformed-surrogate? replace-malformed-surrogate?
              #:null jsnull
              #:make-object make-immutable-hasheq
              #:make-list values
              #:make-key string->symbol
              #:make-string values))

(define (bytes->jsexpr bs
                       #:replace-malformed-surrogate? [replace-malformed-surrogate? #f]
                       #:null [jsnull (json-null)])
  ;; bs is protected by contract
  (read-json* 'bytes->jsexpr (open-input-bytes bs)
              #:replace-malformed-surrogate? replace-malformed-surrogate?
              #:null jsnull
              #:make-object make-immutable-hasheq
              #:make-list values
              #:make-key string->symbol
              #:make-string values))
