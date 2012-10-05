#lang racket/base

#| Roughly based on the PLaneT package by Dave Herman,
   Originally released under MIT license.
|#

;; ----------------------------------------------------------------------------
;; Customization

;; The default translation for a JSON `null' value
(provide json-null)
(define json-null (make-parameter 'null))

;; ----------------------------------------------------------------------------
;; Predicate

(provide jsexpr?)
(define (jsexpr? x #:null [jsnull (json-null)])
  (let loop ([x x])
    (or (exact-integer? x)
        (inexact-real? x)
        (boolean? x)
        (string? x)
        (eq? x jsnull)
        (and (list? x) (andmap loop x))
        (and (hash? x) (for/and ([(k v) (in-hash x)])
                         (and (symbol? k) (loop v)))))))

;; ----------------------------------------------------------------------------
;; Generation: Racket -> JSON

(provide write-json)
(define (write-json x [o (current-output-port)]
                    #:null [jsnull (json-null)] #:encode [enc 'control])
  (define (escape m)
    (define ch (string-ref m 0))
    (define r
      (assoc ch '([#\backspace . "\\b"] [#\newline . "\\n"] [#\return . "\\r"]
                  [#\page . "\\f"] [#\tab . "\\t"]
                  [#\\ . "\\\\"] [#\" . "\\\""])))
    (define (u-esc n)
      (define str (number->string n 16))
      (define pad (case (string-length str)
                    [(1) "000"] [(2) "00"] [(3) "0"] [else ""]))
      (string-append "\\u" pad str))
    (if r
      (cdr r)
      (let ([n (char->integer ch)])
        (if (n . < . #x10000)
          (u-esc n)
          ;; use the (utf-16 surrogate pair) double \u-encoding
          (let ([n (- n #x10000)])
            (string-append (u-esc (+ #xD800 (arithmetic-shift n -10)))
                           (u-esc (+ #xDC00 (bitwise-and n #x3FF)))))))))
  (define rx-to-encode
    (case enc
      [(control) #rx"[\0-\37\\\"\177]"]
      [(all)     #rx"[\0-\37\\\"\177-\U10FFFF]"]
      [else (raise-type-error 'write-json "encoding symbol" enc)]))
  (define (write-json-string str)
    (write-bytes #"\"" o)
    (write-string (regexp-replace* rx-to-encode str escape) o)
    (write-bytes #"\"" o))
  (let loop ([x x])
    (cond [(or (exact-integer? x) (inexact-real? x)) (write x o)]
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
               (raise-type-error 'write-json "bad JSON key value" k))
             (if first? (set! first? #f) (write-bytes #"," o))
             (write (symbol->string k) o) ; no `printf' => proper escapes
             (write-bytes #":" o)
             (loop v))
           (write-bytes #"}" o)]
          [else (raise-type-error 'write-json "bad JSON value" x)]))
  (void))

;; ----------------------------------------------------------------------------
;; Parsing: JSON -> Racket

(require syntax/readerr)

(provide read-json)
(define (read-json [i (current-input-port)] #:null [jsnull (json-null)])
  ;; Follows the specification (eg, at json.org) -- no extensions.
  ;;
  (define (err fmt . args)
    (define-values [l c p] (port-next-location i))
    (raise-read-error (format "read-json: ~a" (apply format fmt args))
                      (object-name i) l c p #f))
  (define (skip-whitespace) (regexp-match? #px#"^\\s*" i))
  ;;
  ;; Reading a string *could* have been nearly trivial using the racket
  ;; reader, except that it won't handle a "\/"...
  (define (read-string)
    (let loop ([l* '()])
      ;; note: use a string regexp to extract utf-8-able text
      (define m (cdr (or (regexp-try-match #rx"^(.*?)(\"|\\\\(.))" i)
                         (err "unterminated string"))))
      (define l (if ((bytes-length (car m)) . > . 0) (cons (car m) l*) l*))
      (define esc (caddr m))
      (cond
        [(not esc) (bytes->string/utf-8 (apply bytes-append (reverse l)))]
        [(assoc esc '([#"b" . #"\b"] [#"n" . #"\n"] [#"r" . #"\r"]
                      [#"f" . #"\f"] [#"t" . #"\t"]
                      [#"\\" . #"\\"] [#"\"" . #"\""] [#"/" . #"/"]))
         => (λ (m) (loop (cons (cdr m) l)))]
        [(equal? esc #"u")
         (let* ([e (or (regexp-try-match #px#"^[a-fA-F0-9]{4}" i)
                       (err "bad string \\u escape"))]
                [e (string->number (bytes->string/utf-8 (car e)) 16)])
           (define e*
             (if (<= #xD800 e #xDFFF)
               ;; it's the first part of a UTF-16 surrogate pair
               (let* ([e2 (or (regexp-try-match #px#"^\\\\u([a-fA-F0-9]{4})" i)
                              (err "bad string \\u escape, ~a"
                                   "missing second half of a UTF16 pair"))]
                      [e2 (string->number (bytes->string/utf-8 (cadr e2)) 16)])
                 (if (<= #xDC00 e2 #xDFFF)
                   (+ (arithmetic-shift (- e #xD800) 10) (- e2 #xDC00) #x10000)
                   (err "bad string \\u escape, ~a"
                        "bad second half of a UTF16 pair")))
               e)) ; single \u escape
           (loop (cons (string->bytes/utf-8 (string (integer->char e*))) l)))]
        [else (err "bad string escape: \"~a\"" esc)])))
  ;;
  (define (read-list what end-rx read-one)
    (skip-whitespace)
    (if (regexp-try-match end-rx i)
      '()
      (let loop ([l (list (read-one))])
        (skip-whitespace)
        (cond [(regexp-try-match end-rx i) (reverse l)]
              [(regexp-try-match #rx#"^," i) (loop (cons (read-one) l))]
              [else (err "error while parsing a json ~a" what)]))))
  ;;
  (define (read-hash)
    (define (read-pair)
      (define k (read-json))
      (unless (string? k) (err "non-string value used for json object key"))
      (skip-whitespace)
      (unless (regexp-try-match #rx#"^:" i)
        (err "error while parsing a json object pair"))
      (list (string->symbol k) (read-json)))
    (apply hasheq (apply append (read-list 'object #rx#"^}" read-pair))))
  ;;
  (define (read-json [top? #f])
    (skip-whitespace)
    (cond
      [(and top? (eof-object? (peek-char i))) eof]
      [(regexp-try-match #px#"^true\\b"  i) #t]
      [(regexp-try-match #px#"^false\\b" i) #f]
      [(regexp-try-match #px#"^null\\b"  i) jsnull]
      [(regexp-try-match
        #rx#"^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?" i)
       => (λ (bs) (string->number (bytes->string/utf-8 (car bs))))]
      [(regexp-try-match #rx#"^[\"[{]" i)
       => (λ (m)
            (let ([m (car m)])
              (cond [(equal? m #"\"") (read-string)]
                    [(equal? m #"[")  (read-list 'array #rx#"^\\]" read-json)]
                    [(equal? m #"{")  (read-hash)])))]
      [else (err "bad input")]))
  ;;
  (read-json #t))

;; ----------------------------------------------------------------------------
;; Convenience functions

(provide jsexpr->string jsexpr->bytes)
(define (jsexpr->string x #:null [jsnull (json-null)] #:encode [enc 'control])
  (define o (open-output-string))
  (write-json x o #:null jsnull #:encode enc)
  (get-output-string o))
(define (jsexpr->bytes x #:null [jsnull (json-null)] #:encode [enc 'control])
  (define o (open-output-bytes))
  (write-json x o #:null jsnull #:encode enc)
  (get-output-bytes o))

(provide string->jsexpr bytes->jsexpr)
(define (string->jsexpr str #:null [jsnull (json-null)])
  (unless (string? str) (raise-type-error 'string->jsexpr "string" str))
  (read-json (open-input-string str) #:null jsnull))
(define (bytes->jsexpr str #:null [jsnull (json-null)])
  (unless (bytes? str) (raise-type-error 'bytes->jsexpr "bytes" str))
  (read-json (open-input-bytes str) #:null jsnull))
