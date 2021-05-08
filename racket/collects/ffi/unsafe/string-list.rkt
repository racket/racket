#lang racket/base
(require ffi/unsafe)

(provide (protect-out
          _bytes-list
          _bytes-list/nul-terminated
          _string-list
          _string-list/utf-8
          _string-list/locale
          _string-list/latin-1
          _string-list/utf-16
          _string-list/ucs-4))

(define (check who l elem elem?)
  (unless (and (list? l)
               (andmap elem? l))
    (raise-argument-error who (format "(listof ~a)" elem) l)))

(define (allocate-bytes-block l terminator-len)
  (define ptr-bytes (ctype-sizeof _pointer))
  (define roundup (sub1 ptr-bytes))
  (define mask (bitwise-not roundup))
  (define (align n) (bitwise-and (+ n roundup) mask))
  (define size (for/fold ([size ptr-bytes]) ([bstr (in-list l)])
                 (+ size ptr-bytes (align (+ (bytes-length bstr) terminator-len)))))
  (define m (malloc size 'atomic-interior))
  (memset m 0 size)
  (for/fold ([offset (* (add1 (length l)) ptr-bytes)]) ([bstr (in-list l)]
                                                        [i (in-naturals)])
    (ptr-set! m _pointer i (ptr-add m offset))
    (memcpy m offset bstr (bytes-length bstr))
    (+ offset (align (+ (bytes-length bstr) terminator-len))))
  m)

(define (extract-list p _t)
  (define len (let loop ([i 0])
                (if (ptr-ref p _pointer i)
                    (loop (add1 i))
                    i)))
  (cast p _pointer (_list o _t len)))

(define _bytes-list
  (make-ctype _pointer
              (lambda (l)
                (check '_bytes-list l 'bytes bytes?)
                (allocate-bytes-block l 0))
              (lambda (p)
                (extract-list p _bytes))))

(define _bytes-list/nul-terminated
  (make-ctype _pointer
              (lambda (l)
                (check '_bytes-list/nul-terminated l 'bytes bytes?)
                (allocate-bytes-block l 1))
              (lambda (p)
                (extract-list p _bytes))))

(define (make-_string-list who convert _t terminator-len string?)
  (make-ctype _pointer
              (lambda (l)
                (check who l 'string string?)
                (allocate-bytes-block (map convert l) terminator-len))
              (lambda (p)
                (extract-list p (or _t _string)))))

(define (_string? str)
  (define _t _string)
  (cond
    [(or (eq? _t _string*/utf-8)
         (eq? _t _string*/latin-1)
         (eq? _t _string*/locale))
     (or (path? str) (string? str))]
    [else (string? str)]))

(define (string->bytes str)
  (define _t _string)
  (cond
    [(eq? _t _string/utf-8) (string->bytes/utf-8 str)]
    [(eq? _t _string*/utf-8) (if (path? str)
                                 (path->bytes str)
                                 (string->bytes/utf-8 str))]
    [(eq? _t _string/latin-1) (bytes->string/latin-1 str)]
    [(eq? _t _string*/latin-1) (if (path? str)
                                   (path->bytes str)
                                   (string->bytes/latin-1 str))]
    [(eq? _t _string/locale) (string->bytes/locale str)]
    [(eq? _t _string*/locale) (if (path? str)
                                  (path->bytes str)
                                  (string->bytes/locale str))]
    [(eq? _t _string/utf-16) (string->bytes/utf-16 str)]
    [(eq? _t _string/ucs-4) (string->bytes/ucs-4 str)]
    [else (error '_string-list
                 "unrecognized current _string conversion")]))

(define (string->bytes/utf-16 str)
  (define len
    (for/fold ([len 0]) ([c (in-string str)])
      (+ len
         (if ((char->integer c) . > . #xFFFF)
             2
             1))))
  (define bstr (make-bytes (* 2 len)))
  (let loop ([i 0] [j 0])
    (unless (= i (string-length str))
      (define v (char->integer (string-ref str i)))
      (cond
        [(v . <= . #xFFFF)
         (ptr-set! bstr _uint16 j v)
         (loop (add1 i) (+ j 1))]
        [else
         (define av (- v #x10000))
         (define hi (bitwise-ior #xD800 (bitwise-and (arithmetic-shift av -10) #x3FF)))
         (define lo (bitwise-ior #xDC00 (bitwise-and av #x3FF)))
         (ptr-set! bstr _uint16 j hi)
         (ptr-set! bstr _uint16 (+ j 1) lo)
         (loop (add1 i) (+ j 2))])))
  bstr)

(define (string->bytes/ucs-4 str)
  (define bstr (make-bytes (* 4 (string-length str))))
  (for ([i (in-range (string-length str))])
    (ptr-set! bstr _uint32 i (char->integer (string-ref str i))))
  bstr)

(define _string-list (make-_string-list '_string-list string->bytes #f 1 _string?))
(define _string-list/utf-8 (make-_string-list '_string-list/utf-8 string->bytes/utf-8 _string/utf-8 1 string?))
(define _string-list/locale (make-_string-list '_string-list/locale string->bytes/locale _string/locale 1 string?))
(define _string-list/latin-1 (make-_string-list '_string-list/latin-1 string->bytes/latin-1 _string/latin-1 1 string?))
(define _string-list/utf-16 (make-_string-list '_string-list/utf-16 string->bytes/utf-16 _string/utf-16 2 string?))
(define _string-list/ucs-4 (make-_string-list '_string-list/ucs-4 string->bytes/ucs-4 _string/ucs-4 4 string?))
