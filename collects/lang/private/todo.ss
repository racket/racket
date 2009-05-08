#lang scheme

#| QQQ: okay?
char-upcase:   use string-upcase instead
char-downcase: use string-downcase instead 
string:        use string-append instead 
|#

#| QQQ: I noticed an oddity: 
substring consumes 2 or 3 arguments 
|#

;; -----------------------------------------------------------------------------
;; auxiliary stuff, ignore
(require test-engine/scheme-tests)

(define 1-letter "1-letter string")
(define 1-letter* (format "~as" 1-letter))

;; Symbol Any -> Boolean 
;; is this a 1-letter string?
(define (1-letter? tag s)
  (unless (string? s)
    (error tag "~a expected, not a string: ~e" 1-letter s))
  (= (string-length s) 1))


;; Symbol Any -> Boolean 
;; is s a list of 1-letter strings
;; effect: not a list, not a list of strings 
(define (1-letter*? tag s)
  (unless (list? s)
    (error tag "list of ~a expected, not a list: ~e" 1-letter* s))
  (for-each 
   (lambda (c) 
     (unless (string? c)
       (error tag "list of ~a expected, not a string: ~e" 1-letter* c)))
   s)
  #;     (lambda (s) (= 1 (string-length s)))
  (andmap (compose (curry = 1) string-length) s))

(define-syntax (define-teach stx)
  (syntax-case stx ()
    [(_ level id expr)
     (with-syntax ([level-id (datum->syntax
                              (syntax id)
                              (string->symbol
                               (format "~a-~a"
                                       (syntax->datum (syntax level))
                                       (syntax->datum (syntax id))))
                              (syntax id))])
       (syntax (define level-id
                 (let ([id expr])
                   id))))]))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string-ith "hell" 0) "h")
(check-error
 (beginner-string-ith "hell" 4) 
 (string-append 
  "string-ith:"
  " second argument must be between 0 and the length of the given string (4), given "
  "4"))

(define-teach beginner string-ith
  (lambda (s n)
    (unless (string? s)
      (error 'string-ith "first argument must be of type <string>, given ~e" s))
    (unless (and (number? n) (integer? n) (>= n 0))
      (error 'string-ith
             "second argument must be of type <natural number>, given ~e" 
             n))
    (unless (< n (string-length s))
      (error 'string-ith 
             "second argument must be between 0 and the length of the given string (~s), given ~a"
             (string-length s) n))
    (string (string-ref s n))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-replicate 3 "a") "aaa")
(check-expect (beginner-replicate 3 "ab") "ababab")
(check-error (beginner-replicate 3 10) "replicate: string expected, given 10")

(define-teach beginner replicate 
  (lambda (n s1)
    (unless (and (number? n) (exact-integer? n) (>= n 0))
      (error 'replicate "(exact) natural number expected, given ~e" n))
    (unless (string? s1)
      (error 'replicate "string expected, given ~e" s1))
    (apply string-append (build-list n (lambda (i) s1)))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-int->string 10) "\n")
(check-error 
 (beginner-int->string 56555) 
 (string-append 
  "int->string: exact integer in [0,55295] or [57344 1114111] expected, given "
  "56555"))
(check-error 
 (beginner-int->string "A") 
 (string-append 
  "int->string: exact integer in [0,55295] or [57344 1114111] expected, given "
  "A"))

(define-teach beginner int->string 
  (lambda (i) 
    (unless (and (exact-integer? i) (or (<= 0 i 55295) (<= 57344 i 1114111)))
      (error 'int->string 
             "exact integer in [0,55295] or [57344 1114111] expected, given ~a"
             i))
    (string (integer->char i))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string->int "A") 65)
(check-error 
 (beginner-string->int 10) 
 (string-append "string->int: " 1-letter " expected, not a string: 10"))
(check-error 
 (beginner-string->int "AB") 
 (string-append "string->int: " 1-letter " expected, given " (format "~s" "AB")))

(define-teach beginner string->int 
  (lambda (s) 
    (unless (1-letter? 'string->int s)
      (error 'string->int "~a expected, given ~e" 1-letter s))
    (char->integer (string-ref s 0))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-explode "hello") (list "h" "e" "l" "l" "o"))
(check-error (beginner-explode 10) 
             (string-append "explode: string expected, given " "10"))

(define-teach beginner explode 
  (lambda (s)
    (unless (string? s)
      (error 'explode "string expected, given ~e" s))
    (map string (string->list s))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-implode (list "h" "e" "l" "l" "o")) "hello")
(check-error (beginner-implode 10)
             (string-append "implode: list of " 1-letter* 
                            " expected, not a list: 10"))
(check-error (beginner-implode '("he" "l"))
             (string-append "implode: list of " 1-letter* " expected, given " 
                            (format "~s" '("he" "l"))))

(define-teach beginner implode
  (lambda (los)
    (unless (1-letter*? 'implode los)
      (error 'implode "list of ~a expected, given ~e" 1-letter* los))
    (apply string-append los)))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string-numeric? "0") true)
(check-expect (beginner-string-numeric? "10") true)
(check-expect (beginner-string-numeric? "a") false)
(check-expect (beginner-string-numeric? "ab") false)
(check-error (beginner-string-numeric? 10)
             (string-append "string-numeric?: string expected, given 10"))

(define-teach beginner string-numeric? 
  ;; is this: (number? (string->number s)) enough?
  (lambda (s1)
    (unless (string? s1)
      (error 'string-numeric? "string expected, given ~e" s1))   
    (andmap char-numeric? (string->list s1))))

;; -----------------------------------------------------------------------------

;; I used copying here and I feel awful. 
(check-expect (beginner-string-alphabetic? "a0") false)
(check-expect (beginner-string-alphabetic? "a") true)
(check-expect (beginner-string-alphabetic? "ba") true)
(check-expect (beginner-string-alphabetic? "ab") true)

(define-teach beginner string-alphabetic? 
  (lambda (s1)
    (unless (string? s1)
      (error 'string-alphabetic? "string expected, given ~e" s1))
    (andmap char-alphabetic? (string->list s1))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string-whitespace? "  ") true)
(check-expect (beginner-string-whitespace? "  \t") true)
(check-expect (beginner-string-whitespace? "ABC") false)

(define-teach beginner string-whitespace? 
  (lambda (s)
    (unless (string? s)
      (error 'string-upper-case? "string expected, given ~e" s))
    (andmap char-whitespace? (string->list s))))

;; -----------------------------------------------------------------------------
;; I copied the next two, and I feel awful, too. 
(check-expect (beginner-string-upper-case? "  ") false)
(check-expect (beginner-string-upper-case? "AB\t") false)
(check-expect (beginner-string-upper-case? "ABC") true)

(define-teach beginner string-upper-case? 
  (lambda (s)
    (unless (string? s)
      (error 'string-upper-case? "string expected, given ~e" s))
    (andmap char-upper-case? (string->list s))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string-lower-case? "  ") false)
(check-expect (beginner-string-lower-case? "ab\t") false)
(check-expect (beginner-string-lower-case? "abc") true)

(define-teach beginner string-lower-case? 
  (lambda (s)
    (unless (string? s)
      (error 'string-lower-case? "string expected, given ~e" s))
    (andmap char-lower-case? (string->list s))))

;; -----------------------------------------------------------------------------

(test)