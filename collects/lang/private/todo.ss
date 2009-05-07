#lang scheme

;; If we eliminate char from HtDP/I, we need to add re-think 
;; the following functions. Concrete proposals attached. 

;; If you're in a hurry, look for QQQ. 

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
;; QQQ: this would be a re-definition of a Scheme function. Should we rename? 

(check-expect (beginner-make-string 3 "a") "aaa")
(check-error
 (beginner-make-string 3 "ab")
 (string-append "make-string: " 1-letter " expected, given " 
                (format "~s" "ab")))

(define-teach beginner make-string 
  (lambda (n s1)
    (unless (and (number? n) (exact-integer? n) (>= n 0))
      (error 'make-string "(exact) natural number expected, given ~e" n))
    (unless (1-letter? 'make-string s1)
      (error 'make-string "~a expected, given ~e" 1-letter s1))
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
 (string-append 
  "string->int: " 1-letter " expected, not a string: "
  "10"))
(check-error 
 (beginner-string->int "AB") 
 (string-append 
  "string->int: " 1-letter " expected, given "
  (format "~s" "AB")))

(define-teach beginner string->int 
  (lambda (s) 
    (unless (1-letter? 'string->int s)
      (error 'string->int "~a expected, given ~e" 1-letter s))
    (char->integer (string-ref s 0))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-explode "hello") (list "h" "e" "l" "l" "o"))
(check-error
 (beginner-explode 10)
 (string-append
  "explode: string expected, given " 
  "10"))

(define-teach beginner explode 
  (lambda (s)
    (unless (string? s)
      (error 'explode "string expected, given ~e" s))
    (map string (string->list s))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-implode (list "h" "e" "l" "l" "o")) "hello")
(check-error 
 (beginner-implode 10)
 (string-append 
  "implode: list of " 1-letter* " expected, not a list: " 
  "10"))
(check-error 
 (beginner-implode '("he" "l"))
 (string-append 
  "implode: list of " 1-letter* " expected, given " 
  (format "~s" '("he" "l"))))

(define-teach beginner implode
  (lambda (los)
    (unless (1-letter*? 'implode los)
      (error 'implode "list of ~a expected, given ~e" 1-letter* los))
    (list->string (map (lambda (s) (string-ref s 0)) los))))

;; -----------------------------------------------------------------------------

(check-expect (beginner-string1-numeric? "0") true)
(check-expect (beginner-string1-numeric? "a") false)
(check-error
 (beginner-string1-numeric? "ab")
 (string-append "string1-numeric?: " 1-letter " expected, given " 
                (format "~s" "ab")))

(define-teach beginner string1-numeric? 
  ;; is this: (number? (string->number s)) enough?
  (lambda (s1)
    (unless (1-letter? 'string1-numeric? s1)
      (error 'string1-numeric? "~a expected, given ~e" 1-letter s1))   
    (char-numeric? (string-ref s1 0))))

;; -----------------------------------------------------------------------------

;; I used copying here and I feel awful. 
(check-expect (beginner-string1-alphabetic? "0") false)
(check-expect (beginner-string1-alphabetic? "a") true)
(check-error
 (beginner-string1-alphabetic? "ab")
 (string-append "string1-alphabetic?: " 1-letter " expected, given " 
                (format "~s" "ab")))

(define-teach beginner string1-alphabetic? 
  ;; is this
  #;
  (andmap (lambda (c) 
            (or (string<=? "A" x "Z") (string<=? "a" x "z")))
          (string->list s))
  ;; enough?
  (lambda (s1)
    (unless (1-letter? 'string1-alphabetic? s1)
      (error 'string1-alphabetic? "~a expected, given ~e" 1-letter s1))
    (char-alphabetic? (string-ref s1 0))))

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

;; !!! redefinition !!! (and copy from teachprims.ss)
;; QQQ: do we need a new name????
(check-expect (intermediate-build-string 3 (lambda (x) "x")) "xxx")

(define-teach intermediate build-string
  (lambda (n f)
    (unless (and (number? n) (integer? n) (>= n 0))
      (error 'build-string
             "first argument must be of type <natural number>, given ~e" 
             n))
    (unless (and (procedure? f) (procedure-arity-includes? f 1))
      (error 'build-string
             "second argument must be a <procedure> that accepts one argument, given ~e"
             f))
    (apply string-append 
           (build-list
            n
            (lambda (i)
              (define r (f i))
              (unless (1-letter? 'build-string r)
                (error 'build-string
                       "second argument must be a <procedure> that produces a ~a, given ~e, which produced ~e for ~e"
                       1-letter f r i))
              r)))))

(test)