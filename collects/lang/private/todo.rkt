#lang scheme

#| QQQ: okay?
char-upcase:   use string-upcase instead
char-downcase: use string-downcase instead 
string:        use string-append instead 
|#

#| QQQ: I noticed an oddity: 
substring consumes 2 or 3 arguments 
|#

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
;; auxiliary stuff, ignore

(define 1-LET "1-letter string")
(define 1-LETTER (format "<~a>" 1-LET))
(define 1-LETTER* (format "<list of ~as>" 1-LET))
(define NAT "<natural number>")

;; Symbol Any -> Boolean 
;; is this a 1-letter string?
(define (1-letter? tag s)
  (unless (string? s) (err tag "~a expected, not a string: ~e" 1-LETTER s))
  (= (string-length s) 1))

;; Symbol Any -> Boolean 
;; is s a list of 1-letter strings
;; effect: not a list, not a list of strings 
(define (1-letter*? tag s)
  (unless (list? s) (err tag "~a expected, not a <list>: ~e" 1-LETTER* s))
  (for-each 
   (lambda (c) 
     (unless (string? c) (err tag "~a expected, not a <string>: ~e" 1-LETTER* c)))
   s)
  (andmap (compose (curry = 1) string-length) s))

(define (err tag msg-format . args)
  (raise 
   (make-exn:fail:contract
    (apply format (string-append (symbol->string tag) ": " msg-format) args)
    (current-continuation-marks))))

(define cerr 
  (case-lambda
    [(tag check-result format-msg actual)
     (unless check-result
       (err tag (string-append format-msg " expected, given ~e") actual))]
    [(tag check-result format-msg actual snd)
     (unless check-result
       (err tag (string-append format-msg " for ~a argument expected, given ~e")
            snd actual))]))

;; -----------------------------------------------------------------------------

(define-teach beginner string-ith
  (lambda (s n)
    (define f "<exact integer in [0, length of the given string (~s)]>")
    (cerr 'string-ith (string? s) "<string>" s "first")
    (cerr 'string-ith (and (number? n) (integer? n) (>= n 0)) NAT n "second")
    (let ([l (string-length s)]) 
      (cerr 'string-ith (< n l) (format f l) n "second"))
    (string (string-ref s n))))

;; -----------------------------------------------------------------------------

(define-teach beginner replicate 
  (lambda (n s1)
    (cerr 'replicate (and (number? n) (exact-integer? n) (>= n 0)) NAT n)
    (cerr 'replicate (string? s1) "<string>" s1)
  (apply string-append (build-list n (lambda (i) s1)))))

;; -----------------------------------------------------------------------------

(define-teach beginner int->string 
  (lambda (i) 
    (cerr 'int->string 
          (and (exact-integer? i) (or (<= 0 i 55295) (<= 57344 i 1114111)))
          "<exact integer in [0,55295] or [57344 1114111]>"
          i)
    (string (integer->char i))))

;; -----------------------------------------------------------------------------

(define-teach beginner string->int 
  (lambda (s) 
    (cerr 'string->int (1-letter? 'string->int s) 1-LETTER s)
    (char->integer (string-ref s 0))))

;; -----------------------------------------------------------------------------

(define-teach beginner explode 
  (lambda (s)
    (cerr 'explode (string? s) "<string>" s)
    (map string (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner implode
  (lambda (los)
    (cerr 'implode (1-letter*? 'implode los) 1-LETTER* los)
    (apply string-append los)))

;; -----------------------------------------------------------------------------

(define-teach beginner string-numeric? 
  ;; is this: (number? (string->number s)) enough?
  (lambda (s1)
    (cerr 'string-numeric? (string? s1) "<string>" s1)
    (andmap char-numeric? (string->list s1))))

;; -----------------------------------------------------------------------------

;; I used copying here and I feel awful. 

(define-teach beginner string-alphabetic? 
  (lambda (s1)
    (cerr 'string-alphabetic? (string? s1) "<string>" s1)
    (andmap char-alphabetic? (string->list s1))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-whitespace? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s)  "<string>" s)
    (andmap char-whitespace? (string->list s))))

;; -----------------------------------------------------------------------------
;; I copied the next two, and I feel awful, too. 

(define-teach beginner string-upper-case? 
  (lambda (s)
    (cerr 'string-upper-case? (string? s) "<string>" s)
    (andmap char-upper-case? (string->list s))))

;; -----------------------------------------------------------------------------

(define-teach beginner string-lower-case? 
  (lambda (s)
    (cerr 'string-lower-case? (string? s) "<string>" s)
    (andmap char-lower-case? (string->list s))))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(require test-engine/scheme-tests)

(check-expect (beginner-string-ith "hell" 0) "h")
(check-error
 (beginner-string-ith "hell" 4) 
 (string-append 
  "string-ith:"
  " <exact integer in [0, length of the given string (4)]>"
  " for second argument expected, given "
  "4"))

(check-error
 (beginner-string-ith 10 4) 
 (string-append "string-ith: <string> for first argument expected, given "
                "10"))

(check-error
 (beginner-string-ith "10" 'a)
 (string-append "string-ith: <natural number> for second argument expected, given "
                "a"))


(check-expect (beginner-replicate 3 "a") "aaa")
(check-expect (beginner-replicate 3 "ab") "ababab")
(check-error (beginner-replicate 3 10) "replicate: <string> expected, given 10")

(check-expect (beginner-int->string 10) "\n")
(check-error 
 (beginner-int->string 56555) 
 (string-append 
  "int->string: <exact integer in [0,55295] or [57344 1114111]> expected, given "
  "56555"))
(check-error 
 (beginner-int->string "A") 
 (string-append 
  "int->string: <exact integer in [0,55295] or [57344 1114111]> expected, given "
  (format "~s" "A")))

(check-expect (beginner-string->int "A") 65)
(check-error 
 (beginner-string->int 10) 
 (string-append "string->int: " 1-LETTER " expected, not a string: 10"))
(check-error 
 (beginner-string->int "AB") 
 (string-append
   "string->int: " 1-LETTER " expected, given " (format "~s" "AB")))

(check-expect (beginner-explode "hello") (list "h" "e" "l" "l" "o"))
(check-error (beginner-explode 10) 
             (string-append "explode: <string> expected, given " "10"))

(check-expect (beginner-implode (list "h" "e" "l" "l" "o")) "hello")
(check-error (beginner-implode 10)
             (string-append "implode: " 1-LETTER* 
                            " expected, not a <list>: 10"))
(check-error (beginner-implode '("he" "l"))
             (string-append "implode: " 1-LETTER* " expected, given " 
                            (format "~s" '("he" "l"))))

(check-expect (beginner-string-numeric? "0") true)
(check-expect (beginner-string-numeric? "10") true)
(check-expect (beginner-string-numeric? "a") false)
(check-expect (beginner-string-numeric? "ab") false)
(check-error (beginner-string-numeric? 10)
             (string-append "string-numeric?: <string> expected, given 10"))


(check-expect (beginner-string-alphabetic? "a0") false)
(check-expect (beginner-string-alphabetic? "a") true)
(check-expect (beginner-string-alphabetic? "ba") true)
(check-expect (beginner-string-alphabetic? "ab") true)

(check-expect (beginner-string-whitespace? "  ") true)
(check-expect (beginner-string-whitespace? "  \t") true)
(check-expect (beginner-string-whitespace? "ABC") false)

(check-expect (beginner-string-upper-case? "  ") false)
(check-expect (beginner-string-upper-case? "AB\t") false)
(check-expect (beginner-string-upper-case? "ABC") true)

(check-expect (beginner-string-lower-case? "  ") false)
(check-expect (beginner-string-lower-case? "ab\t") false)
(check-expect (beginner-string-lower-case? "abc") true)



(test)
