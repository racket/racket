#lang scheme/base

;; Readtable-based R6RS reading

(require syntax/readerr
         scheme/promise
         (for-syntax scheme/base))

(provide with-r6rs-reader-parameters
         rx:id
         rx:number)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generic-not-allowed where ch port src line col pos)
  (raise-read-error 
   (format "illegal character~a in input: `~a'" 
           where
           (let ([s (format "~s" (string ch))])
             (substring s 1 (- (string-length s) 1))))
   src line col pos 1))

(define (not-allowed ch port src line col pos)
  (generic-not-allowed "" ch port src line col pos))

(define (dispatch-not-allowed ch port src line col pos)
  (generic-not-allowed " after `#'" ch port src line col pos))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  #!r6rs as a comment

(define (read-hash-bang ch port src line col pos)
  (if (regexp-try-match #rx"^r6rs" port)
      (make-special-comment #f)
      (let* ([s (regexp-match #rx"^(r6r|r6|r|)(.|)" port)]
             [len (+ 2 (bytes-length (cadr s)))]
             [next (caddr s)])
        (if (bytes=? next #"")
            (raise-read-eof-error
             (format "unexpected end-of-file after `#!~a'"
                     (cadr s))
             src line (and col (+ col len)) (and pos (+ pos len)) 1)
            (generic-not-allowed (format " after `#!~a'" (cadr s))
                                 (string-ref (bytes->string/utf-8 (caddr s)) 0)
                                 port
                                 src
                                 line
                                 (and col (+ col len))
                                 (and pos (+ pos len)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  delimiter helpers

(define (delimiter? delim)
  (or (eof-object? delim)
      (char-whitespace? delim)
      (char=? delim #\()
      (char=? delim #\))
      (char=? delim #\[)
      (char=? delim #\])
      (char=? delim #\")
      (char=? delim #\;)
      (char=? delim #\#)))

(define (check-delimiter result prefix ch port src line col pos len)
  (let ([delim (peek-char port)])
    (if (delimiter? delim)
        result
        (generic-not-allowed (format " after ~a~a (need a delimiter)"
                                     prefix ch)
                             (read-char port)
                             port src line 
                             (and col (+ col len))
                             (and pos (+ pos len))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  booleans (delimiter required)

(define (read-boolean ch port src line col pos)
  (check-delimiter (case ch
                     [(#\t #\T) #t]
                     [else #f])
                   "#" ch
                   port src line col pos 2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  characters

(define (read-character ch port src line col pos)
  ;; #\ has been consumed
  (let ([ch (read-char port)])
    (if (eof-object? ch)
        (raise-read-eof-error
         "unexpected end-of-file after `#\\'"
         src line (and col (+ col 1)) (and pos (+ pos 1)) 
         2)
        (let ([next (peek-char port)])
          (if (delimiter? next)
              ch
              (let ([r+str
                     (ormap (lambda (d)
                              (and (eq? ch (car d))
                                   (regexp-try-match (cadr d) port)
                                   (values (cddr d))))
                            (let-syntax ([char-names
                                          (lambda (stx)
                                            (syntax-case stx ()
                                              [(_ str ...)
                                               (let ([strs (map syntax-e (syntax->list #'(str ...)))])
                                                 (with-syntax ([(init-char ...)
                                                                (map (lambda (s) (string-ref s 0))
                                                                     strs)]
                                                               [(rx ...)
                                                                (map (lambda (s)
                                                                       (regexp (string-append "^" (substring s 1))))
                                                                     strs)]
                                                               [(result-char ...)
                                                                (map (lambda (s)
                                                                       (cond
                                                                        [(string=? s "alarm") #\u07]
                                                                        [(string=? s "esc") #\u1B]
                                                                        [(string=? s "delete") #\u7F]
                                                                        [else
                                                                         (read (open-input-string (string-append "#\\" s)))]))
                                                                     strs)])
                                                   #`(quote ((init-char rx result-char . str) ...))))]))])
                              (char-names
                               "nul"
                               "space"
                               "newline"
                               "alarm"
                               "backspace"
                               "tab"
                               "linefeed"
                               "newline"
                               "vtab"
                               "page"
                               "return"
                               "esc"
                               "space"
                               "delete")))])
                (if r+str
                    (check-delimiter
                     (car r+str)
                     "#\\" (cdr r+str)
                     port src line col pos 
                     (+ 2 (string-length (cdr r+str))))
                    (let ([hex (and (eq? ch #\x)
                                    (regexp-try-match #rx"^[0-9a-fA-F]+" port))])
                      (if hex
                          (let ([hex-val (string->number
                                          (bytes->string/latin-1 (car hex))
                                          16)])
                            (if (or (<= 0 hex-val #xD7FF)
                                    (<= #xE000 hex-val #x10FFFF))
                                (check-delimiter
                                 (integer->char hex-val)
                                 "#\\" (car hex)
                                 port src line col pos
                                 (+ 3 (bytes-length (car hex))))
                                (raise-read-error
                                 (format "out of range character constant `#\\x~a'"
                                         (car hex))
                                 src line 
                                 col
                                 pos
                                 (and pos (+ 3 (bytes-length (car hex)))))))
                          (let ([more (bytes->string/utf-8
                                       (car (regexp-match #px"^.([a-z]{0,20})" port)))])
                            (raise-read-error
                             (format "unknown character constant `#\\~a~a'"
                                     ch more)
                             src line 
                             col
                             pos
                             (and pos (+ 2 (string-length more))))))))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   byte vectors

(define (do-read-byte-string stx? ch port src line col pos)
  ;; #v has been read
  (if (and (regexp-try-match #rx"^u8" port)
           (eq? #\( (peek-char port)))
      (let* ([l (if stx?
                    (read-syntax/recursive src port)
                    (read/recursive port))]
             [lst (if stx?
                      (syntax->list l)
                      l)])
        (unless (list? lst)
          (raise-read-error
           (format "expected a parenthesized sequence without `.' after `#vu8'")
           src line col pos (and pos 4)))
        (for-each (lambda (e)
                    (let ([elem (if stx?
                                    (syntax-e e)
                                    e)])
                      (unless (byte? elem)
                        (let ([msg (format "invalid byte-vector element (not an octet): ~e"
                                           elem)])
                          (if stx?
                              (raise-read-error
                               msg
                               (syntax-source e)
                               (syntax-line e)
                               (syntax-column e)
                               (syntax-position e)
                               (syntax-span e))
                              (raise-read-error
                               msg
                               #f #f #f #f #f))))))
                  lst)
        (list->bytes (if stx? (map syntax-e lst) lst)))
      (raise-read-error
       "`#v' to continue `#vu8('"
       src line
       col pos
       2)))

(define read-byte-string
  (case-lambda
   [(ch port)
    (do-read-byte-string #f ch port #f #f #f #f)]
   [(ch port src line col pos)
    (do-read-byte-string #t ch port src line col pos)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  strings

(define (read-a-string ch port src line col pos)
  (let ([content (regexp-match #rx"^(?:[^\"\\\\]|\\\\.)*(?:\"|\\\\?$)" port)])
    (let* ([bytes (car content)]
           [len (bytes-utf-8-length bytes)])
      ;; Check/convert escapes and <line ending>s
      (let ([pieces
             (let loop ([bpos 0])
               (let ([m (regexp-match-positions #rx"(?:\r\n|\r\u85|[\r\u85\u2028])|\\\\." bytes bpos)])
                 (if m
                     (if (= (bytes-ref bytes (caar m)) (char->integer #\\))
                         (let ([char (string-ref
                                      (bytes->string/utf-8 (subbytes bytes (add1 (caar m)) (cdar m)))
                                      0)])
                           (cond
                            [(memq char '(#\a #\b #\t #\n #\v #\f #\r #\\ #\"))
                             (list* (subbytes bytes bpos (caar m))
                                    (case char
                                      [(#\n) #"\n"]
                                      [(#\r) #"\r"]
                                      [(#\t) #"\t"]
                                      [(#\a) #"\a"]
                                      [(#\b) #"\b"]
                                      [(#\v) #"\v"]
                                      [(#\f) #"\f"]
                                      [(#\\) #"\\"]
                                      [(#\") #"\""])
                                    (loop (cdar m)))]
                            [(eq? char #\x)
                             (let ([hm (regexp-match-positions #px"^[a-zA-Z0-9]*;"
                                                               bytes 
                                                               (+ 2 (caar m)))])
                               (if hm
                                   (let* ([hex-bytes (subbytes bytes (+ 2 (caar m)) (sub1 (cdar hm)))]
                                          [v (string->number (bytes->string/utf-8 hex-bytes) 16)])
                                     (if (or (<= 0 v #xD7FF)
                                             (<= #xE000 v #x10FFFF))
                                         (list* (subbytes bytes bpos (caar m))
                                                (string->bytes/utf-8 (string (integer->char v)))
                                                (loop (cdar hm)))
                                         (raise-read-error
                                          (format "out-of-range `\\x~a;' escape in string" hex-bytes)
                                          src line
                                          col pos
                                          (and pos (+ 1 len)))))
                                   (raise-read-error
                                    (format "bad escape `\\x~a' in string"
                                            (car (regexp-match #px"^[a-zA-Z0-9]*." bytes (+ 2 (caar m)))))
                                    src line
                                    col pos
                                    (and pos (+ 1 len)))))]
                            [(or (eq? char #\tab)
                                 (eq? char #\newline)
                                 (eq? char #\return)
                                 (eq? char #\u85)
                                 (eqv? char #\u2028)
                                 (eq? (char-general-category char) 'zs))
                             (let ([wm (regexp-match-positions #px"^(?:\t|\\p{Zs})*(?:\r\n|\r\u85|[\r\n\u85\u2028])(?:\t|\\p{Zs})*"
                                                               bytes 
                                                               (add1 (caar m)))])
                               (if wm
                                   (cons (subbytes bytes bpos (caar m)) ; drop matched part
                                         (loop (cdar wm)))
                                   ;; This is an eof error if there's only intraline whitespace
                                   ((if (regexp-match? #px"^(?:\t|\\p{Zs})*$" bytes (+ 1 bpos))
                                        raise-read-eof-error
                                        raise-read-error)
                                    "missing <line ending> after `\\<intraline-whitespace>'"
                                    src line
                                    col pos
                                    (and pos (+ 1 len)))))]
                            [else
                             (raise-read-error
                              (format "bad escape `\\~a' in string" char)
                              src line
                              col pos
                              (and pos (+ 1 len)))]))
                         ;; found a <line ending> that isn't just a newline:
                         (list* (subbytes bytes bpos (caar m))
                                #"\n"
                                (loop (cdar m))))
                     (let ([end (sub1 (bytes-length bytes))])
                       (if (or (= end -1)
                               (not (= (char->integer #\")
                                       (bytes-ref bytes end))))
                           (raise-read-error
                            "unexpected end-of-file within string"
                            src line col pos (and pos (+ 1 len)))
                           ;; Ok:
                           (list (subbytes bytes bpos end)))))))])
        (bytes->string/utf-8
         (if (= 1 (length pieces))
             (car pieces)
             (apply bytes-append pieces)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  numbers and symbols

(define-values (rx:id rx:number)
  ;; Build regexp versions of the R6RS grammar productions for
  ;; <identifier> and <number>.
  (let ([or (lambda args
              (and (ormap values args)
                   (apply
                    string-append
                    (append
                     (list "(?:")
                     (cdr (apply append
                                 (map (lambda (a) (list "|" a))
                                      (filter values args))))
                     (list ")")))))]
        [seq (lambda args
               (and (andmap values args)
                    (apply string-append args)))]
        [+ (lambda (s) (and s (string-append s "+")))]
        [* (lambda (s) (and s (string-append s "*")))])

    (define letter "[a-zA-Z]")
    (define constituent (or letter
                            (string-append
                             "(?:(?=[^\0-\177])(?:"
                             (substring
                              (apply
                               string-append
                               (map
                                (lambda (s)
                                  (format "|\\p{~a}" s))
                                '(Ll Lu Lt Lm Lo Mn Nl No Pd Pc Po Sc Sm Sk So Co)))
                              1)
                             "))")))
    (define special-initial "[!$%&*/:<=>?^_~]")
    (define special-subsequent "[-+.@]")
    (define inline-hex-escape "\\\\x[0-9a-fA-F]+;")
    (define initial (or constituent
                        special-initial
                        inline-hex-escape))
    (define subsequent (or initial
                           "[0-9]"
                           special-subsequent
                           "(?:\\p{Nd}|\\p{Mc}|\\p{Me})"))
    (define peculiar-identifier (or "[+]" 
                                    "-" 
                                    "[.][.][.]"
                                    (seq "->" (* subsequent))))
    (define identifier (or (seq initial (* subsequent))
                           peculiar-identifier))

    (define digit-16  "[0-9a-fA-F]")
    (define digit-10  "[0-9]")
    (define digit-8   "[0-7]")
    (define digit-2   "[01]")
    (define (digit R)
      (case R
        [(2) digit-2]
        [(8) digit-8]
        [(10) digit-10]
        [(16) digit-16]))
    (define radix-16  "#[xX]")
    (define radix-10  "(?:#[dD]|)")
    (define radix-8   "#[oO]")
    (define radix-2   "#[bB]")
    (define (radix R)
      (case R
        [(2) radix-2]
        [(8) radix-8]
        [(10) radix-10]
        [(16) radix-16]))
    (define exactness "(?:#[iIeE]|)")
    (define sign      "(?:[+-]|)")
    (define mantissa-width (or "" (seq "[|]" (+ digit-10))))
    (define exponent-marker "[eEsSfFdDlL]")
    (define suffix (or "" (seq exponent-marker sign (+ digit-10))))

    (define (prefix R) (or (seq (radix R) exactness)
                           (seq exactness (radix R))))
    (define (uinteger R) (+ (digit R)))
    (define decimal-10 (or (seq (uinteger 10) suffix)
                           (seq "[.]" (+ (digit 10)) suffix)
                           (seq (+ (digit 10)) "[.]" (* (digit 10)) suffix))) ; removed redundant last production
    (define (decimal R)
      (case R
        [(10) decimal-10]
        [else #f])) ; <<--- using #f to mean "can't match", and combinators propagate #f appropriately
    (define (ureal R) (or (uinteger R)
                          (seq (uinteger R) "/" (uinteger R))
                          (seq (decimal R) mantissa-width)))
    (define naninf (or "[nN][aA][nN][.]0" "[iI][nN][fF][.]0"))
    (define (real R) (or (seq sign (ureal R))
                         (seq "[+]" naninf)
                         (seq "-" naninf)))
    (define (complex R) (or (real R)
                            (seq (real R) "@" (real R))
                            (seq (real R) "[+]" (ureal R) "[iI]")
                            (seq (real R) "-" (ureal R) "[iI]")
                            (seq (real R) "[+]" naninf "[iI]")
                            (seq (real R) "-" naninf "[iI]")
                            (seq (real R) "[+][iI]")
                            (seq (real R) "-[iI]")
                            (seq "[+]" (ureal R) "[iI]")
                            (seq "-" (ureal R) "[iI]")
                            (seq "[+]" naninf "[iI]")
                            (seq "-" naninf "[iI]")
                            "[+][iI]"
                            "-[iI]"))
    (define (num R) (seq (prefix R) (complex R)))
    (define number (or (num 10)
                       (num 16)
                       (num 8)
                       (num 2)))

    (values (delay (pregexp (string-append "^" identifier "$")))
            (delay (pregexp (string-append "^" number "$"))))))

(define (do-read-symbol-or-number num? prefix port src line col pos)
  ;; Read a delimited sequence (using an extended notion of delimiter),
  ;; then make sure it's a number or identifier.
  (let ([thing (string-append
                prefix
                (bytes->string/utf-8
                 (car (or (if (string=? prefix "\\")
                              (regexp-match #px"^x[0-9a-fA-F]+;(?:\\\\x[0-9a-fA-F]+;|[^\\\\\\s\\[\\]()#\";,'`])*" port)
                              (regexp-match #px"^(?:#[xXdDbBoOeEiI])*(?:\\\\x[0-9a-fA-F]+;|[^\\\\\\s\\[\\]()#\";,'`])*" port))
                          '(#"")))))])
    (cond
     [(regexp-match? #rx"^[a-zA-Z!$%&*/:<=>?^_~][a-zA-Z0-9+!$%&*/:<=>?^_~.@-]*$" thing)
      ;; Simple symbol:
      (string->symbol thing)]
     [(regexp-match? (force rx:number) thing)
      (let ([n (string->number 
                ;; Racket doesn't handle mantissa widths, yet, so strip them out:
                (regexp-replace* #rx"[|][0-9]+" thing ""))])
        (unless n
          (error 'r6rs-parser "number didn't convert: ~e" thing))
        n)]
     [(and (not num?)
           (regexp-match? (force rx:id) thing))
      (string->symbol
       (bytes->string/utf-8 
        (let loop ([t (string->bytes/utf-8 thing)])
          (let ([m (regexp-match #rx#"^(.*)\\\\x([0-9a-fA-F]+);(.*)$" t)])
            (if m
                (loop (bytes-append
                       (loop (cadr m))
                       (let ([v (string->number
                                 (bytes->string/latin-1 (caddr m))
                                 16)])
                         (unless (or (<= 0 v #xD7FF)
                                     (<= #xE000 v #x10FFFF))
                           (let ([str (bytes->string/utf-8 thing)])
                             (raise-read-error
                              (format "out of range escape: `\\x~a;'" (cadr m))
                              src line col pos (and pos (string-length str)))))
                         (string->bytes/utf-8 (string (integer->char v))))
                       (loop (cadddr m))))
                t)))))]
     [else
      (raise-read-error
       (format "not a number or identifier: `~a'" thing)
       src line col pos (and pos (string-length thing)))])))


(define (read-symbol-or-number ch port src line col pos)
  (do-read-symbol-or-number #f
                            (string ch)
                            port src line col pos))

(define (read-number ch port src line col pos)
  (do-read-symbol-or-number #t
                            (string #\# ch)
                            port src line col pos))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define r6rs-readtable
  (make-readtable 
   #f
   #\{ 'terminating-macro not-allowed
   #\} 'terminating-macro not-allowed
   #\| 'terminating-macro not-allowed
   #\" 'terminating-macro read-a-string
   #\{ 'dispatch-macro dispatch-not-allowed
   #\\ 'dispatch-macro read-character
   #\" 'dispatch-macro dispatch-not-allowed
   #\% 'dispatch-macro dispatch-not-allowed
   #\: 'dispatch-macro dispatch-not-allowed
   #\& 'dispatch-macro dispatch-not-allowed
   #\! 'dispatch-macro read-hash-bang
   #\~ 'dispatch-macro dispatch-not-allowed
   #\< 'dispatch-macro dispatch-not-allowed
   #\r 'dispatch-macro dispatch-not-allowed
   #\p 'dispatch-macro dispatch-not-allowed
   #\c 'dispatch-macro dispatch-not-allowed
   #\C 'dispatch-macro dispatch-not-allowed
   #\s 'dispatch-macro dispatch-not-allowed
   #\S 'dispatch-macro dispatch-not-allowed
   #\h 'dispatch-macro dispatch-not-allowed
   #\r 'dispatch-macro dispatch-not-allowed
   #\l 'dispatch-macro dispatch-not-allowed
   #\0 'dispatch-macro dispatch-not-allowed
   #\1 'dispatch-macro dispatch-not-allowed
   #\2 'dispatch-macro dispatch-not-allowed
   #\3 'dispatch-macro dispatch-not-allowed
   #\4 'dispatch-macro dispatch-not-allowed
   #\5 'dispatch-macro dispatch-not-allowed
   #\6 'dispatch-macro dispatch-not-allowed
   #\8 'dispatch-macro dispatch-not-allowed
   #\9 'dispatch-macro dispatch-not-allowed
   #\t 'dispatch-macro read-boolean
   #\T 'dispatch-macro read-boolean
   #\f 'dispatch-macro read-boolean
   #\F 'dispatch-macro read-boolean
   #\v 'dispatch-macro read-byte-string
   #\i 'dispatch-macro read-number
   #\I 'dispatch-macro read-number
   #\e 'dispatch-macro read-number
   #\E 'dispatch-macro read-number
   #\b 'dispatch-macro read-number
   #\B 'dispatch-macro read-number
   #\d 'dispatch-macro read-number
   #\D 'dispatch-macro read-number
   #\o 'dispatch-macro read-number
   #\O 'dispatch-macro read-number
   #\x 'dispatch-macro read-number
   #\X 'dispatch-macro read-number
   #\\ 'terminating-macro read-symbol-or-number
   #f 'non-terminating-macro read-symbol-or-number
   ))

(define (with-r6rs-reader-parameters thunk)
  (parameterize ([current-readtable r6rs-readtable]
                 [read-accept-infix-dot #f])
    (thunk)))
