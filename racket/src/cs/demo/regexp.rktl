(define (check pat str)
  (write
   (time
    (let ([rx (byte-regexp (string->bytes/utf-8 pat))]
          [in (string->bytes/utf-8 str)])
      (let loop ([v #f] [n 100000])
        (if (zero? n)
            v
            (loop (regexp-match rx in)
                  (sub1 n)))))))
  (newline))

;; A smallish backtracking test, more or less:
(check "ab(?:a*c)*d" "abaacacaaacacaaacd")

;; Relatively realitic workload:
(define ipv6-hex "[0-9a-fA-F:]*:[0-9a-fA-F:]*")
(define url-s
  (string-append
   "^"
   "(?:"              ; / scheme-colon-opt
   "([^:/?#]*)"       ; | #1 = scheme-opt
   ":)?"              ; \
   "(?://"            ; / slash-slash-authority-opt
   "(?:"              ; | / user-at-opt
   "([^/?#@]*)"       ; | | #2 = user-opt
   "@)?"              ; | \
   "(?:"              ;
   "(?:\\["           ; | / #3 = ipv6-host-opt
   "(" ipv6-hex ")"   ; | | hex-addresses
   "\\])|"            ; | \
   "([^/?#:]*)"       ; | #4 = host-opt
   ")?"               ;
   "(?::"             ; | / colon-port-opt
   "([0-9]*)"         ; | | #5 = port-opt
   ")?"               ; | \
   ")?"               ; \
   "([^?#]*)"         ; #6 = path
   "(?:\\?"           ; / question-query-opt
   "([^#]*)"          ; | #7 = query-opt
   ")?"               ; \
   "(?:#"             ; / hash-fragment-opt
   "(.*)"             ; | #8 = fragment-opt
   ")?"               ; \
   "$"))
(define rlo "https://racket-lang.org:80x/people.html?check=ok#end")
(check url-s rlo)

;; A test of scanning a byte string to look for the letter "b"
;; (where a tight loop in C is likely to win):
(check "a*b" (make-string 1024 #\a))
