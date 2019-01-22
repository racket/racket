#lang racket/base
(require (prefix-in rx: "main.rkt"))

(define-syntax-rule (test expr v)
  (let ([b expr])
    (unless (equal? b v)
      (error 'test "failed: ~s => ~s" 'expr b))))

(test (rx:regexp-match "" (open-input-string "123") 4)
      #f)
(test (rx:regexp-match-peek "" (open-input-string "123") 4)
      #f)

(for* ([succeed? '(#f #t)]
       [char '(#\x #\u3BB)])
  (for ([N '(1 100 1000 1023 1024 10000)])
    (for ([M (list 0 (quotient N 2))])
      (define o (open-output-bytes))
      (log-error "N = ~a, M = ~a" N M)
      (void (rx:regexp-match-positions "y" 
                                       (string-append
                                        (make-string N char)
                                        (if succeed? "y" ""))
                                       M
                                       (+ N (if succeed? 1 0))
                                       o))
      (test (string-length (get-output-string o)) (- N M)))))

;; Test bounded byte consumption on failure:
(let ([is (open-input-string "barfoo")]) 
  (test (list (rx:regexp-match "^foo" is 0 3) (read-char is)) '(#f #\f)))
(let ([is (open-input-string "barfoo")]) 
  (test (list (rx:regexp-match "foo" is 0 3) (read-char is)) '(#f #\f)))

;; Don't consume bytes that corresponds to a prefix:
(let ()
  (define in (open-input-string "a\nb\nc\n"))
  (define rx:.n (rx:byte-regexp #"(?m:^.\n)"))
  (test (rx:regexp-match rx:.n in 0 #f #f #"") '(#"a\n"))
  (test (rx:regexp-match rx:.n in 0 #f #f #"\n") '(#"b\n"))
  (test (rx:regexp-match rx:.n in 0 #f #f #"\n") '(#"c\n")))

(let ()
  (define in (open-input-bytes #" a b c "))

  (define discard (open-output-bytes))
  (rx:regexp-match "[abc]" in 0 3 discard #"")
  (test (get-output-bytes discard) #" ")

  (define discard2 (open-output-bytes))
  (rx:regexp-match "[abc]" in 0 1 discard2 #"")
  (test (get-output-bytes discard2) #" "))

;; Input streams that are large enough for bytes to be discarded along the way
(test (rx:regexp-match #"(.)x" (open-input-string (string-append (make-string 50000 #\y) "x")))
      '(#"yx" #"y"))
(test (rx:regexp-match-positions #"(.)x" (open-input-string (string-append (make-string 50000 #\y) "x")))
      '((49999 . 50001) (49999 . 50000)))
(test (rx:regexp-match "(.)x" (string-append (make-string 50000 #\y) "x"))
      '("yx" "y"))
(test (rx:regexp-match-positions "(.)x" (string-append (make-string 50000 #\y) "x"))
      '((49999 . 50001) (49999 . 50000)))
(test (rx:regexp-match "(.)\u3BC" (string-append (make-string 50000 #\u3BB) "\u3BC"))
      '("\u3BB\u3BC" "\u3BB"))
(test (rx:regexp-match-positions "(.)\u3BC" (string-append (make-string 50000 #\y) "\u3BC"))
      '((49999 . 50001) (49999 . 50000)))

(test (rx:regexp-match-positions #"<([abc])(>)?" "<a + <b = <c" 3)
      '((5 . 7) (6 . 7) #f))
(test (rx:regexp-match-positions "[abc]" " a b c " 2)
      '((3 . 4)))
(test (rx:regexp-match-positions "(?m:^.\n)" "a\nb\nc\n" 2 6 #f #"\n")
      '((2 . 4)))
(test (rx:regexp-match-positions "(?:(?m:^$))(?<=..)" "ge \n TLambda-tc\n\n ;; (extend Γ o Γx-s\n extend\n\n ;;" 29 #f #f #"\n")
      '((46 . 46)))

(test (regexp-replace* "-" "zero-or-more?" "_")
      "zero_or_more?")

;; ----------------------------------------

(define (check rx in N [M (max 1 (quotient N 10))])
  (define c-start (current-inexact-milliseconds))
  (define orig-rx
    (if (bytes? rx)
        (for/fold ([r #f]) ([i (in-range M)])
          (byte-pregexp rx))
        (for/fold ([r #f]) ([i (in-range M)])
          (pregexp rx))))
  (define c-after-orig (current-inexact-milliseconds))
  (define new-rx
    (if (bytes? rx)
        (for/fold ([r #f]) ([i (in-range M)])
          (rx:byte-pregexp rx))
        (for/fold ([r #f]) ([i (in-range M)])
          (rx:pregexp rx))))
  (define c-after-new (current-inexact-milliseconds))

  (define orig-v (regexp-match orig-rx in))
  (define new-v (rx:regexp-match new-rx in))
  (unless (equal? orig-v new-v)
    (error 'check
           "failed\n  pattern: ~s\n  input: ~s\n  expected: ~s\n  got: ~s"
           rx in orig-v new-v))

  (define start (current-inexact-milliseconds))
  (for/fold ([r #f]) ([i (in-range N)])
    (regexp-match? orig-rx in))
  (define after-orig (current-inexact-milliseconds))
  (for/fold ([r #f]) ([i (in-range N)])
    (rx:regexp-match? new-rx in))
  (define after-new (current-inexact-milliseconds))
  
  (define orig-c-msec (- c-after-orig c-start))
  (define new-c-msec (- c-after-new c-after-orig))
  (define orig-msec (- after-orig start))
  (define new-msec (- after-new after-orig))
  
  (unless (= N 1)
    (parameterize ([error-print-width 64])
      (printf "regex: ~.s\non: ~.s\n" rx in))
    
    (define (~n n)
      (car (regexp-match #px"^[0-9]*[.]?[0-9]{0,2}" (format "~a" n))))
    
    (printf " compile: ~a  (~a vs. ~a) / ~a iterations\n"
            (~n (/ new-c-msec orig-c-msec))
            (~n orig-c-msec)
            (~n new-c-msec)
            M)
    (printf " interp:  ~a  (~a vs. ~a) / ~a iterations\n"
            (~n (/ new-msec orig-msec))
            (~n orig-msec)
            (~n new-msec)
            N)))

;; ----------------------------------------

(check #"(?m:^aa$a.)"
       #"abaac\nac\naa\nacacaaacd"
       1)

(check #"\\sa."
       #"cat apple"
       1)

(check "(?>a*)a"
       "aaa"
       1)

(check "(?:a|b)y(\\1)"
       "ayb"
       1)

(check "!.!"
       #"!\x80!"
       1)

(check #"\\P{Ll}"
       #"aB"
       1)

(check #".*"
       #"abaacacaaacacaaacd"
       100000)

(check #"ab(?:a*c)*d"
       #"abaacacaaacacaaacd"
       100000)

(check #"ab(?:a*?c)*d"
       #"abaacacaaacacaaacd"
       100000)

(check #"ab(?:[ab]*c)*d"
       #"abaacacaaacacaaacd"
       100000)

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

(check (string->bytes/utf-8 url-s)
       (string->bytes/utf-8 rlo)
       100000)

(check url-s
       rlo
       10000)

;; all of the work is looking for a must-string
(check #"a*b"
       (make-bytes 1024 (char->integer #\a))
       100000)
