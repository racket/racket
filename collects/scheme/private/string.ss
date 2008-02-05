(module string "pre-base.ss"

  (provide real->decimal-string
           regexp-quote
           regexp-replace-quote
           regexp-match*
           regexp-match-positions*
           regexp-match-peek-positions*
           regexp-split
           regexp-match-exact?
           regexp-try-match)
  (require (for-syntax "stxcase-scheme.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (real->decimal-string n [digits 2])
    (let* ([e (expt 10 digits)]
           [num (round (abs (* e (inexact->exact n))))])
      (format "~a~a.~a"
              (if (negative? n) "-" "")
              (quotient num e)
              (let ([s (number->string (remainder num e))])
                (if (= (string-length s) digits)
                  s
                  (string-append (make-string (- digits (string-length s)) #\0)
                                 s))))))
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexp helpers

  (define (bstring-length s)
    (if (bytes? s) (bytes-length s) (string-length s)))

  (define (bstring->regexp name pattern)
    (cond [(regexp? pattern)      pattern]
          [(byte-regexp? pattern) pattern]
          [(string? pattern) (regexp      pattern)]
          [(bytes?  pattern) (byte-regexp pattern)]
          [else (raise-type-error
                 name "regexp, byte regexp, string, or byte string" pattern)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexp helpers

  (define regexp-quote-chars:s #rx"[][.*?+|(){}\\$^]")
  (define regexp-quote-chars:b #rx#"[][.*?+|(){}\\$^]")

  (define (regexp-quote s [case-sens? #t])
    (let* ([b? (cond [(bytes? s) #t]
                     [(string? s) #f]
                     [else (raise-type-error 'regexp-quote
                                             "string or byte string" s)])]
           [s (if b?
                (regexp-replace* regexp-quote-chars:b s #"\\\\&")
                (regexp-replace* regexp-quote-chars:s s "\\\\&"))])
      (cond [case-sens? s]
            [b?   (bytes-append #"(?i:" s #")")]
            [else (string-append "(?i:" s ")")])))

  (define (regexp-replace-quote s)
    (cond [(bytes?  s) (regexp-replace* #rx#"[&\\]" s #"\\\\&")]
          [(string? s) (regexp-replace* #rx"[&\\]"  s "\\\\&")]
          [else (raise-type-error 'regexp-replace-quote
                                  "string or byte string" s)]))

  (define (regexp-try-match pattern input-port [start-k 0] [end-k #f] [out #f])
    (unless (input-port? input-port)
      (raise-type-error 'regexp-try-match
                        "input port" input-port))
    (unless (or (not out) (output-port? out))
      (raise-type-error 'regexp-try-match
                        "output port or #f" out))
    (let ([m (regexp-match-peek-positions pattern input-port start-k end-k)])
      (and m
           ;; What happens if someone swipes our bytes before we can get them?
           (let ([drop (caar m)])
             ;; drop prefix before match:
             (let ([s (read-bytes drop input-port)])
               (when out
                 (display s out)))
             ;; Get the matching part, and shift matching indicies
             (let ([s (read-bytes (- (cdar m) drop) input-port)])
               (cons s
                     (map (lambda (p)
                            (and p (subbytes s (- (car p) drop) (- (cdr p) drop))))
                          (cdr m))))))))

  ;; Helper function for the regexp functions below.
  (define-syntax make-regexp-loop
    (syntax-rules ()
      [(make-regexp-loop
        name rx string success-k port-success-k failure-k port-failure-k
        need-leftover? peek?)
       (let ([len (cond [(string? string) (string-length string)]
                        [(bytes?  string) (bytes-length  string)]
                        [else #f])])
         (if peek?
           (unless (input-port? string)
             (raise-type-error name "input port" string))
           (unless (or len (input-port? string))
             (raise-type-error name "string, byte string or input port" string)))
         (lambda (start end)
           (unless (and (number? start) (exact? start) (integer? start)
                        (start . >= . 0))
             (raise-type-error name "non-negative exact integer" start))
           (unless (or (not end)
                       (and (number? end) (exact? end) (integer? end)
                            (end . >= . 0)))
             (raise-type-error name "non-negative exact integer or false" end))
           (unless (or (input-port? string) (and len (start . <= . len)))
             (raise-mismatch-error
              name
              (format "starting offset index out of range [0,~a]: " len)
              start))
           (unless (or (not end)
                       (and (start . <= . end)
                            (or (input-port? string)
                                (and len (end . <= . len)))))
             (raise-mismatch-error
              name
              (format "ending offset index out of range [~a,~a]: " start len)
              end))

           (when (and (positive? start) (input-port? string) need-leftover?)
             ;; Skip start chars:
             (let ([s (make-bytes 4096)])
               (let loop ([n 0])
                 (unless (= n start)
                   (let ([m (read-bytes-avail!
                             s string 0 (min (- start n) 4096))])
                     (unless (eof-object? m) (loop (+ n m))))))))

           (if (and (input-port? string) port-success-k)
             ;; Input port match, get string
             (let ([discarded 0]
                   [leftover-port (and need-leftover? (open-output-bytes))])
               (let ([match
                      (regexp-match
                       rx string
                       (if need-leftover? 0 start)
                       (and end (if need-leftover? (- end start) end))
                       (if need-leftover?
                         leftover-port
                         (make-output-port
                          'counter
                          always-evt
                          (lambda (s start end flush? breakable?)
                            (let ([c (- end start)])
                              (set! discarded (+ c discarded))
                              c))
                          void)))]
                     [leftovers
                      (and need-leftover?
                           (if (and (regexp? rx) (string? string))
                             (get-output-string leftover-port)
                             (get-output-bytes leftover-port)))])
                 (if match
                   (port-success-k
                    (car match)
                    (and end (- end (if need-leftover?
                                      (+ (bstring-length leftovers) start)
                                      discarded)
                                (bstring-length (car match))))
                    leftovers)
                   (port-failure-k leftovers))))
             ;; String/port match, get positions
             (let ([match ((if peek?
                             regexp-match-peek-positions
                             regexp-match-positions)
                           rx string start end)])
               (if match
                 (let ([match-start (caar match)]
                       [match-end (cdar match)])
                   (if (= match-start match-end)
                     (error name
                            "pattern matched a zero-length substring: ~e" rx)
                     (success-k start end match-start match-end)))
                 (failure-k start end))))))]))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-positions* pattern string [start 0] [end #f])
    (define rx (bstring->regexp 'regexp-match-positions* pattern))
    (define loop
      (make-regexp-loop
       'regexp-match-positions* rx string
       ;; success-k:
       (lambda (start end match-start match-end)
         (cons (cons match-start match-end)
               (if (or (string? string) (bytes? string))
                 (loop match-end end)
                 ;; Need to shift index of rest as reading:
                 (map (lambda (p)
                        (cons (+ match-end (car p)) (+ match-end (cdr p))))
                      (loop 0 (and end (- end match-end)))))))
       ;; port-success-k: use string case
       #f
       ;; failure-k:
       (lambda (start end) null)
       ;; port-fail-k: use string case
       #f
       #f
       #f))
    (loop start end))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-peek-positions* pattern string [start 0] [end #f])
    (define rx (bstring->regexp 'regexp-match-peek-positions* pattern))
    (define loop
      (make-regexp-loop
       'regexp-match-peek-positions* rx string
       ;; success-k:
       (lambda (start end match-start match-end)
         (cons (cons match-start match-end)
               (loop match-end end)))
       ;; port-success-k: use string case
       #f
       ;; failure-k:
       (lambda (start end) null)
       ;; port-fail-k: use string case
       #f
       #f
       #t))
    (loop start end))

  ;; Splits a string into a list by removing any piece which matches
  ;; the pattern.
  (define (regexp-split pattern string [start 0] [end #f])
    (define rx (bstring->regexp 'regexp-split pattern))
    (define buf (if (and (string? string) (byte-regexp? rx))
                  (string->bytes/utf-8 string (char->integer #\?))
                  string))
    (define sub (if (bytes? buf) subbytes substring))
    (define loop
      (make-regexp-loop
       'regexp-split rx buf
       ;; success-k:
       (lambda (start end match-start match-end)
         (cons (sub buf start match-start)
               (loop match-end end)))
       ;; port-success-k:
       (lambda (match-string new-end leftovers)
         (cons leftovers (loop 0 new-end)))
       ;; failure-k:
       (lambda (start end)
         (list (sub buf start (or end (bstring-length buf)))))
       ;; port-fail-k
       (lambda (leftover) (list leftover))
       #t
       #f))
    (loop start end))

  ;; Returns all the matches for the pattern in the string.
  (define (regexp-match* pattern string [start 0] [end #f])
    (define rx (bstring->regexp 'regexp-match* pattern))
    (define buf (if (and (string? string) (byte-regexp? rx))
                  (string->bytes/utf-8 string (char->integer #\?))
                  string))
    (define sub (if (bytes? buf) subbytes substring))
    (define loop
      (make-regexp-loop
       'regexp-match* rx buf
       ;; success-k:
       (lambda (start end match-start match-end)
         (cons (sub buf match-start match-end)
               (loop match-end end)))
       ;; port-success-k:
       (lambda (match-string new-end leftovers)
         (cons match-string (loop 0 new-end)))
       ;; failure-k:
       (lambda (start end) null)
       ;; port-fail-k:
       (lambda (leftover) null)
       #f
       #f))
    (loop start end))

  (define (regexp-match-exact? p s)
    (let ([m (regexp-match-positions p s)])
      (and m (zero? (caar m))
           (= (cdar m)
              (cond [(bytes? s) (bytes-length s)]
                    [(or (byte-regexp? p) (bytes? p)) (string-utf-8-length s)]
                    [else (string-length s)])))))

  )
