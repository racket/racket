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
    (unless (exact-nonnegative-integer? digits)
      (raise-type-error 'real->decimal-string "exact-nonnegative-integer" n))
    (let* ([e (expt 10 digits)]
           [num (round (abs (* e (inexact->exact n))))])
      (format "~a~a.~a"
              (if (or (negative? n) 
                      (equal? n -0.0))
                  "-" 
                  "")
              (quotient num e)
              (if (zero? digits)
                  ""
                  (let ([s (number->string (remainder num e))])
                    (if (= (string-length s) digits)
                        s
                        (string-append (make-string (- digits (string-length s)) #\0)
                                       s)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexp utilities

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

  (define (make-regexp-tweaker tweaker)
    (let ([t (make-weak-hasheq)])
      (lambda (rx)
        (define-syntax-rule (->str x) (if (bytes? x) (bytes->string/utf-8 x) x))
        (define-syntax-rule (->bts x) (if (bytes? x) x (string->bytes/utf-8 x)))
        (define-syntax-rule (tweak unwrap wrap convert)
          (let ([tweaked (tweaker (unwrap rx))])
            ;; the tweaker is allowed to return a regexp
            (if (or (regexp? tweaked) (byte-regexp? tweaked))
              tweaked
              (wrap (convert tweaked)))))
        (define (run-tweak)
          (cond [(pregexp? rx)      (tweak object-name pregexp ->str)]
                [(regexp?  rx)      (tweak object-name regexp  ->str)]
                [(byte-pregexp? rx) (tweak object-name byte-pregexp ->bts)]
                [(byte-regexp?  rx) (tweak object-name byte-regexp  ->bts)]
                ;; allow getting a string, so if someone needs to go
                ;; from a string to a regexp, there's no penalty
                ;; because of the intermediate regexp being recreated
                [(string? rx) (tweak (lambda (x) x) regexp      ->str)]
                [(bytes?  rx) (tweak (lambda (x) x) byte-regexp ->bts)]
                [else (raise-type-error
                       'regexp-tweaker
                       "regexp, byte regexp, string, or byte string"
                       rx)]))
        (or (hash-ref t rx #f)
            (let ([rx* (run-tweak)]) (hash-set! t rx rx*) rx*)))))

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
             ;; Get the matching part, and shift matching indices
             (let ([s (read-bytes (- (cdar m) drop) input-port)])
               (cons s
                     (map (lambda (p)
                            (and p (subbytes s (- (car p) drop) (- (cdr p) drop))))
                          (cdr m))))))))

  ;; Helper macro for the regexp functions below, with some utilities.
  (define (bstring-length s)
    (if (bytes? s) (bytes-length s) (string-length s)))
  (define (no-empty-edge-matches n)
    (make-regexp-tweaker (lambda (rx) 
                           (if (bytes? rx)
                               (bytes-append #"(?=.)(?:" rx #")(?<=" (make-bytes n (char->integer #\.)) #")")
                               (format "(?=.)(?:~a)(?<=~a)" rx (make-bytes n (char->integer #\.)))))))
  (define-syntax-rule (regexp-loop
                       name loop start end pattern string
                       ipre
                       success-choose failure-k
                       port-success-k port-success-choose port-failure-k
                       need-leftover? peek?)
    (let* ([len (cond [(string? string) (string-length string)]
                      [(bytes?  string) (bytes-length  string)]
                      [else #f])]
           [orig-rx (cond [(bytes? pattern) (byte-regexp pattern)]
                          [(string? pattern) (regexp pattern)]
                          [(regexp? pattern) pattern]
                          [(byte-regexp? pattern) pattern]
                          [else
                           (raise-type-error 'name
                                             "regexp, byte regexp, string, or byte string" 
                                             pattern)])]
           [max-lookbehind (regexp-max-lookbehind orig-rx)])
      (if peek?
        (unless (input-port? string)
          (raise-type-error 'name "input port" string))
        (unless (or len (input-port? string))
          (raise-type-error
           'name "string, byte string or input port" string)))
      (unless (and (number? start) (exact? start) (integer? start)
                   (start . >= . 0))
        (raise-type-error 'name "non-negative exact integer" start))
      (unless (or (not end)
                  (and (number? end) (exact? end) (integer? end)
                       (end . >= . 0)))
        (raise-type-error 'name "non-negative exact integer or false" end))
      (unless (bytes? ipre)
        (raise-type-error 'name "byte string" ipre))
      (unless (or (input-port? string) (and len (start . <= . len)))
        (raise-mismatch-error
         'name
         (format "starting offset index out of range [0,~a]: " len)
         start))
      (unless (or (not end)
                  (and (start . <= . end)
                       (or (input-port? string) (and len (end . <= . len)))))
        (raise-mismatch-error
         'name
         (format "ending offset index out of range [~a,~a]: " start len)
         end))
      (reverse
       (let loop ([acc '()] [start start] [end end] [ipre ipre] [rx #f] [rx-lb 0])
         (let* ([new-rx-lb (add1 (bytes-length ipre))]
                [rx (if (= rx-lb new-rx-lb)
                        rx
                        ((no-empty-edge-matches new-rx-lb) orig-rx))])
           (if (and port-success-choose (input-port? string))

               ;; Input port match, get string
               (let* ([_ (when (positive? start)
                           ;; Skip start chars:
                           (let ([s (make-bytes 4096)])
                             (let loop ([n 0])
                               (unless (= n start)
                                 (let ([m (read-bytes-avail!
                                           s string 0 (min (- start n) 4096))])
                                   (unless (eof-object? m) (loop (+ n m))))))))]
                      [discarded/leftovers (if need-leftover? #f 0)]
                      [spitout (if need-leftover?
                                   (open-output-bytes)
                                   (make-output-port
                                    'counter always-evt
                                    (lambda (s start end flush? breakable?)
                                      (let ([c (- end start)])
                                        (set! discarded/leftovers
                                              (+ c discarded/leftovers))
                                        c))
                                    void))]
                      [end (and end (- end start))])
                 (let-values ([(m ipre) (regexp-match/end rx string 0 end spitout ipre
                                                          max-lookbehind)])
                   (let* ([m (and m (car m))]
                          [discarded/leftovers (if need-leftover?
                                                   (get-output-bytes spitout)
                                                   discarded/leftovers)]
                          [end (and end m
                                    (- end (if need-leftover?
                                               (bstring-length discarded/leftovers)
                                               discarded/leftovers)
                                       (bstring-length m)))])
                     (if m
                         (loop (cons (port-success-choose m discarded/leftovers) acc)
                               0 end ipre
                               rx new-rx-lb)
                         (port-failure-k acc discarded/leftovers)))))
               
               ;; String/port match, get positions
               (let-values ([(m ipre)
                             (if peek?
                                 (regexp-match-peek-positions/end rx string start end #f ipre
                                                                  max-lookbehind)
                                 (regexp-match-positions/end rx string start end #f ipre
                                                             max-lookbehind))])
                 
                 (if (not m)
                     (failure-k acc start end)
                     (let ([mstart (caar m)] [mend (cdar m)])
                       (if port-success-k
                           (port-success-k
                            (lambda (acc new-start new-end)
                              (loop acc new-start new-end ipre rx new-rx-lb))
                            acc start end mstart mend)
                           (loop (cons (success-choose start mstart mend) acc)
                                 mend end ipre rx new-rx-lb)))))))))))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-positions* pattern string [start 0] [end #f] [ipre #""])
    (regexp-loop
     regexp-match-positions* loop start end
     pattern string
     ipre
     ;; success-choose:
     (lambda (start mstart mend) (cons mstart mend))
     ;; failure-k:
     (lambda (acc start end) acc)
     ;; port-success-k: need to shift index of rest as reading; cannot
     ;; do a tail call without adding another state variable to the
     ;; regexp loop, so this remains inefficient
     (and (input-port? string)
          (lambda (loop acc start end mstart mend)
            (append (map (lambda (p)
                           (cons (+ mend (car p)) (+ mend (cdr p))))
                         (loop '() 0 (and end (- end mend))))
                    (cons (cons mstart mend) acc))))
     ;; other port functions: use string case
     #f
     #f
     #f
     #f))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-peek-positions* pattern string [start 0] [end #f] [ipre #""])
    (regexp-loop
     regexp-match-peek-positions* loop start end
     pattern string
     ipre
     ;; success-choose:
     (lambda (start mstart mend) (cons mstart mend))
     ;; failure-k:
     (lambda (acc start end) acc)
     ;; port functions: use string case
     #f
     #f
     #f
     #f
     #t))

  ;; Splits a string into a list by removing any piece which matches
  ;; the pattern.
  (define (regexp-split pattern string [start 0] [end #f] [ipre #""])
    (define buf (if (and (string? string) (or (byte-regexp? pattern)
                                              (bytes? pattern)))
                  (string->bytes/utf-8 string (char->integer #\?))
                  string))
    (define sub (if (bytes? buf) subbytes substring))
    (regexp-loop regexp-split loop start end pattern buf ipre
     ;; success-choose:
     (lambda (start mstart mend) (sub buf start mstart))
     ;; failure-k:
     (lambda (acc start end)
       (cons (if end (sub buf start end) (sub buf start)) acc))
     ;; port-success-k:
     #f
     ;; port-success-choose:
     (lambda (match-string leftovers) leftovers)
     ;; port-failure-k:
     (lambda (acc leftover) (if leftover (cons leftover acc) acc))
     #t
     #f))

  ;; Returns all the matches for the pattern in the string.
  (define (regexp-match* pattern string [start 0] [end #f] [ipre #""])
    (define buf (if (and (string? string) (or (byte-regexp? pattern)
                                              (bytes? pattern)))
                  (string->bytes/utf-8 string (char->integer #\?))
                  string))
    (define sub (if (bytes? buf) subbytes substring))
    (regexp-loop regexp-match* loop start end pattern buf ipre
     ;; success-choose:
     (lambda (start mstart mend) (sub buf mstart mend))
     ;; failure-k:
     (lambda (acc start end) acc)
     ;; port-success-k:
     #f
     ;; port-success-choose:
     (lambda (match-string leftovers) match-string)
     ;; port-failure-k:
     (lambda (acc leftover) acc)
     #f
     #f))

  (define (regexp-match-exact? p s)
    (let ([m (regexp-match-positions p s)])
      (and m (zero? (caar m))
           (= (cdar m)
              (cond [(bytes? s) (bytes-length s)]
                    [(or (byte-regexp? p) (bytes? p)) (string-utf-8-length s)]
                    [else (string-length s)])))))

  )
