(module string mzscheme
  (provide string-lowercase!
           string-uppercase!
           eval-string
           read-from-string
           read-from-string-all
           expr->string
           real->decimal-string
           regexp-quote
           regexp-replace-quote
           regexp-match*
           regexp-match-positions*
           regexp-match-peek-positions*
           regexp-split
           regexp-match-exact?
           regexp-match/fail-without-reading
           glob->regexp)

  (require (lib "kw.ss"))

  (define ((make-string-do! translate) s)
    (let loop ([n (sub1 (string-length s))])
      (unless (negative? n)
        (string-set! s n (translate (string-ref s n)))
        (loop (sub1 n)))))
  (define string-lowercase! (make-string-do! char-downcase))
  (define string-uppercase! (make-string-do! char-upcase))

  ;; helpers for eval-string and read-from-string-one-or-all
  (define-syntax wrap-errors
    (syntax-rules ()
      [(wrap-errors who error-handler body ...)
       (if error-handler
         (with-handlers
             ([void
               (cond [(not (procedure? error-handler))
                      (error who "bad error handler: ~e" error-handler)]
                     [(procedure-arity-includes? error-handler 1)
                      error-handler]
                     [(procedure-arity-includes? error-handler 0)
                      (lambda (exn) (error-handler))]
                     [else (error who "bad error handler: ~e" error-handler)])])
           body ...)
         (begin body ...))]))
  (define (open-input-bstring s)
    (if (bytes? s) (open-input-bytes s) (open-input-string s)))

  (define/kw (eval-string str #:optional error-handler)
    (wrap-errors 'eval-string error-handler
      (let ([p (open-input-bstring str)])
        (apply values
               (let loop ()
                 (let ([e (read p)])
                   (if (eof-object? e)
                     '()
                     (call-with-values
                       (lambda () (eval e))
                       (lambda vals (append vals (loop)))))))))))

  (define/kw (read-from-string str #:optional error-handler)
    (wrap-errors 'read-from-string error-handler
      (read (open-input-bstring str))))

  (define/kw (read-from-string-all str #:optional error-handler)
    (let ([p (open-input-bstring str)])
      (wrap-errors 'read-from-string-all error-handler
        (let loop ([r '()])
          (let ([v (read p)])
            (if (eof-object? v) (reverse! r) (loop (cons v r))))))))

  (define (expr->string v)
    (let ([port (open-output-string)])
      (write v port)
      (get-output-string port)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define/kw (real->decimal-string n #:optional [digits 2])
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

  (define (subbstring s st e)
    (if (bytes? s) (subbytes s st e) (substring s st e)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Regexp helpers

  (define regexp-quote-chars:s #rx"[][.*?+|(){}\\$^]")
  (define regexp-quote-chars:b #rx#"[][.*?+|(){}\\$^]")

  (define/kw (regexp-quote s #:optional [case-sens? #t])
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
    (let ([b? (cond [(bytes? s) #t]
                    [(string? s) #f]
                    [else (raise-type-error 'regexp-replace-quote
                                            "string or byte string" s)])])
      (if b?
        (regexp-replace* #rx#"[&\\]" s #"\\\\&")
        (regexp-replace* #rx"[&\\]"  s "\\\\&"))))

  (define/kw (regexp-match/fail-without-reading
              pattern input-port #:optional [start-k 0] [end-k #f] [out #f])
    (unless (input-port? input-port)
      (raise-type-error 'regexp-match/fail-without-reading
                        "input port" input-port))
    (unless (or (not out) (output-port? out))
      (raise-type-error 'regexp-match/fail-without-reading
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
  (define (regexp-fn name success-k port-success-k failure-k port-failure-k
                     need-leftover? peek?)
    (lambda (pattern string start end)
      (unless (or (string? pattern) (bytes? pattern)
                  (regexp? pattern) (byte-regexp? pattern))
        (raise-type-error name "regexp, byte regexp, string, or byte string" pattern))
      (if peek?
          (unless (input-port? string)
            (raise-type-error name "input port" string))
          (unless (or (string? string)
                      (bytes? string)
                      (input-port? string))
            (raise-type-error name "string, byte string or input port" string)))
      (unless (and (number? start) (exact? start) (integer? start) (start . >= . 0))
        (raise-type-error name "non-negative exact integer" start))
      (unless (or (not end)
                  (and (number? end) (exact? end) (integer? end) (end . >= . 0)))
        (raise-type-error name "non-negative exact integer or false" end))
      (unless (or (input-port? string)
                  (and (string? string)
                       (start . <= . (string-length string)))
                  (and (bytes? string)
                       (start . <= . (bytes-length string))))
        (raise-mismatch-error
         name
         (format "starting offset index out of range [0,~a]: "
                 (if (string? string)
                   (string-length string)
                   (bytes-length string)))
         start))
      (unless (or (not end)
                  (and (start . <= . end)
                       (or (input-port? string)
                           (and (string? string)
                                (end . <= . (string-length string)))
                           (and (bytes? string)
                                (end . <= . (bytes-length string))))))
        (raise-mismatch-error
         name
         (format "ending offset index out of range [~a,~a]: "
                 end
                 (if (string? string)
                   (string-length string)
                   (bytes-length string)))
         start))

      (when (and (positive? start) (input-port? string) need-leftover?)
        ;; Skip start chars:
        (let ([s (make-bytes 4096)])
          (let loop ([n 0])
            (unless (= n start)
              (let ([m (read-bytes-avail! s string 0 (min (- start n) 4096))])
                (unless (eof-object? m)
                  (loop (+ n m))))))))

      (let ([expr (cond [(string? pattern) (regexp pattern)]
                        [(bytes? pattern) (byte-regexp pattern)]
                        [else pattern])])
        (if (and (input-port? string) port-success-k)
          ;; Input port match, get string
          (let ([discarded 0]
                [leftover-port (and need-leftover? (open-output-bytes))])
            (let ([match
                   (regexp-match
                    expr string
                    (if need-leftover? 0 start)
                    (and end (if need-leftover? (- end start) end))
                    (if need-leftover?
                      leftover-port
                      (make-output-port 'counter
                                        always-evt
                                        (lambda (s start end flush? breakable?)
                                          (let ([c (- end start)])
                                            (set! discarded (+ c discarded))
                                            c))
                                        void)))]
                  [leftovers
                   (and need-leftover?
                        (if (and (regexp? pattern) (string? string))
                          (get-output-string leftover-port)
                          (get-output-bytes leftover-port)))])
                (if match
                  (port-success-k expr string (car match)
                                  (and end (- end
                                              (if need-leftover?
                                                (+ (bstring-length leftovers) start)
                                                discarded)
                                              (bstring-length (car match))))
                                  leftovers)
                  (port-failure-k leftovers))))
          ;; String/port match, get positions
          (let ([match ((if peek?
                          regexp-match-peek-positions
                          regexp-match-positions)
                        expr string start end)])
            (if match
              (let ([match-start (caar match)]
                    [match-end (cdar match)])
                (when (= match-start match-end)
                  (error name "pattern matched a zero-length substring"))
                (success-k expr string start end match-start match-end))
              (failure-k expr string start end)))))))

  (define-syntax wrap
    (syntax-rules ()
      [(_ out orig)
       (define/kw (out pattern string #:optional [start 0] [end #f])
         (orig pattern string start end))]))

  ;; Returns all the positions at which the pattern matched.
  (define -regexp-match-positions*
    (regexp-fn 'regexp-match-positions*
               ;; success-k:
               (lambda (expr string start end match-start match-end)
                 (cons (cons match-start match-end)
                       (if (or (string? string) (bytes? string))
                         (regexp-match-positions* expr string match-end end)
                         ;; Need to shift index of rest as reading:
                         (map (lambda (p)
                                (cons (+ match-end (car p))
                                      (+ match-end (cdr p))))
                              (regexp-match-positions* expr string 0 (and end (- end match-end)))))))
               ;; port-success-k --- use string case
               #f
               ;; fail-k:
               (lambda (expr string start end) null)
               ;; port-fail-k --- use string case
               #f
               #f
               #f))
  (wrap regexp-match-positions* -regexp-match-positions*)

  ;; Returns all the positions at which the pattern matched.
  (define -regexp-match-peek-positions*
    (regexp-fn 'regexp-match-peek-positions*
               ;; success-k:
               (lambda (expr string start end match-start match-end)
                 (cons (cons match-start match-end)
                       (regexp-match-peek-positions* expr string match-end end)))
               ;; port-success-k --- use string case
               #f
               ;; fail-k:
               (lambda (expr string start end) null)
               ;; port-fail-k --- use string case
               #f
               #f
               #t))
  (wrap regexp-match-peek-positions* -regexp-match-peek-positions*)

  ;; Splits a string into a list by removing any piece which matches
  ;; the pattern.
  (define -regexp-split
    (regexp-fn 'regexp-split
               ;; success-k
               (lambda (expr string start end match-start match-end)
                 (let ([string (if (and (string? string)
                                        (or (bytes? expr) (byte-regexp? expr)))
                                 (string->bytes/utf-8 string (char->integer #\?))
                                 string)])
                   (cons (subbstring string start match-start)
                         (regexp-split expr string match-end end))))
               ;; port-success-k:
               (lambda (expr string match-string new-end leftovers)
                 (cons leftovers (regexp-split expr string 0 new-end)))
               ;; failure-k:
               (lambda (expr string start end)
                 (list (subbstring string start (or end (bstring-length string)))))
               ;; port-fail-k
               (lambda (leftover) (list leftover))
               #t
               #f))
  (wrap regexp-split -regexp-split)

  ;; Returns all the matches for the pattern in the string.
  (define -regexp-match*
    (regexp-fn 'regexp-match*
               ;; success-k:
               (lambda (expr string start end match-start match-end)
                 (let ([string (if (and (string? string)
                                        (or (bytes? expr) (byte-regexp? expr)))
                                   (string->bytes/utf-8 string (char->integer #\?))
                                   string)])
                   (cons (subbstring string match-start match-end)
                         (regexp-match* expr string match-end end))))
               ;; port-success-k:
               (lambda (expr string match-string new-end leftovers)
                 (cons match-string (regexp-match* expr string 0 new-end)))
               ;; fail-k:
               (lambda (expr string start end) null)
               ;; port-fail-k:
               (lambda (leftover) null)
               #f
               #f))
  (wrap regexp-match* -regexp-match*)

  (define (regexp-match-exact? p s)
    (let ([m (regexp-match-positions p s)])
      (and m
           (zero? (caar m))
           (if (or (byte-regexp? p) (bytes? p) (bytes? s))
             (= (cdar m) (if (bytes? s) (bytes-length s) (string-utf-8-length s)))
             (= (cdar m) (string-length s))))))

  (define glob->regexp
    (let-values
        ([(def-case-sens) (not (memq (system-type) '(windows macos macosx)))]
         [(item:s item:b simple-item:s simple-item:b)
          (let ([rx (lambda (s)
                      (string-append
                       "(?:"
                       "[\\]." ; escaped item
                       "|"
                       "[*?]"  ; wildcards -- the only 1-character match
                       s       ; [*] more stuff here
                       ")"
                       ))]
                [range "|\\[(?:\\^?\\]|\\^?[^]^])[^]]*\\]"]) ; goes in [*]
            (values (regexp (rx range))
                    (byte-regexp (string->bytes/utf-8 (rx range)))
                    (regexp (rx ""))
                    (byte-regexp (string->bytes/utf-8 (rx "")))))])
      (lambda/kw (glob #:optional
                       [hide-dots? #t] [case-sens? def-case-sens] [simple? #f])
        (let*-values ([(b?) (cond [(bytes? glob) #t]
                                  [(string? glob) #f]
                                  [else (raise-type-error
                                         'glob->regexp
                                         "string or byte string" glob)])]
                      [(app sub ref rx item star any one)
                       (if b?
                         (values bytes-append subbytes bytes-ref byte-regexp
                                 (if simple? simple-item:b item:b)
                                 (char->integer #\*) #".*" #".")
                         (values string-append substring string-ref regexp
                                 (if simple? simple-item:s item:s)
                                 #\* ".*" "."))]
                      [(pfx sfx) (if case-sens?
                                   (if b? (values #"^" #"$")
                                          (values  "^"  "$"))
                                   (if b? (values #"^(?i:" #")$")
                                          (values  "^(?i:"  ")$")))]
                      [(pfx) (if hide-dots?
                               (app pfx (if b? #"(?![.])" "(?![.])"))
                               pfx)]
                      [(subq) (lambda xs (regexp-quote (apply sub xs)))])
          (let loop ([i 0] [ps (regexp-match-positions* item glob)] [r '()])
            (if (null? ps)
              (let ([r (apply app (reverse! (cons (subq glob i) r)))])
                (rx (app pfx r sfx)))
              (loop (cdar ps) (cdr ps)
                    ;; length=1 is only for `*' or `?'
                    (cons (if (= 1 (- (cdar ps) (caar ps)))
                            (if (equal? star (ref glob (caar ps))) any one)
                            (sub glob (caar ps) (cdar ps)))
                          (if (= i (caar ps))
                            r (cons (subq glob i (caar ps)) r))))))))))

  )
