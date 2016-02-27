(module string "pre-base.rkt"

  (provide real->decimal-string
           regexp-quote
           regexp-replace-quote
           regexp-match*
           regexp-match-positions*
           regexp-match-peek-positions*
           regexp-split
           regexp-match-exact?
           regexp-try-match
           -regexp-replace*
           regexp-replaces)
  (require (for-syntax "stxcase-scheme.rkt"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (real->decimal-string n [digits 2])
    (unless (exact-nonnegative-integer? digits)
      (raise-argument-error 'real->decimal-string "exact-nonnegative-integer?"
                            digits))
    (let* ([e (expt 10 digits)]
           [num (round (abs (* e (inexact->exact n))))])
      (format
       "~a~a.~a"
       (if (or (negative? n) (equal? n -0.0)) "-" "")
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
                     [else (raise-argument-error 'regexp-quote
                                                 "(or/c string? bytes?)" s)])]
           [s (if b?
                (regexp-replace* regexp-quote-chars:b s #"\\\\&")
                (regexp-replace* regexp-quote-chars:s s "\\\\&"))])
      (cond [case-sens? s]
            [b?   (bytes-append #"(?i:" s #")")]
            [else (string-append "(?i:" s ")")])))

  (define (regexp-replace-quote s)
    (cond [(bytes?  s) (regexp-replace* #rx#"[&\\]" s #"\\\\&")]
          [(string? s) (regexp-replace* #rx"[&\\]"  s "\\\\&")]
          [else (raise-argument-error 'regexp-replace-quote
                                      "(or/c string? bytes?)" s)]))

  ;; This was originally intended to be general, but it has become specialized
  ;; to deal with the combination of a regexp and a number:
  (define (make-regexp-tweaker tweaker)
    (let ([t (make-hash)])
      (lambda (rx n)
        (define-syntax-rule (->str x) (if (bytes? x) (bytes->string/utf-8 x) x))
        (define-syntax-rule (->bts x) (if (bytes? x) x (string->bytes/utf-8 x)))
        (define-syntax-rule (tweak unwrap wrap convert)
          (let ([tweaked (tweaker (unwrap rx) n)])
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
                [else (raise-argument-error
                       'regexp-tweaker
                       "(or/c regexp? byte-regexp? string? bytes?)"
                       rx)]))
        (let ([key (cons n rx)])
          (or (hash-ref t key #f)
              (let ([rx* (run-tweak)]) (hash-set! t key rx*) rx*))))))

  (define (regexp-try-match pattern input-port [start-k 0] [end-k #f] [out #f]
                            [prefix #""])
    (unless (input-port? input-port)
      (raise-argument-error 'regexp-try-match "input-port?" input-port))
    (unless (or (not out) (output-port? out))
      (raise-argument-error 'regexp-try-match "(or/c output-port? #f)" out))
    (let ([m (regexp-match-peek-positions pattern input-port start-k end-k #f
                                          prefix)])
      (and m
           ;; What happens if someone swipes our bytes before we can get them?
           (let ([drop (caar m)])
             ;; drop prefix before match:
             (let ([s (read-bytes drop input-port)])
               (when out (write-bytes s out)))
             ;; Get the matching part, and shift matching indices
             (let ([s (read-bytes (- (cdar m) drop) input-port)])
               (cons s
                     (map (lambda (p)
                            (and p (subbytes s (- (car p) drop)
                                               (- (cdr p) drop))))
                          (cdr m))))))))

  ;; Helper macro for the regexp functions below, with some utilities.
  (define (bstring-length s)
    (if (bytes? s) (bytes-length s) (string-length s)))
  (define no-empty-edge-matches
    (make-regexp-tweaker
     (lambda (rx n)
       (if (bytes? rx)
         (bytes-append #"(?:" rx #")(?<="
                       (make-bytes n (char->integer #\.)) #")")
         (format "(?:~a)(?<=~a)" rx (make-bytes n (char->integer #\.)))))))
  (define-syntax-rule (regexp-loop
                       name loop start end pattern string ipre
                       success-choose failure-k
                       port-success-k port-success-choose port-failure-k
                       need-leftover? peek?)
    (let* ([string (if (path? string)
                     (if (or (string? pattern) (regexp? pattern))
                       (path->string string)
                       (path->bytes string))
                     string)]
           [len (cond [(string? string) (string-length string)]
                      [(bytes?  string) (bytes-length  string)]
                      [else #f])]
           [orig-rx
            (cond [(bytes? pattern) (byte-regexp pattern)]
                  [(string? pattern) (regexp pattern)]
                  [(regexp? pattern) pattern]
                  [(byte-regexp? pattern) pattern]
                  [else (raise-argument-error
                         'name
                         "(or/c regexp? byte-regexp? string? bytes?)"
                         pattern)])]
           [max-lookbehind (regexp-max-lookbehind orig-rx)])
      (if peek?
        (unless (input-port? string)
          (raise-argument-error 'name "input-port?" string))
        (unless (or len (input-port? string))
          (raise-argument-error
           'name "(or/c string? bytes? path? input-port?)" string)))
      (unless (and (number? start) (exact? start) (integer? start)
                   (start . >= . 0))
        (raise-argument-error 'name "exact-nonnegative-integer?" start))
      (unless (or (not end)
                  (and (number? end) (exact? end) (integer? end)
                       (end . >= . 0)))
        (raise-argument-error 'name "(or/c exact-nonnegative-integer? #f)" end))
      (unless (bytes? ipre)
        (raise-argument-error 'name "bytes?" ipre))
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
       (let loop ([acc '()] [start start] [end end] [ipre ipre] [0-ok? #t])
         (let ([rx (if 0-ok?
                     orig-rx
                     (no-empty-edge-matches orig-rx
                                            (add1 (bytes-length ipre))))])
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
               (let-values ([(ms ipre)
                             (regexp-match/end rx string 0 end spitout ipre
                                               max-lookbehind)])
                 (let* ([m (and ms (car ms))]
                        [discarded/leftovers (if need-leftover?
                                               (get-output-bytes spitout)
                                               discarded/leftovers)]
                        [skipped (if need-leftover?
                                   (bstring-length discarded/leftovers)
                                   discarded/leftovers)]
                        [got (and m (bstring-length m))]
                        [end (and end m (- end skipped got))])
                   (if m
                     (let ([0-ok? (not (zero? got))])
                       (loop (port-success-choose discarded/leftovers ms acc)
                             0 end ipre 0-ok?))
                     (port-failure-k acc discarded/leftovers)))))

             ;; String/port match, get positions
             (let-values ([(m ipre)
                           (if peek?
                             (regexp-match-peek-positions/end
                              rx string start end #f ipre
                              max-lookbehind)
                             (regexp-match-positions/end
                              rx string start end #f ipre
                              max-lookbehind))])
               (if (not m)
                 (failure-k acc start end)
                 (let* ([mstart (caar m)]
                        [mend (cdar m)]
                        [0-ok? (not (= mstart mend))])
                   (if (and port-success-k (input-port? string))
                     (port-success-k
                      (lambda (acc new-start new-end)
                        (loop acc new-start new-end ipre 0-ok?))
                      acc start end m)
                     (loop (success-choose start m acc)
                           mend end ipre 0-ok?)))))))))))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-positions* pattern string [start 0] [end #f] [ipre #""]
                                   #:match-select [match-select car])
    (unless (procedure? match-select)
      (raise-argument-error 'regexp-match-positions* "procedure?" match-select))
    ;; Note: no need for a #:gap-select?, since it is easily inferred from the
    ;; resulting matches
    (if (eq? match-select car)
      ;; common case
      (regexp-loop regexp-match-positions* loop start end pattern string ipre
        ;; success-choose:
        (lambda (start ms acc) (cons (car ms) acc))
        ;; failure-k:
        (lambda (acc start end) acc)
        ;; port-success-k: need to shift index of rest as reading; cannot
        ;; do a tail call without adding another state variable to the
        ;; regexp loop, so this remains inefficient
        (lambda (loop acc start end ms)
          (let ([mstart (caar ms)] [mend (cdar ms)])
            (append (map (lambda (p) (cons (+ mend (car p)) (+ mend (cdr p))))
                         (loop '() 0 (and end (- end mend))))
                    (cons (car ms) acc))))
        ;; other port functions: use string case
        #f #f
        ;; flags
        #f #f)
      ;; using some selector
      (regexp-loop regexp-match-positions* loop start end pattern string ipre
        ;; success-choose:
        (lambda (start ms acc) (cons (match-select ms) acc))
        ;; failure-k:
        (lambda (acc start end) acc)
        ;; port-success-k: need to shift index of rest as reading; cannot
        ;; do a tail call without adding another state variable to the
        ;; regexp loop, so this remains inefficient
        (lambda (loop acc start end ms)
          (let* ([mend (cdar ms)]
                 [rest (loop '() 0 (and end (- end mend)))]
                 [s    (match-select ms)])
            ;; assumes that it's a valid selector (and always works the same,
            ;; ie, does not return a single pair sometimes a list of pairs at
            ;; other times)
            (append
             (map (if (or (and (pair? s) (exact-integer? (car s))) (not s))
                    ;; assume that it returns a single match
                    (lambda (p) (cons (+ mend (car p)) (+ mend (cdr p))))
                    ;; assume that it returns a list
                    (lambda (ps)
                      (map (lambda (p)
                             (and p (cons (+ mend (car p)) (+ mend (cdr p)))))
                           ps)))
                  rest)
             (cons s acc))))
        ;; other port functions: use string case
        #f #f
        ;; flags
        #f #f)))

  ;; Returns all the positions at which the pattern matched.
  (define (regexp-match-peek-positions* pattern string [start 0] [end #f]
                                        [ipre #""]
                                        #:match-select [match-select car])
    (unless (procedure? match-select)
      (raise-argument-error 'regexp-match-peek-positions* "procedure?" match-select))
    (if (eq? match-select car)
      ;; common case
      (regexp-loop regexp-match-peek-positions* loop start end pattern string ipre
        ;; success-choose:
        (lambda (start ms acc) (cons (car ms) acc))
        ;; failure-k:
        (lambda (acc start end) acc)
        ;; port functions: use string case
        #f #f #f
        ;; flags
        #f #t)
      ;; using some selector
      (regexp-loop regexp-match-peek-positions* loop start end pattern string ipre
        ;; success-choose:
        (lambda (start ms acc) (cons (match-select ms) acc))
        ;; failure-k:
        (lambda (acc start end) acc)
        ;; port functions: use string case
        #f #f #f
        ;; flags
        #f #t)))

  ;; Small helper for the functions below
  (define (get-buf+sub string pattern)
    (let ([buf (if (and (string? string)
                        (or (byte-regexp? pattern) (bytes? pattern)))
                 (string->bytes/utf-8 string (char->integer #\?))
                 string)])
      (values
       buf
       (if (or (bytes? buf)
               (and (path? string)
                    (or (bytes? pattern) (byte-regexp? pattern))))
         subbytes substring))))

  ;; Splits a string into a list by removing any piece which matches
  ;; the pattern.
  (define (regexp-split pattern string [start 0] [end #f] [ipre #""])
    (define-values [buf sub] (get-buf+sub string pattern))
    (regexp-loop regexp-split loop start end pattern buf ipre
      ;; success-choose:
      (lambda (start ms acc) (cons (sub buf start (caar ms)) acc))
      ;; failure-k:
      (lambda (acc start end)
        (cons (if end (sub buf start end) (sub buf start)) acc))
      ;; port-success-k:
      #f
      ;; port-success-choose:
      (lambda (leftovers ms acc) (cons leftovers acc))
      ;; port-failure-k:
      (lambda (acc leftover) (if leftover (cons leftover acc) acc))
      ;; flags
      #t #f))

  ;; Like splitting, but insert a replacement between matches
  (define -regexp-replace*
    (let ([regexp-replace*
           (lambda (pattern string orig-replacement [start 0] [end #f] [ipre #""])
             (define-values [buf sub] (get-buf+sub string pattern))
             (define needs-string?
               (and (or (string? pattern) (regexp? pattern)) (string? string)))
             (define replacement
               (if (and (not needs-string?) (string? orig-replacement))
                 (string->bytes/utf-8 orig-replacement)
                 orig-replacement))
             (define (check proc args)
               (let ([v (apply proc args)])
                 (unless (if needs-string? (string? v) (bytes? v))
                   (raise-mismatch-error
                    '|regexp-replace* (calling given filter procedure)|
                    (if needs-string?
                      "expected a string result: "
                      "expected a byte string result: ")
                    v))
                 v))
             (define need-replac?
               (and (not (procedure? replacement))
                    (regexp-match? #rx#"[\\&]" replacement)))
             (define (replac ms str)
               (if need-replac?
                 ((if (string? str) bytes->string/utf-8 values)
                  (apply
                   bytes-append
                   (let ([str (if (string? str) (string->bytes/utf-8 str) str)]
                         [get-match
                          (lambda (n)
                            (if (n . < . (length ms))
                              (let* ([p (list-ref ms n)]
                                     [s (if (pair? p)
                                          (sub buf (car p) (cdr p))
                                          p)])
                                (if (string? s) (string->bytes/utf-8 s) s))
                              #""))])
                     (let loop ([pos 0])
                       (let ([m (regexp-match-positions #rx#"[\\&]" str pos)])
                         (if m
                           (cons (subbytes str pos (caar m))
                                 (cond
                                   [(equal? (char->integer #\&)
                                            (bytes-ref str (caar m)))
                                    (cons (get-match 0) (loop (cdar m)))]
                                   [(= (cdar m) (bytes-length str))
                                    ;; \ with no following character
                                    (list (get-match 0))]
                                   [(let ([next (bytes-ref str (cdar m))])
                                      (or (and (equal? (char->integer #\&) next)
                                               #"&")
                                          (and (equal? (char->integer #\\) next)
                                               #"\\")
                                          (and (equal? (char->integer #\$) next)
                                               #"")))
                                    => (lambda (s)
                                         (cons s (loop (add1 (cdar m)))))]
                                   [else
                                    (let ([n (regexp-match #rx#"^[0-9]+" str
                                                           (cdar m))])
                                      (if n
                                        (cons (get-match (string->number
                                                          (bytes->string/utf-8
                                                           (car n))))
                                              (loop (+ (cdar m)
                                                       (bytes-length (car n)))))
                                        (cons (get-match 0)
                                              (loop (cdar m)))))]))
                           (list (subbytes str pos))))))))
                 str))
             (when (or (string? pattern) (bytes? pattern)
                       (regexp? pattern) (byte-regexp? pattern))
               (unless (or (string? string)
                           (bytes? string))
                 (raise-argument-error 'regexp-replace* "(or/c string? bytes?)"
                                       string))
               (unless (or (string? replacement)
                           (bytes? replacement)
                           (procedure? replacement))
                 (raise-argument-error 'regexp-replace*
                                       "(or/c string? bytes? procedure?)"
                                       replacement))
               (when (and needs-string? (bytes? replacement))
                 (raise-mismatch-error
                  'regexp-replace*
                  "cannot replace a string with a byte string: "
                  replacement)))
             (define r
               (regexp-loop regexp-replace* loop start end pattern buf ipre
                 ;; success-choose:
                 (lambda (start ms acc)
                   (list* (if (procedure? replacement)
                            (check
                             replacement
                             (for/list ([m ms])
                               (and m (sub buf (car m) (cdr m)))))
                            (replac ms replacement))
                          (sub buf start (caar ms))
                          acc))
                 ;; failure-k:
                 (lambda (acc start end)
                   (cons (if end (sub buf start end) (sub buf start)) acc))
                 ;; port functions: use string case
                 #f #f #f
                 ;; flags
                 #t #f))
             (apply
              (if (bytes? buf) bytes-append string-append)
              (cond [(and (= start 0) (not end)) r]
                    [(not end) (cons (sub string 0 start) r)]
                    [else `(,(sub string 0 start) ,@r ,(sub string end))])))])
      regexp-replace*))

  ;; Returns all the matches for the pattern in the string, optionally
  ;; other submatches and/or gap strings too.
  (define (regexp-match* pattern string [start 0] [end #f] [ipre #""]
                         #:match-select [match-select car]
                         #:gap-select?  [gap-select   #f])
    (cond
      ;; nonsensical case => throw an error
      [(and (not match-select) (not gap-select))
       ;; An alternative would be to throw '(), but that's non-uniform in that
       ;; it wouldn't consume the contents of a port.  Another alternative is
       ;; to do the full code to consume what's needed, but that's spending
       ;; cycles to always get a '() result.
       (raise
        (exn:fail:contract
         "regexp-match*: one of `match-select' or `gap-select' must be non-#f"
         (current-continuation-marks)))]
      ;; no match-select => same as `regexp-split'
      [(not match-select) (regexp-split pattern string start end ipre)]
      [(not (procedure? match-select))
       (raise-argument-error 'regexp-match* "(or/c procedure? #f)" match-select)]
      ;; uncommon case => full code
      [(not (eq? match-select car))
       (define-values [buf sub] (get-buf+sub string pattern))
       (regexp-loop regexp-explode loop start end pattern buf ipre
         ;; success-choose:
         (lambda (start ms acc)
           (cons (let ([s (match-select ms)])
                   ;; assumes a valid selector
                   (cond [(not (pair? s)) s] ; #f or '()
                         [(integer? (car s)) (sub buf (car s) (cdr s))]
                         [else (map (lambda (m)
                                      (and m (sub buf (car m) (cdr m))))
                                    s)]))
                 (if gap-select (cons (sub buf start (caar ms)) acc) acc)))
         ;; failure-k:
         (lambda (acc start end)
           (if gap-select
             (cons (if end (sub buf start end) (sub buf start)) acc)
             acc))
         ;; port-success-k:
         #f
         ;; port-success-choose:
         (lambda (leftovers ms acc)
           ;; assumes a valid selector too: here it's used with a bytes list so
           ;; it's simple
           (cons (match-select ms) (if gap-select (cons leftovers acc) acc)))
         ;; port-failure-k:
         (lambda (acc leftover)
           (if (and gap-select leftover) (cons leftover acc) acc))
         ;; flags
         gap-select #f)]
      ;; default for matches, but also include gaps
      [gap-select
       (define-values [buf sub] (get-buf+sub string pattern))
       (regexp-loop regexp-explode loop start end pattern buf ipre
         ;; success-choose:
         (lambda (start ms acc)
           (cons (sub buf (caar ms) (cdar ms))
                 (cons (sub buf start (caar ms)) acc)))
         ;; failure-k:
         (lambda (acc start end)
           (cons (if end (sub buf start end) (sub buf start)) acc))
         ;; port-success-k:
         #f
         ;; port-success-choose:
         (lambda (leftovers ms acc) (cons (car ms) (cons leftovers acc)))
         ;; port-failure-k:
         (lambda (acc leftover) (if leftover (cons leftover acc) acc))
         ;; flags
         gap-select #f)]
      ;; default case => optimized with specific code (this is the previous
      ;; functionality of `regexp-explode*' too)
      [else
       (define-values [buf sub] (get-buf+sub string pattern))
       (regexp-loop regexp-match* loop start end pattern buf ipre
         ;; success-choose:
         (lambda (start ms acc) (cons (sub buf (caar ms) (cdar ms)) acc))
         ;; failure-k:
         (lambda (acc start end) acc)
         ;; port-success-k:
         #f
         ;; port-success-choose:
         (lambda (leftovers ms acc) (cons (car ms) acc))
         ;; port-failure-k:
         (lambda (acc leftover) acc)
         ;; flags
         #f #f)]))

  (define (regexp-match-exact? p s)
    (let ([m (regexp-match-positions p s)])
      (and m (zero? (caar m))
           (= (cdar m)
              (cond [(bytes? s) (bytes-length s)]
                    [(or (byte-regexp? p) (bytes? p))
                     (if (path? s)
                       (bytes-length (path->bytes s))
                       (string-utf-8-length s))]
                    [else
                     (if (path? s)
                       (string-length (path->string s))
                       (string-length s))])))))

  (define (regexp-replaces string replacements)
    (let loop ([str string] [rs replacements])
      (if (null? rs)
        str
        (loop (regexp-replace* (caar rs) str (cadar rs)) (cdr rs)))))

  )
