#lang racket/base
(require "regexp.rkt"
         "lazy-bytes.rkt"
         "port.rkt"
         "compile.rkt"
         "extract.rkt"
         "search.rkt")

;; Drives a regexp matcher on a byte string, character string, or port

(provide drive-regexp-match
         
         fast-drive-regexp-match?/bytes
         fast-drive-regexp-match?/string
         fast-drive-regexp-match-positions/bytes
         fast-drive-regexp-match-positions/string
         fast-drive-regexp-match/bytes
         fast-drive-regexp-match/string

         FAST-STRING-LEN)

;; ----------------------------------------
;; Start with some (repetative) functions for the most common cases to
;; keep the overhead low for reaching these cases.

(define FAST-STRING-LEN 64)

(define (fast-drive-regexp-match?/bytes rx in start-pos end-pos)
  (define state (and (rx:regexp-references? rx)
                     (make-vector (rx:regexp-num-groups rx) #f)))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) state))
  (and ms-pos #t))

(define (fast-drive-regexp-match?/string rx in-str start-offset end-offset)
  (define state (and (rx:regexp-references? rx)
                     (make-vector (rx:regexp-num-groups rx) #f)))
  (define in (string->bytes/utf-8 in-str 0 start-offset (or end-offset (string-length in-str))))
  (define-values (ms-pos me-pos)
    (search-match rx in 0 0 (bytes-length in) state))
  (and ms-pos #t))

(define (fast-drive-regexp-match-positions/bytes rx in start-pos end-pos)
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) state))
  (and ms-pos
       (if state
           (cons (cons ms-pos me-pos) (vector->list state))
           (list (cons ms-pos me-pos)))))

(define (fast-drive-regexp-match-positions/string rx in-str start-offset end-offset)
  (define in (string->bytes/utf-8 in-str 0 start-offset (or end-offset (string-length in-str))))
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in 0 0 (bytes-length in) state))
  (define (string-offset pos)
    (+ start-offset (bytes-utf-8-length in #\? 0 pos)))
  (and ms-pos
       (cons (cons (string-offset ms-pos) (string-offset me-pos))
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (cons (string-offset (car p))
                              (string-offset (cdr p)))))
                 null))))

(define (fast-drive-regexp-match/bytes rx in start-pos end-pos)
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) state))
  (and ms-pos
       (cons (subbytes in ms-pos me-pos)
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (subbytes in (car p) (cdr p))))
                 null))))

(define (fast-drive-regexp-match/string rx in-str start-offset end-offset)
  (define in (string->bytes/utf-8 in-str 0 start-offset (or end-offset (string-length in-str))))
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in 0 0 (bytes-length in) state))
  (and ms-pos
       (cons (bytes->string/utf-8 in #\? ms-pos me-pos)
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (bytes->string/utf-8 in #\? (car p) (cdr p))))
                 null))))

;; ----------------------------------------
;; The general case

;; An "offset" refers to a position in a byte string (in bytes) string
;; (in characters), or port (in bytes). A "pos" always refers to a
;; position in bytes --- so, a "pos" is normalized to UTF-8 bytes in
;; the case of a string.

(define (drive-regexp-match who orig-rx orig-in orig-start-offset orig-end-offset out prefix
                            #:search-offset [search-offset orig-start-offset]
                            #:mode mode
                            #:in-port-ok? [in-port-ok? #t]
                            #:in-path-ok? [in-path-ok? #t]
                            #:peek? [peek? #f] #:immediate-only? [immediate-only? #f]
                            #:progress-evt [progress-evt #f]
                            #:end-bytes? [end-bytes? #f]
                            #:end-bytes-count [end-bytes-count #f])
  
  (define rx (cond
              [(rx:regexp? orig-rx) orig-rx]
              [(string? orig-rx) (make-regexp who orig-rx #f #f #f)]
              [(bytes? orig-rx) (make-regexp who orig-rx #f #t #f)]
              [else (raise-argument-error who "(or/c regexp? byte-regexp? string? bytes?)" orig-rx)]))
  (define in (if (and in-path-ok? (path? orig-in))
                 (if (rx:regexp-bytes? rx)
                     (path->bytes orig-in)
                     (path->string orig-in))
                 orig-in))
  (unless (or (and (bytes? in) (not peek?))
              (and (string? in) (not peek?))
              (and in-port-ok? (input-port? in)))
    (raise-argument-error who
                          (cond
                           [peek? "input-port?"]
                           [in-port-ok? "(or/c bytes? string? input-port? path?)"]
                           [in-path-ok? "(or/c bytes? string? path?)"]
                           [else "(or/c bytes? string?)"])
                          orig-in))
  
  (define start-offset (cond
                        [orig-start-offset
                         (unless (exact-nonnegative-integer? orig-start-offset)
                           (raise-argument-error who "exact-nonnegative-integer?" orig-start-offset))
                         (check-range who "starting index" in orig-start-offset 0)
                         orig-start-offset]
                        [else 0]))
  (define end-offset (cond
                      [orig-end-offset
                       (unless (exact-nonnegative-integer? orig-end-offset)
                         (raise-argument-error who "(or/c #f exact-nonnegative-integer?)" orig-end-offset))
                       (check-range who "ending index" in orig-end-offset start-offset)
                       orig-end-offset]
                      [(bytes? in) (bytes-length in)]
                      [(string? in) (string-length in)]
                      [else 'eof]))
  
  (unless (or (not out) (output-port? out))
    (raise-argument-error who "(or/c #f output-port?)" out))
  
  (unless (bytes? prefix)
    (raise-argument-error who "bytes?" prefix))
  
  (when end-bytes?
    (unless (exact-nonnegative-integer? end-bytes-count)
      (raise-argument-error who "exact-nonnegative-integer?" end-bytes-count)))
  
  (define state (and (or (not (eq? mode '?))
                         (rx:regexp-references? rx))
                     (let ([n (rx:regexp-num-groups rx)])
                       (and (positive? n)
                            (make-vector n #f)))))
  
  ;; Separate cases for bytes, strings, and port.
  ;; There's an annoying level of duplication here, but
  ;; there are lots of little differences in each case.
  (cond
   
   ;; Bytes input, no provided prefix: ----------------------------------------
   [(and (bytes? in)
         (not out)
         (equal? #"" prefix))
    (define start-pos start-offset)
    (define search-pos search-offset)
    (define end-pos end-offset)
    
    ;; Search for a match:
    (define-values (ms-pos me-pos) (search-match rx in search-pos start-pos end-pos state))
    
    ;; Maybe write skipped bytes:
    (when out
      (write-bytes in out 0 (or ms-pos end-pos)))
    
    ;; Return match results:
    (case (and ms-pos mode)
      [(#f) (add-end-bytes #f end-bytes-count #f #f)]
      [(?) #t]
      [(positions)
       (define positions (byte-positions->byte-positions ms-pos me-pos state))
       (add-end-bytes positions end-bytes-count in me-pos)]
      [(strings)
       (define bytess (byte-positions->bytess in ms-pos me-pos state))
       (add-end-bytes bytess end-bytes-count in me-pos)])]

   ;; Sufficiently small string input, no provided prefix: --------------------
   [(and (string? in)
         (not out)
         (equal? #"" prefix)
         ((- end-offset start-offset) . < . FAST-STRING-LEN))
    ;; `bstr-in` includes only the characters fom `start-offset` to
    ;; `end-offset`, so the starting offset (in characters)
    ;; corresponds to a 0 position (in bytes):
    (define bstr-in (string->bytes/utf-8 in 0 start-offset end-offset))
    (define search-pos (if (= start-offset search-offset)
                           0
                           (string-utf-8-length in start-offset search-offset)))
    (define end-pos (bytes-length bstr-in))
    
    ;; Search for a match:
    (define-values (ms-pos me-pos) (search-match rx bstr-in search-pos 0 end-pos state))
    
    ;; Maybe write skipped bytes:
    (when out
      (write-string in out 0 start-offset)
      (write-bytes bstr-in out 0 (or ms-pos end-pos)))
    
    ;; Return match results:
    (case (and ms-pos mode)
      [(#f) (add-end-bytes #f end-bytes-count #f #f)]
      [(?) #t]
      [(positions)
       ;; If pattern is bytes-based, then results will be bytes-based:
       (define positions
         (cond
          [(rx:regexp-bytes? rx)
           (define delta (string-utf-8-length in 0 start-offset))
           (byte-positions->byte-positions ms-pos me-pos state #:delta delta)]
          [else
           (byte-positions->string-positions bstr-in ms-pos me-pos state
                                             #:result-offset start-offset)]))
       (add-end-bytes positions end-bytes-count bstr-in me-pos)]
      [(strings)
       ;; If pattern is bytes-based, then results will be bytes instead of strings:
       (define bytes/strings
         (cond
          [(rx:regexp-bytes? rx)
           (byte-positions->bytess bstr-in ms-pos me-pos state)]
          [else
           (byte-positions->strings bstr-in ms-pos me-pos state)]))
       (add-end-bytes bytes/strings end-bytes-count bstr-in me-pos)])]
   
   ;; Port input, long string input, and/or provided prefix: --------------------
   [else
    (define prefix-len (bytes-length prefix))
    ;; The lazy-bytes record will include the prefix,
    ;; and it won't include bytes/characters before
    ;; `start-offset`:
    (define start-pos prefix-len)
    (define search-pos (if (= start-offset search-offset)
                           start-pos
                           (+ start-pos
                              (cond
                               [(string? in) (string-utf-8-length in start-offset search-offset)]
                               [else (- search-offset start-offset)]))))
    (define port-in
      (cond
       [(bytes? in) (open-input-bytes/no-copy in start-offset end-offset)]
       [(string? in) (open-input-string/lazy in start-offset end-offset)]
       [else in]))
    (define any-bytes-left?
      (cond
        [(and (input-port? in)
              (positive? start-offset))
         (cond
           [peek?
            ;; Make sure we can skip over `start-offset` bytes:
            (not (eof-object? (peek-byte port-in (sub1 start-offset))))]
           [else
            ;; discard skipped bytes:
            (copy-port-bytes port-in #f start-offset)])]
        [else #t]))
    ;; Create a lazy string from the port:
    (define lb-in (make-lazy-bytes port-in (if peek? start-offset 0) prefix
                                   peek? immediate-only? progress-evt
                                   out (rx:regexp-max-lookbehind rx)
                                   (and (input-port? in)
                                        (not (eq? 'eof end-offset))
                                        (- end-offset start-offset))))
    (define end-pos (if (or (eq? 'eof end-offset)
                            (string? in))
                        'eof
                        (+ start-pos
                           (- end-offset start-offset))))

    ;; Search for a match:
    (define-values (ms-pos me-pos)
      (if any-bytes-left?
          (search-match rx lb-in search-pos 0 end-pos state)
          ;; Couldn't skip past `start-offset` bytes for an input port:
          (values #f #f)))
    
    ;; To write and consume skipped bytes, but we'll do this only
    ;; after we've extracted match information from the lazy byte
    ;; string:
    (define (write/consume-skipped)
      (when (not peek?)
        (cond
         [ms-pos
          (when out
            ;; Flush bytes before match:
            (lazy-bytes-advance! lb-in ms-pos #t))
          (when (input-port? in)
            ;; Consume bytes that correspond to match:
            (copy-port-bytes port-in #f (- me-pos prefix-len)))]
         [(eq? end-pos 'eof)
          ;; Copy all remaining bytes from input to output
          (when (or out (input-port? in))
            (copy-port-bytes port-in out #f))]
         [else
          (when out
            ;; Copy all bytes to output
            (lazy-bytes-advance! lb-in end-pos #t))
          (when (input-port? in)
            ;; Consume all bytes
            (copy-port-bytes port-in #f (- end-pos start-pos)))])))

    (begin0

     ;; Return match results:
     (case (and ms-pos
                (not (lazy-bytes-failed? lb-in))
                mode)
       [(#f)
        (add-end-bytes #f end-bytes-count #f #f)]
       [(?) #t]
       [(positions)
        ;; Result positions correspond to the port after `start-offset`, 
        ;; but with the prefix bytes (= `start-pos`)
        (define bstr (lazy-bytes-bstr lb-in))
        (define positions
          (cond
           [(or (not (string? in))
                (rx:regexp-bytes? rx))
            (define delta (- start-offset start-pos))
            (byte-positions->byte-positions ms-pos me-pos state #:delta delta)]
           [else
            ;; Some bytes may have been discarded in `lb-in`, and we
            ;; don't know how many characters those add up to. The
            ;; starting position `ms-pos` must be on a code-point
            ;; boundary, and everything from `ms-pos` to `ms-end` must
            ;; still be in `lb-in`. So, find `ms-pos` in the original
            ;; string, and take it from there.
            (define ms-str-pos (byte-index->string-index in start-offset (- ms-pos start-pos)))
            (define delta (lazy-bytes-discarded-count lb-in))
            (byte-positions->string-positions bstr ms-pos me-pos state
                                              #:start-index (- ms-pos delta)
                                              #:delta delta
                                              #:result-offset (+ ms-str-pos start-offset))]))
        (add-end-bytes positions end-bytes-count bstr me-pos)]
       [(strings)
        ;; The byte string may be shifted by discarded bytes, if not
        ;; in `peek?` mode
        (define bstr (lazy-bytes-bstr lb-in))
        (define delta (lazy-bytes-discarded-count lb-in))
        (define bytes/strings
          (cond
           [(or (not (string? in))
                (rx:regexp-bytes? rx))
            (byte-positions->bytess bstr ms-pos me-pos state #:delta delta)]
           [else
            (byte-positions->strings bstr ms-pos me-pos state #:delta delta)]))
        (add-end-bytes bytes/strings end-bytes-count bstr me-pos)])
     
     ;; Now, write and consume port content:
     (write/consume-skipped))]))

;; -------------------------------------------------------
;; Range-checking arguments to `regexp-match` and company:

(define (check-range who what in pos start-pos)
  (define len (cond
               [(bytes? in) (bytes-length in)]
               [(string? in) (string-length in)]
               [else +inf.0]))
  (unless (pos . >= . start-pos)
    (raise-arguments-error who
                           (format "~a is smaller than starting index" what)
                           what pos
                           "starting index" start-pos))
  (unless (pos . <= . len)
    (raise-arguments-error who
                           (format "~a is out of range" what)
                           what pos)))

