#lang racket/base
(require "../match/regexp.rkt"
         "../match/main.rkt"
         "chyte.rkt")

(provide regexp-replace
         regexp-replace*)

(define (regexp-replace rx orig-in insert [prefix #""])
  (do-regexp-replace 'regexp-replace rx orig-in insert prefix #f))

(define (regexp-replace* rx orig-in insert [prefix #""])
  (do-regexp-replace 'regexp-replace* rx orig-in insert prefix #t))

(define (do-regexp-replace who rx-in orig-in insert prefix all?)
  (define string-mode?
    (and (or (string? rx-in) (regexp? rx-in))
         (string? orig-in)))
  (define in (if (and (not string-mode?)
                      (string? orig-in))
                 (string->bytes/utf-8 orig-in)
                 orig-in))
  
  (when (or string-mode?
            (and (or (bytes? rx-in) (byte-regexp? rx-in))
                 (or (string? orig-in) (bytes? orig-in))))
    (unless (or (string? insert)
                (bytes? insert)
                (procedure? insert))
      (raise-argument-error who "(or/c string? bytes? procedure?)" insert)))
  
  (when string-mode?
    (when (bytes? insert)
      (raise-arguments-error who
                             "cannot replace a string with a byte string"
                             "byte string" insert)))
  
  (define rx (cond
              [(string? rx-in) (make-regexp who rx-in #f #f #f)]
              [(bytes? rx-in) (make-regexp who rx-in #f #t #f)]
              [else rx-in]))
  
  (define ins (if (and (not string-mode?)
                       (string? insert))
                  (string->bytes/utf-8 insert)
                  insert))

  (define need-lookbehind (rx:regexp-max-lookbehind rx))

  (let loop ([search-pos 0] [get-list? #f])
    (define use-prefix (and (search-pos . < . need-lookbehind) prefix))
    (define in-start (if use-prefix 0 (max 0 (- search-pos need-lookbehind))))

    (define poss
      (drive-regexp-match who rx in in-start #:search-offset search-pos #f #f prefix
                          #:in-port-ok? #f
                          #:in-path-ok? #f
                          #:mode 'positions))

    (define (recur)
      (define end (cdar poss))
      (cond
        [(= (caar poss) end)
         (if (= end (chytes-length in))
             null
             (cons (subchytes in end (add1 end))
                   (loop (add1 end) #t)))]
        [else (loop end #t)]))
    
    (cond
      [(not poss)
       (define result (cond
                        [(zero? search-pos) in]
                        [else (subchytes in search-pos)]))
       (if get-list?
           (list result)
           result)]
      [else
       (define pre (subchytes in search-pos (caar poss)))
       (define new (replacements who in poss ins prefix))
       (cond
         [all?
          (define result (list* pre new (recur)))
          (if get-list?
              result
              (apply chytes-append result))]
         [else
          (chytes-append pre new (subchytes in (cdar poss)))])])))

;; ----------------------------------------

(define (replacements who in poss insert prefix)
  (cond
   [(procedure? insert)
    (define a (apply insert
                     (for/list ([pos (in-list poss)])
                       (and pos
                            (subchytes* in (car pos) (cdr pos) prefix)))))
    (unless (chytes? in a)
      (raise-result-error who (if (bytes? in) "bytes?" "string?") a))
    a]
    
   [else
    (define count (length poss))
    
    (define (get-chytes n)
      (cond
       [(n . < . count)
        (define pos (list-ref poss n))

        (if pos
            (subchytes* in (car pos) (cdr pos) prefix)
            (subchytes in 0 0))]
       [else (subchytes in 0 0)]))
  
    (define (cons-chytes since pos l)
      (if (= since pos)
          l
          (cons (subchytes insert since pos) l)))
    
    (define len (chytes-length insert))
    (apply (if (bytes? insert)
               bytes-append
               string-append)
           (let loop ([pos 0] [since 0])
             (cond
              [(= pos len)
               (cons-chytes since pos null)]
              [(= (char->integer #\&) (chytes-ref insert pos))
               (cons-chytes since pos
                            (cons (get-chytes 0)
                                  (loop (add1 pos) (add1 pos))))]
              [(= (char->integer #\\) (chytes-ref insert pos))
               (cons-chytes
                since pos
                (let ([c (and ((add1 pos) . < . len)
                              (chytes-ref insert (add1 pos)))])
                  (cond
                   [(or (eq? c (char->integer #\&))
                        (eq? c (char->integer #\\)))
                    (loop (+ pos 2) (add1 pos))]
                   [(eq? c (char->integer #\$))
                    (loop (+ pos 2) (+ pos 2))]
                   [else
                    (let d-loop ([pos (add1 pos)] [accum 0])
                      (cond
                       [(= pos len)
                        (list (get-chytes accum))]
                       [else
                        (define c (chytes-ref insert pos))
                        (if (and (>= c (char->integer #\0))
                                 (<= c (char->integer #\9)))
                            (d-loop (add1 pos) (+ (* accum 10)
                                                  (- c (char->integer #\0))))
                            (cons (get-chytes accum)
                                  (loop pos pos)))]))])))]
              [else
               (loop (add1 pos) since)])))]))

(define (subchytes* in start end prefix)
  (cond
    [(start . < . 0)
     (define len (bytes-length prefix))
     (cond
       [(string? in)
        ;; need to find characters working backward from the end of
        ;; the prefix; we know that the end encodes valid characters,
        ;; since they matched, we can exploit the property that
        ;; characters start with bytes that have the high bit cleared
        ;; or top two bits set
        (let loop ([index len] [start start] [end-index len] [end end])
          (cond
            [(zero? start)
             (define pre-part (bytes->string/utf-8 (subbytes prefix index end-index)))
             (if (end . > . 0)
                 (string-append pre-part (substring in 0 end))
                 pre-part)]
            [else
             (let bloop ([index (sub1 index)])
               (define b (bytes-ref prefix index))
               (cond
                 [(or (not (bitwise-bit-set? b 7))
                      (bitwise-bit-set? b 6))
                  ;; found character start
                  (if (end . >= . 0)
                      (loop index (add1 start) end-index end)
                      (loop index (add1 start) index (add1 end)))]
                 [else (bloop (sub1 index))]))]))]
       [else
        (define pre-part
          (subbytes prefix
                    (+ (bytes-length prefix) start)
                    (+ (bytes-length prefix) (min 0 end))))
        (if (end . > . 0)
            (bytes-append pre-part (subbytes in 0 end))
            pre-part)])]
    [else (subchytes in start end)]))
