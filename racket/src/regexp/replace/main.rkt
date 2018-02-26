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

  (let loop ([search-pos 0])
    (define poss
      (drive-regexp-match who rx in 0 #:search-offset search-pos #f #f prefix
                          #:in-port-ok? #f
                          #:in-path-ok? #f
                          #:mode 'positions))
    
    (define (recur)
      (define pos (cdar poss))
      (cond
       [(= pos search-pos)
        (if (= search-pos (chytes-length in))
            (subchytes in 0 0)
            (chytes-append (subchytes in search-pos (add1 search-pos))
                           (loop (add1 search-pos))))]
       [else (loop (cdar poss))]))
    
    (cond
     [(not poss) (cond
                  [(zero? search-pos) in]
                  [else (subchytes in search-pos)])]
     [else
      (chytes-append (subchytes in search-pos (caar poss))
                     (replacements who in poss ins)
                     (if all?
                         (recur)
                         (subchytes in (cdar poss))))])))

;; ----------------------------------------

(define (replacements who in poss insert)
  (cond
   [(procedure? insert)
    (define a (apply insert
                     (for/list ([pos (in-list poss)])
                       (subchytes in (car pos) (cdr pos)))))
    (unless (chytes? in a)
      (raise-result-error who (if (bytes? in) "bytes?" "string?") a))
    a]
    
   [else
    (define count (length poss))
    
    (define (get-chytes n)
      (cond
       [(n . < . count)
        (define pos (list-ref poss n))
        (subchytes in (car pos) (cdr pos))]
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
