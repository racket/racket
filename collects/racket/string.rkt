#lang racket/base

(provide string-append* string-join string-trim string-normalize-spaces)

(define string-append*
  (case-lambda [(strs) (apply string-append strs)] ; optimize common case
               [(s1 strs) (apply string-append s1 strs)]
               [(s1 s2 strs) (apply string-append s1 s2 strs)]
               [(s1 s2 s3 strs) (apply string-append s1 s2 s3 strs)]
               [(s1 s2 s3 s4 strs) (apply string-append s1 s2 s3 s4 strs)]
               [(str . strss) (apply apply string-append str strss)]))

(require (only-in racket/list add-between))

(define (string-join strs sep)
  (cond [(not (and (list? strs) (andmap string? strs)))
         (raise-type-error 'string-join "list-of-strings" strs)]
        [(not (string? sep))
         (raise-type-error 'string-join "string" sep)]
        [(null? strs) ""]
        [(null? (cdr strs)) (car strs)]
        [else (apply string-append (add-between strs sep))]))

;; See http://en.wikipedia.org/wiki/Trimming_(computer_programming) for a nice
;; overview of popular names etc for these functions;
;; http://blog.stevenlevithan.com/archives/faster-trim-javascript for some ways
;; to implement trimming.
(define (string-trim str [rx #px"\\s+"]
                     #:left? [left? #t] #:right? [right? #t])
  (unless (string? str) (raise-type-error 'string-trim "string" str))
  (unless (regexp? rx)  (raise-type-error 'string-trim "regexp" rx))
  (define len (string-length str))
  (if (zero? len)
    str
    (let* ([start (if (and left? (regexp-match? rx (substring str 0 1)))
                    (cdar (regexp-match-positions rx str))
                    0)]
           [end (and right? (< start len)
                     (regexp-match? rx (substring str (- len 1)))
                     (for/or ([i (in-range (- len 2) (- start 1) -1)])
                       (and (not (regexp-match? rx (substring str i (add1 i))))
                            (add1 i))))])
      (if (and (not start) (not end))
        str
        (substring str (or start 0) (or end len))))))

(define (string-normalize-spaces str [rx #px"\\s+"]
                                 #:space [space " "] #:trim? [trim? #t])
  (define ps (regexp-match-positions* rx str))
  (if (null? ps)
    str
    (let ([drop-first? (and trim? (zero? (caar ps)))]
          [len (string-length str)])
      (let loop ([ps (if drop-first? (cdr ps) ps)]
                 [i  (if drop-first? (cdar ps) 0)]
                 [r  '()])
        (if (or (null? ps) (and trim? (= len (cdar ps))))
          (apply string-append
                 (reverse (cons (substring str i (if (null? ps) len (caar ps)))
                                r)))
          (loop (cdr ps)
                (cdar ps)
                (list* space (substring str i (caar ps)) r)))))))
