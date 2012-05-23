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

(define (string-join strs [sep " "])
  (cond [(not (and (list? strs) (andmap string? strs)))
         (raise-type-error 'string-join "list-of-strings" strs)]
        [(not (string? sep))
         (raise-type-error 'string-join "string" sep)]
        [(null? strs) ""]
        [(null? (cdr strs)) (car strs)]
        [else (apply string-append (add-between strs sep))]))

;; Utilities for the functions below
(define none (gensym))
(define get-rxs
  (let ([t (make-weak-hasheq)] [t+ (make-weak-hasheq)])
    (let ([spaces '(#px"\\s+" #px"^\\s+" #px"\\s+$")])
      (hash-set! t none spaces)
      (hash-set! t+ none spaces))
    (λ (who rx +?)
      (hash-ref! (if +? t+ t) rx
        (λ () (let* ([s (cond [(string? rx) (regexp-quote rx)]
                              [(regexp? rx) (object-name rx)]
                              [else (raise-type-error
                                     who "string-or-regexp" rx)])]
                     [s (if +? (string-append "(?:" s ")+") s)]
                     [^s (string-append "^" s)]
                     [s$ (string-append s "$")])
                (if (pregexp? rx)
                  (list (pregexp s) (pregexp ^s) (pregexp s$))
                  (list (regexp  s) (regexp  ^s) (regexp  s$)))))))))

;; returns start+end positions, #f when no trimming should happen
(define (internal-trim who str sep l? r? rxs)
  (unless (string? str) (raise-type-error who "string" str))
  (define l
    (and l? (let ([p (regexp-match-positions (car rxs) str)])
              (and p (let ([p (cdar p)]) (and (> p 0) p))))))
  (define r
    (and r? (let ([p (regexp-match-positions (cadr rxs) str)])
              (and p (let ([p (caar p)])
                       (and (< p (string-length str))
                            (if (and l (> l p)) l p)))))))
  (values l r))

;; See http://en.wikipedia.org/wiki/Trimming_(computer_programming) for a nice
;; overview of popular names etc for these functions;
;; http://blog.stevenlevithan.com/archives/faster-trim-javascript for some ways
;; to implement trimming.
(define (string-trim str [sep none]
                     #:left? [l? #t] #:right? [r? #t] #:repeat? [+? #f])
  (define rxs (get-rxs 'string-trim sep +?))
  (define-values [l r] (internal-trim 'string-trim str sep l? r? (cdr rxs)))
  (cond [(and l r) (substring str l r)]
        [l         (substring str l)]
        [r         (substring str 0 r)]
        [else      str]))

(define (string-normalize-spaces str [sep none] [space " "]
                                 #:trim? [trim? #t] #:repeat? [+? #f])
  (define rxs (get-rxs 'string-normalize-spaces sep +?))
  (define-values [l r]
    (if trim?
      (internal-trim 'string-normalize-spaces str sep #t #t (cdr rxs))
      (values #f #f)))
  (string-join (regexp-split (car rxs) str (or l 0) r) space))
