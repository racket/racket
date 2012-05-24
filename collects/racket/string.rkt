#lang racket/base

(provide string-append*
         string-join
         string-trim
         string-normalize-spaces
         string-split
         string-replace)

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

;; Utility for the functions below: get a string or a regexp and return a list
;; of the regexp (strings are converted using `regexp-quote'), the and versions
;; that matches at the beginning/end.
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

(define (internal-split who str sep trim? +?)
  (define rxs (get-rxs who sep +?))
  (define-values [l r]
    (if trim? (internal-trim who str sep #t #t (cdr rxs)) (values #f #f)))
  (define strs (regexp-split (car rxs) str (or l 0) r))
  ;; Seems to make more sense for these functions (eg, this corresponds to
  ;; simple uses where `string-split' in Emacs uses t for `omit-nulls' (but we
  ;; don't do that for all nulls).)
  (if (equal? strs '("")) '() strs))

(define (string-split str [sep none] #:trim? [trim? #t] #:repeat? [+? #f])
  (internal-split 'string-split str sep trim? +?))

(define (string-normalize-spaces str [sep none] [space " "]
                                 #:trim? [trim? #t] #:repeat? [+? #f])
  (string-join (internal-split 'string-normalize-spaces str sep trim? +?)
               space))

(define replace-cache (make-weak-hasheq))
(define (string-replace str from to #:all? [all? #t])
  (unless (string? str) (raise-type-error 'string-replace "string" str))
  (unless (string? to)  (raise-type-error 'string-replace "string" to))
  (define from*
    (if (regexp? from)
      from
      (hash-ref! replace-cache from
        (λ() (if (string? from)
               (regexp (regexp-quote from))
               (raise-type-error 'string-replace "string" from))))))
  (define to* (regexp-replace-quote to))
  (if all?
    (regexp-replace* from* str to*)
    (regexp-replace  from* str to*)))
