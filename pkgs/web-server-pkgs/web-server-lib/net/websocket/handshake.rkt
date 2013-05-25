#lang racket/base

(require racket/list
         racket/contract
         file/md5)

(define RANGE 100000)

(provide/contract
 [compute-ans (number? number? bytes? . -> . bytes?)]
 [remove-alphas (string? . -> . string?)]
 [count-spaces (string? . -> . exact-nonnegative-integer?)]
 [key->number (string? . -> . exact-nonnegative-integer?)]
 [handshake-solution (string? string? bytes? . -> . bytes?)]
 [generate-key (-> (values string? string? bytes? bytes?))]
 [random-char-between (char? char? . -> . char?)]
 [random-alpha-char (-> char-alphabetic?)]
 [add-spaces (exact-nonnegative-integer? string? . -> . string?)]
 [add-alphas (string? . -> . string?)]
 [number->key (exact-nonnegative-integer? . -> . string?)])
 
(define (compute-ans n1 n2 key3)
  (md5 (bytes-append (integer->integer-bytes n1 4 #f #t)
                     (integer->integer-bytes n2 4 #f #t)
                     key3)
       #f))

(define (remove-alphas key)
  (apply string-append
         (regexp-match* "[0-9]" key)))

(define (count-spaces key)
  (length (regexp-match* " " key)))

(define (key->number key)
  (define spaces (count-spaces key))
  (define n1 (string->number (remove-alphas key)))
  (/ n1 spaces))

(define (handshake-solution key1 key2 key3)
  (compute-ans (key->number key1) (key->number key2) key3))

(define (generate-key)
  (define n1 (random RANGE))
  (define n2 (random RANGE))
  (define key1 (number->key n1))
  (define key2 (number->key n2))
  (define key3 (apply bytes (build-list 8 (λ (i) (random 256)))))
  (define ans 
    (compute-ans n1 n2 key3))
  (values key1 key2 key3 ans))

(define (random-char-between botc topc)
  (if (char=? botc topc)
      botc
      (let ()
        (define bot (char->integer botc))
        (define top (char->integer topc))
        (integer->char (+ bot (random (- top bot)))))))

(define (random-alpha-char)
  (case (random 2)
    [(0) (random-char-between #\a #\z)]
    [(1) (random-char-between #\A #\Z)]))

(define (add-spaces how-many s0)
  (list->string
   (reverse
    (for/fold ([s1 empty])
      ([c (in-string s0)]
       [i (in-naturals)])
      (if (i . < . how-many)
          (list* #\space c s1)
          (list* c s1))))))

(define (add-alphas s0)
  (list->string
   (reverse
    (for/fold ([s1 empty])
      ([c (in-string s0)]
       [i (in-naturals)])
      (if (zero? (random 3))
          (list* c s1)
          (list* (random-alpha-char) c s1))))))

(define (number->key n0)
  (define s0 (number->string n0))
  (define how-many-spaces (add1 (random (string-length s0))))
  (define n1 (* n0 how-many-spaces))
  (add-alphas (add-spaces how-many-spaces (number->string n1))))
