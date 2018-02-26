#lang racket/base
(require "chyte.rkt"
         "chyte-case.rkt"
         "../common/range.rkt")

(provide parse-class
         parse-posix-char-class)

;; returns (values success? range pos)
(define (parse-class s pos config)
  ;; We know there's at least one character
  (define (success v) (values #t v (add1 pos)))
  (chyte-case
   (chytes-ref s pos)
   [(#\d) (success (range:d))]
   [(#\D) (success (range-invert (range:d) (chytes-limit s)))]
   [(#\w) (success (range:w))]
   [(#\W) (success (range-invert (range:w) (chytes-limit s)))]
   [(#\s) (success (range:s))]
   [(#\S) (success (range-invert (range:s) (chytes-limit s)))]
   [else (values #f #f #f)]))
         
(define (range:d)
  (range-add-span empty-range (chyte #\0) (chyte #\9)))

(define (range:w)
  (range-add
   (range-add-span
    (range-add-span
     (range:d)
     (chyte #\a) (chyte #\z))
    (chyte #\A) (chyte #\Z))
   (chyte #\_)))

(define (range:s)
  (let* ([r (range-add empty-range (chyte #\space))]
         [r (range-add r (chyte #\tab))]
         [r (range-add r (chyte #\newline))]
         [r (range-add r (chyte #\page))]
         [r (range-add r (chyte #\return))])
    r))

;; ----------------------------------------

;; Returns (values success? range position)
(define (parse-posix-char-class s pos)
  (chyte-case/eos
   s pos
   [(#\:)
    (define class
      (let loop ([accum null] [pos (add1 pos)])
        (cond
         [(= pos (chytes-length s)) #f]
         [else
          (define c (chytes-ref s pos))
          (cond
           [(and (c . >= . (chyte #\a)) (c . <= . (chyte #\z)))
            (loop (cons c accum) (add1 pos))]
           [(and (= c (chyte #\:))
                 ((add1 pos) . < . (chytes-length s))
                 (= (chytes-ref s (add1 pos)) (chyte #\])))
            (list->bytes (reverse accum))]
           [else #f])])))
    (define range
      (case class
        [(#"alpha") (range-add-span 
                     (range-add-span 
                      empty-range
                      (chyte #\a) (chyte #\z))
                     (chyte #\A) (chyte #\Z))]
        [(#"upper") (range-add-span 
                     empty-range
                     (chyte #\A) (chyte #\Z))]
        [(#"lower") (range-add-span 
                     empty-range
                     (chyte #\a) (chyte #\z))]
        [(#"digit") (range-add-span 
                     empty-range
                     (chyte #\0) (chyte #\9))]
        [(#"xdigit") (range-add-span 
                      (range-add-span 
                       (range-add-span 
                        empty-range
                        (chyte #\0) (chyte #\9))
                       (chyte #\a) (chyte #\f))
                      (chyte #\A) (chyte #\F))]
        [(#"alnum") (range-add-span 
                     (range-add-span 
                      (range-add-span 
                       empty-range
                       (chyte #\0) (chyte #\9))
                      (chyte #\a) (chyte #\z))
                     (chyte #\A) (chyte #\Z))]
        [(#"word") (range-add
                    (range-add-span 
                     (range-add-span 
                      empty-range
                      (chyte #\a) (chyte #\z))
                     (chyte #\A) (chyte #\Z))
                    (chyte #\_))]
        [(#"blank") (range-add
                     (range-add empty-range (chyte #\space))
                     (chyte #\tab))]
        [(#"space") (range:s)]
        [(#"graph" #"print")
         (define range
           (for/fold ([range empty-range]) ([i (in-range 0 128)])
             (if (char-graphic? (integer->char i))
                 (range-add range i)
                 range)))
         (if (equal? class #"print")
             (range-add
              (range-add range (chyte #\space))
              (chyte #\tab))
             range)]
        [(#"cntrl") (range-add-span empty-range 0 31)]
        [(#"ascii") (range-add-span empty-range 0 127)]
        [else #f]))
    (if range
        (values #t range (+ pos 3 (bytes-length class)))
        (values #f #f #f))]
   [else (values #f #f #f)]))
