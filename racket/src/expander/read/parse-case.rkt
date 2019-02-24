#lang racket/base
(require racket/fixnum
         (for-syntax racket/base))

;; `parse-case` is used for numebr parsing in "number.rkt"

(provide parse-case
         parse-case*
         digit)

(define-syntax digit #f)

(define (maybe-digit c radix)
  (define v (char->integer c))
  (cond
    [(v . fx< . (char->integer #\0)) c]
    [(v . fx< . (fx+ (fxmin radix 10) (char->integer #\0)))
     (fx- v (char->integer #\0))]
    [(radix . fx<= . 10) c]
    [(v . fx< . (char->integer #\A)) c]
    [(v . fx< . (+ radix (- (char->integer #\A) 10)))
     (fx- v (- (char->integer #\A) 10))]
    [(v . fx< . (char->integer #\a)) c]
    [(v . fx< . (+ radix (- (char->integer #\a) 10)))
     (fx- v (- (char->integer #\a) 10))]
    [else c]))

;; Extracts the byte at index `start` of `s`, or produces 'eof if
;; `start` is `end`. Binds the digit value, character, or 'eof to
;; `var`. Each `clause` is as in `case`, but more frequently used
;; clauses should be first. Assumes that `start` and `end` can be
;; duplicated.
(define-syntax-rule (parse-case s start end radix => var clause ...)
  (let* ([var (if (fx= start end)
                  'eof
                  (let ([c (string-ref s start)])
                    (maybe-digit c radix)))])
    (parse/case var clause ...)))

(define-syntax parse/case
  (syntax-rules (else)
    [(_ var) (void)]
    [(_ var [else body ...])
     (let () body ...)]
    [(_ var [(datum ...) body ...] clause ...)
     (if (parse-matches? var (datum ...))
         (let ()
           body ...)
         (parse/case var clause ...))]))

(define-syntax parse-matches?
  (syntax-rules (digit)
    [(_ var ()) #f]
    [(_ var (digit . datums))
     (or (fixnum? var) (parse-matches? var datums))]
    [(_ var (datum . datums))
     (or (eqv? var 'datum) (parse-matches? var datums))]))

;; Nests a sequence of matches with a shared "else"
(define-syntax parse-case*
  (syntax-rules (else)
    [(_ s start end [[] body ...] [else body2 ...])
     (let ()
       body ...)]
    [(_ s (fx+ start n) end
        [[datums . datums-rest] body ...]
        [else body2 ...])
     (let ([fail (lambda () body2 ...)])
       (let* ([start+n (fx+ start n)]
              [var (if (fx= start+n end)
                       'eof
                       (string-ref s start+n))])
         (case var
           [datums
            (parse-case*
             s (fx+ start (+ n 1)) end
             [datums-rest body ...]
             [else (fail)])]
           [else (fail)])))]
    [(_ s start . rest)
     (parse-case* s (fx+ start 0) . rest)]))
