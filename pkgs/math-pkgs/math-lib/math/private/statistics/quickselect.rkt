#lang typed/racket/base

#|
Quickselect to find the kth order statistic, using random pivot to reduce probability of worst case

Technically O(n^2), but astronomically unlikely; expected running time is O(n)

Seems to be faster than sort-and-direct-ref on sequences with length > 350 or so, not much slower
otherwise
|#

(require racket/fixnum
         "../unsafe.rkt"
         "statistics-utils.rkt")

(provide kth-value! kth-value)

(: partition! (All (A) ((Vectorof A) Fixnum Fixnum (A A -> Any) -> Fixnum)))
(define (partition! vs start end lt?)
  (define p (+ start (random (unsafe-fx- (unsafe-fx+ end 1) start))))
  (define pivot (unsafe-vector-ref vs p))
  (unsafe-vector-set! vs p (unsafe-vector-ref vs end))
  (unsafe-vector-set! vs end pivot)
  (let loop ([#{start : Fixnum} start] [#{end : Fixnum} end])
    (cond [(start . fx< . end)
           (define v1 (unsafe-vector-ref vs start))
           (cond [(lt? v1 pivot)  (loop (unsafe-fx+ start 1) end)]
                 [else
                  (unsafe-vector-set! vs end v1)
                  (let ([end  (unsafe-fx- end 1)])
                    (unsafe-vector-set! vs start (unsafe-vector-ref vs end))
                    (loop start end))])]
          [else
           (unsafe-vector-set! vs start pivot)
           start])))

(: kth-value!* (All (A) ((Vectorof A) Nonnegative-Fixnum (A A -> Any) -> A)))
(define (kth-value!* vs k lt?)
  (define n (vector-length vs))
  (let loop ([#{start : Fixnum} 0] [#{end : Fixnum} (fx- n 1)] [k k])
    (cond [(start . fx< . end)
           (let ([c  (partition! vs start end lt?)])
             (define start+k (unsafe-fx+ start k))
             (cond [(c . fx> . start+k)
                    (loop start (unsafe-fx- c 1) k)]
                   [(c . fx< . start+k)
                    (define c+1 (unsafe-fx+ c 1))
                    (loop c+1 end (unsafe-fx- start+k c+1))]
                   [else
                    (unsafe-vector-ref vs start+k)]))]
          [else
           (unsafe-vector-ref vs start)])))

(: kth-value! (All (A) ((Vectorof A) Integer (A A -> Any) -> A)))
(define (kth-value! vs k lt?)
  (define n (vector-length vs))
  (cond [(n . fx<= . 0)  (raise-argument-error 'kth-value! "nonempty Vector" 0 vs k lt?)]
        [(k . < . 0)  (raise-argument-error 'kth-value! "Natural" 1 vs k lt?)]
        [(k . >= . n)  (raise-argument-error 'kth-value! (format "Natural < ~a" n) 1 vs k lt?)]
        [else  (kth-value!* vs k lt?)]))

(: kth-value (All (A) ((Sequenceof A) Integer (A A -> Any) -> A)))
(define (kth-value seq k lt?)
  (define vs (sequence->vector seq))
  (define n (vector-length vs))
  (cond [(n . fx<= . 0)  (raise-argument-error 'kth-value "nonempty Sequence" 0 seq k lt?)]
        [(k . < . 0)  (raise-argument-error 'kth-value "Natural" 1 seq k lt?)]
        [(k . >= . n)  (raise-argument-error 'kth-value (format "Natural < ~a" n) 1 seq k lt?)]
        [else  (kth-value!* vs k lt?)]))
