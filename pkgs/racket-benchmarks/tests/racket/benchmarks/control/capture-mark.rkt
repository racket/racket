#lang racket/base

(require racket/include)

(include "config.rktl")

(define my-prompt (make-continuation-prompt-tag 'mine))

'----------------------------------------

'compose-baseline
(times
 (let ([k (let/ec esc
            (call-with-continuation-prompt
             (lambda ()
               (let loop ([j 100])
                 (if (zero? j)
                     (call-with-composable-continuation
                      (lambda (k)
                        (esc k)))
                     (add1 (loop (sub1 j))))))))])
   (let loop ([i Q] [v #f])
     (if (zero? i)
        v
        (loop (sub1 i) (k 0))))))

'compose-deep-marks
(times
 (let ([k (let/ec esc
            (call-with-continuation-prompt
             (lambda ()
               (let loop ([j 100])
                 (if (zero? j)
                     (call-with-composable-continuation
                      (lambda (k)
                        (esc k)))
                     (with-continuation-mark
                      'key j
                      (add1 (loop (sub1 j)))))))))])
   (let loop ([i Q] [v #f])
     (if (zero? i)
        v
        (loop (sub1 i) (k 0))))))

'compose-deep/prompt-marks
(times
 (let ([k (let/ec esc
            (call-with-continuation-prompt
             (lambda ()
               (let loop ([j 100])
                 (if (zero? j)
                     (call-with-composable-continuation
                      (lambda (k)
                        (esc k)))
                     (call-with-continuation-prompt
                      (lambda ()
                        (with-continuation-mark
                         'key j
                         (add1 (loop (sub1 j)))))
                      my-prompt))))))])
   (let loop ([i Q] [v #f])
     (if (zero? i)
        v
        (loop (sub1 i) (k 0))))))

;; Like the previous case, but the composable
;;  continuation doesn't start with a mark
;;  that needs to be spliced
'compose-deep/prompt/nosplice-marks
(times
 (let ([k (let/ec esc
            (call-with-continuation-prompt
             (lambda ()
               (add1
                (let loop ([j 100])
                  (if (zero? j)
                      (call-with-composable-continuation
                       (lambda (k)
                         (esc k)))
                      (call-with-continuation-prompt
                       (lambda ()
                         (with-continuation-mark
                          'key j
                          (add1 (loop (sub1 j)))))
                       my-prompt)))))))])
   (let loop ([i Q] [v #f])
     (if (zero? i)
        v
        (loop (sub1 i) (k 0))))))
