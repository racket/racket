 #lang racket/base
(require racket/include)

(include "config.rktl")

(define my-prompt (make-continuation-prompt-tag 'mine))

'----------------------------------------

;; This is relatively slow, because it collects backtrace information
'marks-loop
(times
 (let loop ([i M] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (current-continuation-marks)))))

;; Lookup with key not found
'first-none-loop
(times
 (let loop ([i L] [a #f])
   (if (zero? i)
       a
       (loop (sub1 i) (continuation-mark-set-first #f 'key)))))

;; Lookup with key found
'first-some-loop
(times
 (with-continuation-mark
  'key 'val
  (let loop ([i L] [a #f])
    (if (zero? i)
        a
        (loop (sub1 i) (continuation-mark-set-first #f 'key))))))

;; Lookup with key found on the other side of a prompt
'first-some/prompt-loop
(times
 (with-continuation-mark
  'key 'val
  (call-with-continuation-prompt
   (lambda ()
     (let loop ([i L] [a #f])
       (if (zero? i)
           a
           (loop (sub1 i) (continuation-mark-set-first #f 'key)))))
   my-prompt)))

;; Lookup with key found in distant frame
'first-some/deep-loop
(times
 (with-continuation-mark
  'key 'val
  (let loop ([j 100])
    (cond
      [(zero? j)
       (let loop ([i L] [a #f])
         (if (zero? i)
             a
             (loop (sub1 i) (continuation-mark-set-first #f 'key))))]
      [(odd? j)
       (list (loop (sub1 j)))]
      [else
       (car (loop (sub1 j)))]))))

;; Lookup with key found in distant frame with lots of other
;;  keys on frames in between
'first-some/deep/push-loop
(times
 (with-continuation-mark
  'key 'val
  (let loop ([j 100])
    (cond
      [(zero? j)
       (let loop ([i L] [a #f])
         (if (zero? i)
             a
             (loop (sub1 i) (continuation-mark-set-first #f 'key))))]
      [(odd? j)
       (list  (with-continuation-mark
               'other 'val
               (loop (sub1 j))))]
      [else
       (car (loop (sub1 j)))]))))

;; Lookup with key *not* found with lots of other
;;  keys on frames
'first-none/deep/push-loop
(times
 (let loop ([j 100])
   (cond
     [(zero? j)
      (let loop ([i L] [a #f])
        (if (zero? i)
            a
            (loop (sub1 i) (continuation-mark-set-first #f 'key))))]
     [(odd? j)
      (list  (with-continuation-mark
              'other 'val
              (loop (sub1 j))))]
     [else
      (car (loop (sub1 j)))])))

;; Lookup with key found in distant frame with lots of prompts
;;  and other keys on frames in between
'first-some/deep/prompt/push-loop
(times
 (with-continuation-mark
  'key 'val
  (let k-loop ([k 100])
    (let loop ([j 100])
      (cond
        [(zero? j)
         (if (zero? k)
             (let loop ([i M] [a #f])
               (if (zero? i)
                   a
                   (loop (sub1 i) (continuation-mark-set-first #f 'key))))
             (call-with-continuation-prompt
              (lambda ()
                (k-loop (sub1 k)))
              my-prompt))]
        [(odd? j)
         (list  (with-continuation-mark
                 'other 'val
                 (loop (sub1 j))))]
        [else
         (car (loop (sub1 j)))])))))

;; Like the previous one, but prompt makes only marks within
;;  the last prompt relevant
'first-some/deep/stop-prompt/push-loop
(times
 (with-continuation-mark
  'key 'val
  (let k-loop ([k 100])
    (let loop ([j 100])
      (cond
        [(zero? j)
         (if (zero? k)
             (let loop ([i M] [a #f])
               (if (zero? i)
                   a
                   (loop (sub1 i) (continuation-mark-set-first #f 'key))))
             (call-with-continuation-prompt
              (lambda ()
                (k-loop (sub1 k)))))]
        [(odd? j)
         (list  (with-continuation-mark
                 'other 'val
                 (loop (sub1 j))))]
        [else
         (car (loop (sub1 j)))])))))
