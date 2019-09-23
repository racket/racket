;; This file is meant to be run in Scheme

(load "setup.rktl")
(load "config.rktl")

;; Not inlined:
(define (f x) x)

(show '----------------------------------------)

;; Compare to 'pair-loop; the difference is the time to check
;;  that the continuation is reified and pop the old attachementt
(show 'attachment-set-loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        (car (current-continuation-attachments))
        (call-setting-continuation-attachment
         i
         (lambda ()
           (loop (sub1 i))))))))

;; Compare to 'loop; the diffrenmce is the time to check
;;  for an attachment that is never there
(show 'attachment-get-loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        0
        (call-getting-continuation-attachment
         1
         (lambda (v)
           (loop (- i v))))))))

;; Compare to 'loop; the diffrenmce is the time to check
;;  for an attachment --- discovering the reified continuation
;;  that indicates that the attachment is there
(show 'attachment-get-loop/has-attachment)
(show
 (times
  (call-setting-continuation-attachment
   1
   (lambda ()
     (let loop ([i N])
       (if (zero? i)
           0
           (call-getting-continuation-attachment
            1
            (lambda (v)
              (loop (- i v))))))))))

;; Combines the overheads of 'attachment-get-loop and 'attachment-set-loop
(show 'attachment-get-set-loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        (car (current-continuation-attachments))
        (call-getting-continuation-attachment
         1
         (lambda (v)
           (call-setting-continuation-attachment
            i
            (lambda ()
              (loop (sub1 i))))))))))

;; Like 'attachment-get-loop and 'attachment-set-loop, but
;;  "cosume" insteda of "get" sets up a faster "set" because
;;  it doesn't have to re-check for reified or attachment
(show 'attachment-consume-set-loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        (car (current-continuation-attachments))
        (call-consuming-continuation-attachment
         0
         (lambda (v)
           (call-setting-continuation-attachment
            i
            (lambda ()
              (loop (sub1 i))))))))))

;; Simulate `with-continuation-mark`
(show 'attachment-consume-pair-set-loop)
(show
 (times
  (let loop ([i N])
    (if (zero? i)
        (car (current-continuation-attachments))
        (call-consuming-continuation-attachment
         #f
         (lambda (v)
           (call-setting-continuation-attachment
            (if v
                (cons i (cdr v))
                (cons i #f))
            (lambda ()
              (loop (sub1 i))))))))))

;; Build a long chain of attachments, where the continuation
;;  doesn't have to be reified because the compiler can add
;;  a "push" and "pop" around the `add1` call
;; Most of the time here is GC time to deal with the growing
;;  chain of attachments
(show 'attachment-set-nontail-easy)
(show
 (times
  (let loop ([i M])
    (if (zero? i)
        (car (current-continuation-attachments))
        (add1
         (call-setting-continuation-attachment
          i
          (lambda ()
            (add1 (loop (sub1 i))))))))))

;; Build a long chain of attachments *and* continuaiton frames;
;;  the continuation frame is reified for the current `loop`
;;  call, since `call-setting-continuation-attachment` is in
;;  tail position
;; Most of the time here is GC time, because
;;  this creates a chain of continuation records;
;;  compare to 'k-nontail
(show 'attachment-set-nontail-outside)
(show
 (times
  (let loop ([i M])
    (if (zero? i)
        (car (current-continuation-attachments))
        (call-setting-continuation-attachment
         i
         (lambda ()
           (add1 (loop (sub1 i)))))))))

;; Like the "outside" version, but the continuation frame is
;;  reified on the previous `loop` call to move the attachment
;;  to the frame
;; Most of the time here is GC time, still
(show 'attachment-set-nontail-inside)
(show
 (times
  (let loop ([i M])
    (if (zero? i)
        (car (current-continuation-attachments))
        (add1
         (call-setting-continuation-attachment
          i
          (lambda ()
            (loop (sub1 i)))))))))

;; Like 'attachment-set-nontail-inside in that the attachment
;;  is moved to a reified frame for a non-tail call, but
;;  no chain of frames is created
;; A frame cache pays off here, because the reified frame is
;;  short-lived
(show 'attachment-nontail-argument-loop)
(show
 (times
  (let ([f f])
    (let loop ([i N])
      (if (zero? i)
          0
          (loop (call-setting-continuation-attachment
                 i
                 (lambda ()
                   (f (sub1 i))))))))))

;; Like 'attachment-nontail-argument-loop, but with a
;;  `cons` to simulate a key--value mapping
(show 'attachment-nontail-argument-pair-loop)
(show
 (times
  (let ([f f])
    (let loop ([i N])
      (if (zero? i)
          0
          (loop (call-setting-continuation-attachment
                 (cons 'key i)
                 (lambda ()
                   (f (sub1 i))))))))))

;; Since the compiler knows about `sub1`, it doesn't have
;;  to reify the continuation frame
(show 'attachment-nontail-argument-loop-easy)
(show
 (times
  (let ([f f])
    (let loop ([i N])
      (if (zero? i)
          0
          (loop (f (call-setting-continuation-attachment
                    i
                    (lambda ()
                      (sub1 i))))))))))


;; Like 'attachment-nontail-argument-loop-easy, but
;;  with a pair for the attachment
(show 'attachment-nontail-argument-pair-loop-easy)
(show
 (times
  (let ([f f])
    (let loop ([i N])
      (if (zero? i)
          0
          (loop (f (call-setting-continuation-attachment
                    (cons 'key i)
                    (lambda ()
                      (sub1 i))))))))))
