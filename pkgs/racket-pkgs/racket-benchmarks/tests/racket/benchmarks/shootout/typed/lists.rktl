(require compatibility/mlist)
(define SIZE 10000)

(: sequence (Integer Integer -> (MListof Integer)))
(define (sequence start stop)
  (if (> start stop)
      '()
      (mcons start (sequence (+ start 1) stop))))

(: head-to-tail! ((MListof Integer) (MListof Integer)
                  -> (values (MListof Integer) (MListof Integer))))
(define (head-to-tail! headlist taillist)
  (when (null? taillist) (begin
                           (set! taillist (ann (mlist (mcar headlist)) (MListof Integer)))
                           (set! headlist (mcdr headlist))))
  (letrec: ((htt-helper : ((MListof Integer) -> Void)
                        (lambda (dest)
                          (when (not (null? headlist))
                            (let ((headlink headlist))
                              (set-mcdr! dest headlink)
                              (set! headlist (mcdr headlist))
                              (htt-helper headlink))))))
    (htt-helper taillist)
    (values headlist taillist)))

(: test-lists ( -> Integer))
(define (test-lists)
  (let*: ([L1 : (MListof Integer) (sequence 1 SIZE)]
          [L2 : (MListof Integer) (mappend L1 '())]
          [L3 : (MListof Integer) '()])
    (set!-values (L2 L3) (head-to-tail! L2 L3))
    (set!-values (L3 L2) (head-to-tail! (mreverse! L3) L2))
    (set! L1 (mreverse! L1))
    (cond ((not (= SIZE (mcar L1))) 0)
          ((not (equal? L1 L2))    0)
          (else           (mlength L1)))))

(: main ((Vectorof String) -> Void))
(define (main args)
  (let: ((result : Integer 0))
    (let loop ((counter (if (= (vector-length args) 0)
                            1
                            (assert (string->number (vector-ref args 0)) exact-integer?))))
      (when (> counter 0)
        (set! result (test-lists))
        (loop (- counter 1))))
    (printf "~s\n" result)))

(main (current-command-line-arguments))
