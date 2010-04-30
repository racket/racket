#lang racket
(require racket/system)

#|

This test is designed to to check whether meta-continuations are
correctly split when a continuation is delimited in the middle of
a meta-continuation other than the current one. In particular,
the `x' binding is part of the deeper meta-continuation when `ak'
is captured, but it is delimited inside the binding, so `x'
should not be reated in `ak'.

The test is implemented using `dump-memory-stats' in another racket
process.

|#

(when (equal? #() (current-command-line-arguments))
  (let ([f (find-executable-path (find-system-path 'exec-file) #f)])
    (let ([p (open-output-bytes)])
      (parameterize ([current-error-port p])
        (system* f "-l" "tests/racket/prompt-sfs" "sub"))
      (unless (regexp-match? #rx"<will-executor>: +1 +" (get-output-bytes p))
        (error "wrong output")
        (exit 1))))
  (exit 0))

(define (make-big-thing) (cons (make-string 100000) (make-will-executor)))
(define (show-big-thing say x) (say (string-length (car x))))

(collect-garbage)
(collect-garbage)
(define orig (current-memory-use))

(define single (make-will-executor))

(let loop ([n 10][accums null])
  (if (zero? n)
      (begin
        (collect-garbage)
        (collect-garbage)
        (dump-memory-stats) ; look for just one <will-executor>
        (printf "~s\n" (- (current-memory-use) orig))
        accums)
      (let* ([says null]
             [say (lambda (s)
                    (set! says (cons s says)))]
             [a (make-continuation-prompt-tag 'a)]
             [b (make-continuation-prompt-tag 'b)])
        (let ([ak
               (let ([x (make-big-thing)])
                 (begin0
                  (call-with-continuation-prompt
                   (lambda ()
                     (with-continuation-mark 'y "y0"
                       (let ([bk (call-with-continuation-prompt
                                  (lambda ()
                                    (let ([f (call-with-composable-continuation
                                              (lambda (k)
                                                (lambda () k))
                                              b)])
                                      (say "bcall")
                                      (begin0
                                       (f)
                                       (say "breturn"))))
                                  b)])
                         (call-with-continuation-prompt
                          (lambda ()
                            ((bk (lambda ()
                                   (let ([f (call/cc (lambda (k) (lambda () (lambda () k))) a)])
                                     (begin0
                                      (f)
                                      (say "areturn")))))))
                          b))))
                   a)
                  (show-big-thing say x)))])
          (loop (sub1 n) (cons ak accums))))))
