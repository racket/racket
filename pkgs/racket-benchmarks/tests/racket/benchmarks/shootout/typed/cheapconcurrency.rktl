(require scheme/cmdline)

(: generate ((Channelof Natural) Natural -> (Channelof Natural)))
(define (generate receive-ch n)
  (if (zero? n)
      receive-ch
      (let: ([ch : (Channelof Natural) (make-channel)])
        (thread (lambda ()
                  (let: loop : Void ()
                    (channel-put ch (add1 (channel-get receive-ch)))
                    (loop))))
        (generate ch (sub1 n)))))

(let ([n (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?))])
  (let*: ([start-ch : (Channelof Natural) (make-channel)]
          [end-ch : (Channelof Natural) (generate start-ch 500)])
    (let: loop : Void ([n : Integer n][total : Integer 0])
      (if (zero? n)
          (printf "~a\n" total)
          (begin
            (channel-put start-ch 0)
            (loop (sub1 n)
                  (+ total (channel-get end-ch))))))))
