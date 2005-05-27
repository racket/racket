;; Mike Burns, July 8th, 2004, netgeek@speakeasy.net
;; Test async-channel:
(module test-channel mzscheme
  (require (lib "test.ss" "schemeunit")
           (lib "channel.ss" "web-server"))

  (provide test-channel)

  (define test-channel
    (let ((c (make-async-channel))
          (v (gensym)))
      (make-test-suite
        "Test async-channel"
        (make-test-case
          "async-channel-get-available of the empty channel"
          (assert-eq? (void) (async-channel-get-available c (lambda (x) #f))))
        (make-test-case
          "async-channel-get-available of the non-empty channel"
          (assert-false (begin (async-channel-put c v)
                               (async-channel-get-available c (lambda (x) #f)))))
        (make-test-case
          "async-channel-try-get of the empty channel"
          (assert-false (async-channel-try-get c (lambda () #f))))
        (make-test-case
          "async-channel-try-get of the non-empty channel" 
          (assert-eq? v (begin (async-channel-put c v)
                               (async-channel-try-get c (lambda () #f))))))))
  )
