
(module test-case mzscheme
  (require (lib "class.ss")
           "wxme.ss"
           "private/class-help.ss")

  (provide reader
           test-case%)

  (define test-case%
    (class object%
      (init-accessible test
                       expected
                       [comment #f]
                       [predicate #f]
                       [should-raise #f]
                       [error-message #f]
                       [enabled? #t]
                       [collapsed? #f]
                       [error-box? #f])
      (super-new)))

  (define (concat port)
    (if port
        (let loop ([accum null])
          (let ([s (read-bytes 4096 port)])
            (if (eof-object? s)
                (apply bytes-append (reverse accum))
                (loop (cons s accum)))))
        #""))

  (define reader
    (new
     (class* object% (snip-reader<%>)
       (define/public (read-header vers stream)
         (void))
       (define/public (read-snip text? cvers stream)
         (let ([v (cond
                   [(= cvers 1)
                    (new test-case% 
                         [comment (send stream read-editor-snip "test-case-box content")] 
                         [test (send stream read-editor-snip "test-case-box test")]
                         [expected (send stream read-editor-snip "test-case-box expected")])]
                   [else
                    (new test-case% 
                         [test (send stream read-editor-snip "test-case-box test")]
                         [expected (send stream read-editor-snip "test-case-box expected")]
                         [predicate (send stream read-editor-snip "test-case-box predicate")]
                         [should-raise (send stream read-editor-snip "test-case-box should-raise")]
                         [error-message (send stream read-editor-snip "test-case-box error-message")]
                         [enabled? (= 1 (send stream read-integer "test-case-box enabled?"))]
                         [collapsed? (= 1 (send stream read-integer "test-case-box collapsed?"))]
                         [error-box? (= 1 (send stream read-integer "test-case-box error-box?"))])])])
           (if text?
               (apply
                bytes-append
                (map concat
                     (send v get-comment)
                     (send v get-test)
                     (send v get-expected)
                     (send v get-predicate)
                     (send v get-should-raise)
                     (send v error-message)))
               v)))
       (super-new)))))
