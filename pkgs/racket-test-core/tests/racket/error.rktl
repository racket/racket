(load-relative "../racket/loadtest.rktl")

(Section 'error)

(test #t parameter? current-error-message-adjuster)

(test #f (current-error-message-adjuster) 'i-just-made-up-this-new-mode)
(err/rt-test ((current-error-message-adjuster) "oops"))

(define-syntax-rule (test-error-match rx e)
  (test #t
        regexp-match?
        rx
        (with-handlers ([exn:fail? exn-message])
          e
          "no error")))

(let ()
  (define (adjuster mode)
    (case mode
      [(name)
       (lambda (name realm)
         (case realm
           [(racket/primitive)
            (values
             (case name
               [(cons) 'kons]
               [(vector-ref) 'vector/ref]
               [(bytes-ref) 'bytes/ref]
               [(thread-wait) 'wait-thread]
               [(regexp-match) 'rx-match]
               [(read-char) 'read/char]
               [else name])
             'mars)]
           [else (values name realm)]))]
      [else #f]))
  (define (mangle-adjuster mode)
    (case mode
      [(name) (lambda (name realm)
                ;; mangle
                (values (string->symbol (format "!!~a" name))
                        (string->symbol (format "~a!!" realm))))]
      [else #f]))
  (define (unmangle-adjuster mode)
    (case mode
      [(name) (lambda (name realm)
                ;; unmagle
                (values (string->symbol (substring (format "~a" name) 2))
                        (let ([s (format "~a" realm)])
                          (string->symbol (substring s 0 (- (string-length s) 2))))))]
      [else #f]))
  (define (check)
    (test-error-match #rx"^kons" (cons 1))
    (test-error-match #rx"^vector/ref" (vector-ref 1 2))
    (test-error-match #rx"^vector/ref" (vector-ref '#(1) 2))
    (test-error-match #rx"^bytes/ref" (bytes-ref 1 2))
    (test-error-match #rx"^bytes/ref" (bytes-ref #"1" 2))
    (test-error-match #rx"^bytes/ref" (bytes-ref 1))
    (test-error-match #rx"^wait-thread" (thread-wait "not a thread"))
    (test-error-match #rx"^rx-match" (regexp-match 10))
    (test-error-match #rx"^read/char" (read-char (open-output-bytes)))
    (test-error-match #rx"^read/char" (let ([p (open-input-bytes #"")])
                                        (close-input-port p)
                                        (read-char p)))
    
    (test-error-match #rx"^cons" (raise-argument-error 'cons "string?" 17))
    (test-error-match #rx"^cons" (raise-result-error 'cons "string?" 17))
    (test-error-match #rx"^cons" (raise-arity-error 'cons 1 'a 'b))
    (test-error-match #rx"^cons" (raise-arity-mask-error 'cons 2 'a 'b))
    (test-error-match #rx"^cons" (raise-range-error 'cons "pair" "" 3 '(1 . 2) 0 1))
    (test-error-match #rx"^cons" (raise-range-error 'cons "pair" "" 3 '(1 . 2) 0 1 0))
    (test-error-match #rx"^cons" (let ([cons (lambda (x) x)])
                                   (cons 1 2)))
    (test-error-match #rx"^cons: expect" (raise-type-error 'cons "something" 5))
    
    (test-error-match #rx"^kons" (raise-argument-error* 'cons 'racket/primitive "string?" 17))
    (test-error-match #rx"^kons" (raise-result-error* 'cons 'racket/primitive "string?" 17))
    (test-error-match #rx"^kons" (raise-arity-error* 'cons 'racket/primitive 1 'a 'b))
    (test-error-match #rx"^kons" (raise-arity-mask-error* 'cons 'racket/primitive 2 'a 'b))
    (test-error-match #rx"^kons" (raise-range-error* 'cons 'racket/primitive "pair" "" 3 '(1 . 2) 0 1))
    (test-error-match #rx"^kons" (raise-range-error* 'cons 'racket/primitive "pair" "" 3 '(1 . 2) 0 1 0)))
  (parameterize ([current-error-message-adjuster adjuster])
    (check))
  (with-continuation-mark
   error-message-adjuster-key
   adjuster
   (begin
     (check)
     (void)))
  (parameterize ([current-error-message-adjuster adjuster])
    (with-continuation-mark
     error-message-adjuster-key
     unmangle-adjuster
     (begin
       (with-continuation-mark
        error-message-adjuster-key
        mangle-adjuster
        (check))
       (void)))))

(parameterize ([current-error-message-adjuster
                (lambda (mode)
                  (case mode
                    [(message)
                     (lambda (who who-realm str str-realm)
                       (if (eq? str-realm 'racket/primitive)
                           (values #f 'mars (format "~a>> ~a" who str) 'mars)
                           (values who who-realm str str-realm)))]
                    [else #f]))])
  (test-error-match #rx"^cons>> arity mismatch" (cons 1))
  (test-error-match #rx"^f>> arity mismatch" (let ([f (lambda (x y) x)])
                                               (f 1)))
  (test-error-match #rx"^vector-ref>> index is out of range" (vector-ref '#(1 2 3) 10))
  (test-error-match #rx"^vector[*]-ref>> index is out of range" (vector*-ref '#(1 2 3) 10))
  (test-error-match #rx"^f: arity mismatch" (error 'f "arity mismatch")))

(parameterize ([current-error-message-adjuster
                (lambda (mode)
                  (case mode
                    [(contract)
                     (lambda (ctc realm)
                       (values (case (and (eq? realm 'racket/primitive)
                                          ctc)
                                 [("number?") "number/c"]
                                 [else ctc])
                               'mars))]
                    [else #f]))])
  (test-error-match #rx"expected: number/c" (+ 'a 'b))

  (test-error-match #rx"expected: number[?]" (raise-argument-error 'plus "number?" 'a)))

(parameterize ([current-error-message-adjuster
                (lambda (mode)
                  (case mode
                    [(message)
                     (lambda (who who-realm str str-realm)
                       (if (and (eq? who 'application)
                                (eq? str-realm 'racket/primitive))
                           (values '|function call| 'mars
                                   "bad call" 'mars)
                           (values who who-realm str str-realm)))]
                    [else #f]))])
  (test-error-match #rx"^function call: bad call" (1 2))
  (test-error-match #rx"^function call: bad call" (1 #:x 2)))
  
(report-errs)
