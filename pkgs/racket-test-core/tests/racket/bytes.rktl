
(load-relative "loadtest.rktl")

(Section 'bytes)

(require racket/bytes
         racket/stream)

;; ---------- bytes-join ----------
(let ()
  (test #""    bytes-join '() #" ")
  (test #""    bytes-join '(#"") #" ")
  (test #" "   bytes-join '(#"" #"") #" ")
  (test #"x y" bytes-join '(#"x" #"y") #" ")
  (test #"x"   bytes-join '(#"x") #" "))

;; ---------- sequence->bytes ----------
(let ()
  
  (let ()
    (define immutable-bstr #"xyz")
    (test #"xyz" sequence->bytes immutable-bstr)
    (test #true immutable? (sequence->bytes immutable-bstr))
    (test #true eq? (sequence->bytes immutable-bstr) immutable-bstr))

  (let ()
    (define mutable-bstr (bytes 120 121 122))
    (test #"xyz" sequence->bytes mutable-bstr)
    (test #true immutable? (sequence->bytes mutable-bstr))
    (test #false eq? (sequence->bytes mutable-bstr) mutable-bstr))

  (let ()
    (define lst (list 120 121 122))
    (test #"xyz" sequence->bytes lst)
    (test #true immutable? (sequence->bytes lst)))

  (let ()
    (define mutable-vec (vector 120 121 122))
    (test #"xyz" sequence->bytes mutable-vec)
    (test #true immutable? (sequence->bytes mutable-vec)))

  (let ()
    (define immutable-vec (vector->immutable-vector (vector 120 121 122)))
    (test #"xyz" sequence->bytes immutable-vec)
    (test #true immutable? (sequence->bytes immutable-vec)))

  (let ()
    (define byte-stream (stream 120 121 122))
    (test #"xyz" sequence->bytes byte-stream)
    (test #true immutable? (sequence->bytes byte-stream)))
  
  (let ()
    (err/rt-test
     (sequence->bytes "hello") exn:fail:contract? #rx"sequence\\->bytes: contract violation")
    (err/rt-test (sequence->bytes "hello") exn:fail:contract? #rx"use string\\->bytes/utf\\-8")
    (err/rt-test
     (sequence->bytes "hello") exn:fail:contract? #rx"expected: \\(sequence/c bytes\\?\\)")
    (err/rt-test
     (sequence->bytes "hello") exn:fail:contract? #rx"given: \"hello\""))

  (let ()
    (err/rt-test
     (sequence->bytes (vector 120 121 'oops))
     exn:fail:contract?
     #rx"sequence\\->bytes: contract violation")
    (err/rt-test
     (sequence->bytes (vector 120 121 'oops))
     exn:fail:contract?
     #rx"expected: byte\\?")
    (err/rt-test
     (sequence->bytes (vector 120 121 'oops))
     exn:fail:contract?
     #rx"given: 'oops")))

(report-errs)
