
(load-relative "loadtest.rktl")

(Section 'bytes)

(require racket/bytes)

;; ---------- bytes-append* ----------
(err/rt-test (bytes-append* (vector)) exn:fail:contract? #rx"(listof bytes?)")
(err/rt-test (bytes-append* (list "a")) exn:fail:contract? #rx"(listof bytes?)")
(err/rt-test (bytes-append* "a" (list #"b")) exn:fail:contract? #rx"bytes?")
(err/rt-test (bytes-append*) exn:fail:contract? #rx"arity mismatch")
(test #"abc" bytes-append* #"a" #"b" (list #"c"))

;; ---------- bytes-join ----------
(let ()
  (test #""    bytes-join '() #" ")
  (test #""    bytes-join '(#"") #" ")
  (test #" "   bytes-join '(#"" #"") #" ")
  (test #"x y" bytes-join '(#"x" #"y") #" ")
  (test #"x"   bytes-join '(#"x") #" ")
  (let ((s #"abcd"))
    (test #f eq? (bytes-join (list s) #" ") s)
    (test #t bytes=? (bytes-join (list s) #" ") s)))


(report-errs)
