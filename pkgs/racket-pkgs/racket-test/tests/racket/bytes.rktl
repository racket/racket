
(load-relative "loadtest.rktl")

(Section 'bytes)

(require racket/bytes)

;; ---------- bytes-join ----------
(let ()
  (test #""    bytes-join '() #" ")
  (test #""    bytes-join '(#"") #" ")
  (test #" "   bytes-join '(#"" #"") #" ")
  (test #"x y" bytes-join '(#"x" #"y") #" ")
  (test #"x"   bytes-join '(#"x") #" "))


(report-errs)
