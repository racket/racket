#lang racket
(require net/head tests/eli-tester)

;; a few tests of head.rkt -- JBC, 2006-07-31

(provide tests)
(module+ main (tests))
(define (tests)
  (define test-header
    (string-append "From: abc\r\nTo: field is\r\n continued\r\n"
                   "Another: zoo\r\n continued\r\n\r\n"))
  (define test-header/bytes
    (bytes-append #"From: abc\r\nTo: field is\r\n continued\r\n"
                  #"Another: zoo\r\n continued\r\n\r\n"))
  (test

   (validate-header "From: me@here.net\r\n\r\n")
   (validate-header #"From: me@here.net\r\n\r\n")
   (validate-header "From: a\r\nTo: b\r\nResent-to: qrv@erocg\r\n\r\n")
   (validate-header #"From: a\r\nTo: b\r\nResent-to: qrv@erocg\r\n\r\n")

   (validate-header "From: a\r\nTo: b\r\nMissingTrailingrn: qrv@erocg\r\n")
   =error> "missing ending CRLF"
   (validate-header #"From: a\r\nTo: b\r\nMissingTrailingrn: qrv@erocg\r\n")
   =error> "missing ending CRLF"
   (validate-header "From: a\r\nnocolon inthisline\r\n\r\n")
   =error> "ill-formed header"
   (validate-header #"From: a\r\nnocolon inthisline\r\n\r\n")
   =error> "ill-formed header"
   (validate-header "From: a\r\nMissingReturn: och\n\r\n")
   =error> "missing ending CRLF"
   (validate-header #"From: a\r\nMissingReturn: och\n\r\n")
   =error> "missing ending CRLF"
   (validate-header "From: a\r\nSpacein Fieldname: och\r\n\r\n")
   =error> "ill-formed header"
   (validate-header #"From: a\r\nSpacein Fieldname: och\r\n\r\n")
   =error> "ill-formed header"

   (extract-field "From" test-header)
   => "abc"
   (extract-field #"From" test-header/bytes)
   => #"abc"
   (extract-field "To" test-header)
   => "field is\r\n continued"
   (extract-field #"To" test-header/bytes)
   => #"field is\r\n continued"
   (extract-field "Another" test-header)
   => "zoo\r\n continued"
   (extract-field #"Another" test-header/bytes)
   => #"zoo\r\n continued"

   (replace-field "From" "def" test-header)
   => "From: def\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field #"From" #"def" test-header/bytes)
   => #"From: def\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field "From" #f test-header)
   => "To: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field #"From" #f test-header/bytes)
   => #"To: field is\r\n continued\r\nAnother: zoo\r\n continued\r\n\r\n"

   (replace-field "To" "qrs" test-header)
   => "From: abc\r\nTo: qrs\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field #"To" #"qrs" test-header/bytes)
   => #"From: abc\r\nTo: qrs\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field "To" #f test-header)
   => "From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
   (replace-field #"To" #f test-header/bytes)
   => #"From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"

   (replace-field "Another" "abc\r\n def" test-header)
   => "From: abc\r\nTo: field is\r\n continued\r\nAnother: abc\r\n def\r\n\r\n"
   (replace-field #"Another" #"abc\r\n def" test-header/bytes)
   => #"From: abc\r\nTo: field is\r\n continued\r\nAnother: abc\r\n def\r\n\r\n"
   (replace-field "Another" #f test-header)
   => "From: abc\r\nTo: field is\r\n continued\r\n\r\n"
   (replace-field #"Another" #f test-header/bytes)
   => #"From: abc\r\nTo: field is\r\n continued\r\n\r\n"

   (remove-field "To" test-header)
   => "From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"
   (remove-field #"To" test-header/bytes)
   => #"From: abc\r\nAnother: zoo\r\n continued\r\n\r\n"

   (extract-all-fields test-header)
   => `(("From" . "abc") ("To" . "field is\r\n continued") ("Another" . "zoo\r\n continued"))
   (extract-all-fields test-header/bytes)
   => `((#"From" . #"abc") (#"To" . #"field is\r\n continued") (#"Another" . #"zoo\r\n continued"))

   (append-headers test-header "Athird: data\r\n\r\n")
   => "From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\nAthird: data\r\n\r\n"
   (append-headers test-header/bytes #"Athird: data\r\n\r\n")
   => #"From: abc\r\nTo: field is\r\n continued\r\nAnother: zoo\r\n continued\r\nAthird: data\r\n\r\n"

   )

  (let ([test (lambda (expect f . args)
                (unless (equal? expect (apply f args))
                  (error "failed")))])
    (for-each 
     (lambda (addr)
       (test '("o.gu@racket-lang.com") extract-addresses (car addr) 'address)
       (test '("o.gu@racket-lang.com" "o.gu@racket-lang.com") extract-addresses 
             (format "~a, ~a" (car addr) (car addr))
             'address)
       (test (list (cdr addr)) extract-addresses (car addr) 'name)
       (for-each 
        (lambda (addr2)
          (let ([two (format " ~a, \n\t~a" (car addr) (car addr2))])
            (test '("o.gu@racket-lang.com" "s.gu@racket-lang.org") extract-addresses two 'address)
            (test (list (cdr addr) (cdr addr2)) extract-addresses two 'name)))
        '(("s.gu@racket-lang.org" . "s.gu@racket-lang.org")
          ("<s.gu@racket-lang.org>" . "s.gu@racket-lang.org")
          ("s.gu@racket-lang.org (Gu, Sophia)" . "Gu, Sophia")
          ("s.gu@racket-lang.org (Sophia Gu)" . "Sophia Gu")
          ("s.gu@racket-lang.org (Sophia \"Sophie\" Gu)" . "Sophia \"Sophie\" Gu")
          ("Sophia Gu <s.gu@racket-lang.org>" . "Sophia Gu")
          ("\"Gu, Sophia\" <s.gu@racket-lang.org>" . "\"Gu, Sophia\"")
          ("\"Gu, Sophia (Sophie)\" <s.gu@racket-lang.org>" . "\"Gu, Sophia (Sophie)\""))))
     '(("o.gu@racket-lang.com" . "o.gu@racket-lang.com")))
    (printf "more tests passed\n")))

(module+ test (require (submod ".." main))) ; for raco test & drdr
