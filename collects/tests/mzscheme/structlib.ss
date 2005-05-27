
(load-relative "loadtest.ss")

(SECTION 'structlib)

(require (lib "struct.ss"))

(let ([now (seconds->date (current-seconds))])
  (test #t equal? now (copy-struct date now))
  (test #f equal? now (copy-struct date now (date-second -1)))
  (test -1 date-second (copy-struct date now (date-second -1)))
  (test (date-year now) date-year (copy-struct date now (date-second -1))))

(err/rt-test (copy-struct date 10))
(err/rt-test (copy-struct date 10 (date-second 0)))

(syntax-test #'copy-struct)
(syntax-test #'(copy-struct))
(syntax-test #'(copy-struct date))
(syntax-test #'(copy-struct date 10 foo))
(syntax-test #'(copy-struct date 10 . foo))
(syntax-test #'(copy-struct date 10 (foo)))
(syntax-test #'(copy-struct date 10 (foo . bar)))

(syntax-test #'(copy-struct x 10))
(syntax-test #'(copy-struct date 10 (date-foo 12)))
(syntax-test #'(copy-struct date 10 (date-second 12) (date-yeeer 10)))
(syntax-test #'(copy-struct date 10 (date-second 12) (date-second 10)))

(require (rename mzscheme mz:date-second date-second))
(syntax-test #'(copy-struct date 10 (date-second 12) (mz:date-second 10)))

(let ([v (let ()
	   (define-struct a (b c) (make-inspector))
	   ;; This `copy-struct' is expanded in an internal-defn context
	   (copy-struct a (make-a 1 2) (a-c 13)))])
  (test #(struct:a 1 13) struct->vector v))

(report-errs)
