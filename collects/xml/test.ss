;; run these tests with:
;;  % mzscheme --require test.ss

(module test mzscheme
  (require xml/main
           scheme/list
           scheme/port)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; utils
  ;;
  
  ;; test-bad-read-input : format-str str -> void
  ;; First argument is the input, second is the error message
  (define (test-bad-read-input format-str err-string)
    (let ([str (format format-str)])
      (with-handlers ([exn:xml?
                       (lambda (x)
                         (unless (equal? (exn-message x) err-string)
                           (report-err format-str (exn-message x) err-string)))])
        (read-xml (open-input-string str))
        (report-err str "no error" err-string))))
  
  ;; tests-failed : number
  ;; incremened for each test that fails
  (define tests-failed 0)
  
  ;; report-err : string string string -> void
  ;; reports an error in the test suite
  ;; increments tests-failed.
  (define (report-err test got expected)
    (set! tests-failed (+ tests-failed 1))
    (printf "FAILED     test: ~a~n            got: ~a~n       expected: ~a~n"
            test got expected))
  
  ;; done : -> void
  ;; prints out a message saying the tests are done.
  ;; if any tests failed, prints a message saying how many
  (define (done)
    (if (= tests-failed 0)
        (printf "All tests passed~n")
        (printf "~a tests failed~n" tests-failed)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; reader error tests
  ;;
  
  (test-bad-read-input "<" "read-xml: lex-error: at position 1.1/2: unexpected eof")
  (test-bad-read-input "<a>" "read-xml: parse-error: unclosed `a' tag at [1.0/1 1.3/4]")
  (test-bad-read-input
   "<a></b>"
   "read-xml: parse-error: start tag `a' at [1.0/1 1.3/4] doesn't match end tag `b' at [1.3/4 1.7/8]")
  (test-bad-read-input
   "<a <a>" "read-xml: lex-error: at position 1.4/5: expected / or > to close tag `a'")
  
  (test-bad-read-input "~n<" "read-xml: lex-error: at position 2.1/3: unexpected eof")
  (test-bad-read-input "~n<a>" "read-xml: parse-error: unclosed `a' tag at [2.0/2 2.3/5]")
  (test-bad-read-input
   "~n<a></b>"
   "read-xml: parse-error: start tag `a' at [2.0/2 2.3/5] doesn't match end tag `b' at [2.3/5 2.7/9]")
  (test-bad-read-input
   "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'")
  
  ;; permissive?
  (with-handlers ([exn?
                   (lambda (exn)
                     (regexp-match #rx"Expected content," (exn-message exn)))])
    (report-err "Non-permissive" (xml->xexpr #f) "Exception"))
  
  (with-handlers ([exn?
                   (lambda (exn)
                     (report-err "Permissive" "Exception" "#f"))])
    (parameterize ([permissive? #t])
      (let ([tmp (xml->xexpr #f)])
        (when tmp
          (report-err "Permissive" tmp "#f")))))
  
  ;; doctype
  (let ()
    (define source-string #<<END
<!DOCTYPE html PUBLIC
 "-//W3C//DTD XHTML 1.0 Transitional//EN"
 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"> </html>
END
      )
    
    (define source-document
      (read-xml (open-input-string source-string)))
    (define result-string
      (with-output-to-string (lambda () (write-xml source-document))))
    (define expected-string #<<END
<html xmlns="http://www.w3.org/1999/xhtml"> </html>
END
      )
    (unless (string=? expected-string result-string)
      (report-err "DOCTYPE dropping"
                  result-string
                  expected-string)))
  
  
  ;; pis
  (define a-pi (make-p-i #f #f "foo" "bar"))
  (define a-p (make-prolog empty #f))
  (define a-p/pi (make-prolog (list a-pi) #f))
  (define a-d0
    (make-document a-p (make-element #f #f 'html empty empty)
                   empty))
  (define a-d1
    (make-document a-p (make-element #f #f 'html empty empty)
                   (list a-pi)))
  (define a-d2
    (make-document a-p/pi (make-element #f #f 'html empty empty)
                   (list a-pi)))
  
  (define (test-string=? test result expected)
    (unless (string=? result expected)
      (report-err test result expected)))
  
  (test-string=? "Display XML w/o pis"
                 (with-output-to-string (lambda () (display-xml a-d0)))
                 "\n<html />")
  (test-string=? "Display XML w/ pi in doc-misc"
                 (with-output-to-string (lambda () (display-xml a-d1)))
                 "\n<html /><?foo bar?>\n")
  (test-string=? "Display XML w/ pi in doc-misc and prolog"
                 (with-output-to-string (lambda () (display-xml a-d2)))
                 "<?foo bar?>\n\n<html /><?foo bar?>\n")
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; done
  ;;
  (done))
