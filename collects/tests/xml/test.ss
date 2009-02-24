#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         xml)

;; test-bad-read-input : format-str str -> void
;; First argument is the input, second is the error message
(define (test-bad-read-input format-str err-string)
  (define str (format format-str))
  (test-exn
   str
   (lambda (x)
     (and (exn:xml? x)
          (equal? (exn-message x) err-string)))
   (lambda ()
     (read-xml (open-input-string str)))))

(define xml-tests
  (test-suite
   "XML"
   
   (test-suite
    "read-xml"
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
     "~n<a <a>" "read-xml: lex-error: at position 2.4/6: expected / or > to close tag `a'"))
   
   (test-suite
    "xml->xexpr"
    (test-exn
     "Non-permissive"
     (lambda (exn)
       (and (exn? exn)
            (regexp-match #rx"Expected content," (exn-message exn))))
     (lambda ()
       (xml->xexpr #f)))
    
    (test-false
     "Permissive"
     (parameterize ([permissive? #t])
       (xml->xexpr #f))))
   
   (test-suite 
    "DOCTYPE"
    
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
      (test-equal?
       "DOCTYPE dropping" result-string expected-string)))
   
   (let ()
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
     (test-suite 
      "PIs"
      (test-equal? "Display XML w/o pis"
                   (with-output-to-string (lambda () (display-xml a-d0)))
                   "\n<html />")
      (test-equal? "Display XML w/ pi in doc-misc"
                   (with-output-to-string (lambda () (display-xml a-d1)))
                   "\n<html /><?foo bar?>\n")
      (test-equal? "Display XML w/ pi in doc-misc and prolog"
                   (with-output-to-string (lambda () (display-xml a-d2)))
                   "<?foo bar?>\n\n<html /><?foo bar?>\n")))))

(run-tests xml-tests)