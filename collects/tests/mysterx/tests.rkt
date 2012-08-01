#lang racket/base
(require mysterx
         racket/system
         setup/dirs
         (only-in ffi/unsafe/com com-object-get-iunknown))

(define-syntax-rule (test expect expr)
  (let ([val expr]
        [ex expect])
    (printf "~s\n" 'expr)
    (unless (equal? ex val)
      (error 'test "~s failed: ~e" 'expr val))
    (set! count (add1 count))))

(define count 0)

(when (eq? 'windows (system-type))
  (system* (build-path (find-console-bin-dir) "MzCom.exe")
           "/RegServer")
  (define mzcom-progid (string-append "MzCOM.MzObj." (version)))

  (define mzcom (cocreate-instance-from-progid mzcom-progid))
  (test #t (com-object? mzcom))
  (test #t (com-is-a? mzcom (com-object-type mzcom)))
  (test (void) (set-coclass-from-progid! mzcom mzcom-progid))
  (test #t (com-object-eq? mzcom mzcom))
  (test '("About" "Eval" "Reset") (com-methods mzcom))
  (test '("About" "Eval" "Reset") (com-methods (com-object-type mzcom)))
  (test '(-> void) (com-method-type mzcom "About"))
  (test '(-> void) (com-method-type (com-object-type mzcom) "About"))
  (test '(-> void) (com-method-type mzcom "Reset"))
  (test '(string -> string) (com-method-type mzcom "Eval"))
  (test "3" (com-invoke mzcom "Eval" "(+ 1 2)"))

  (test '() (com-get-properties mzcom))
  (test '() (com-get-properties (com-object-type mzcom)))
  (test '() (com-set-properties mzcom))
  (test '() (com-set-properties (com-object-type mzcom)))

  (test '("SchemeError") (com-events mzcom))
  (test '("SchemeError") (com-events (com-object-type mzcom)))
  (test #f (com-event-type mzcom "SchemeError"))
  (test #f (com-event-type (com-object-type mzcom) "SchemeError"))
  (define recved #f)
  (test (void) (com-register-event-handler mzcom "SchemeError" 
                                           (lambda (msg) (set! recved msg))))
  (test #t (with-handlers ([exn:fail? (lambda (exn) 
                                        (regexp-match? #rx"COM object exception"
                                                       (exn-message exn)))])
             (com-invoke mzcom "Eval" "bad")))
  (sync (system-idle-evt))
  (test #t (regexp-match? #rx"bad" recved))
  (test (void) (com-unregister-event-handler mzcom "SchemeError"))

  (test #f (com-iunknown? mzcom))
  (test #t (com-iunknown? (com-object-get-iunknown mzcom)))

  (test com-omit com-omit)

  (define ie (cocreate-instance-from-progid "InternetExplorer.Application.1"))
  (test #t (and (member "Visible" (com-set-properties ie)) #t))
  (test #f (com-get-property ie "Visible"))
  (test (void) (com-set-property! ie "Visible" #t))
  (test #t (com-get-property ie "Visible"))
  (test (void) (com-set-property! ie "Visible" #f))
  (test #f (com-get-property ie "Container"))

  ;; For IE 7 (or 8?), this needs to be a web page; opening
  ;; a local document disconnects the object for some reason:
  (test (void) (com-invoke ie "Navigate" "http://racket-lang.org"))
  (sleep 3) ; give the document time to load

  (define doc (com-get-property ie "Document"))
  (test #t (com-object? doc))
  (test "The Racket Language" (com-get-property ie "Document" "title"))
  (test (void) (com-set-property! ie "Document" "title" "The Racket Documentation"))
  (test "The Racket Documentation" (com-get-property ie "Document" "title"))
  (test '(-> string) (com-get-property-type doc "title"))
  (test '(string ->  void) (com-set-property-type doc "title"))

  (void))

(define (test-currency n)
  (= (/ (round (* (inexact->exact n) 10000)) 10000)
     (com-currency->number (number->com-currency n))))

(define (test-scode n)
  (= n (com-scode->number (number->com-scode n))))

(define (test-date date)
  (equal? date (com-date->date (date->com-date date))))

(for-each
 (lambda (n)
   (unless (test-scode n)
     (eprintf "Error in test-scode for value ~a\n" n)))
 '(25 -22 -1 -233344433 177000000 859489222))


(print-struct #t)

(let ([date (seconds->date (current-seconds))])
  (unless (test-date date)
    (eprintf "Error in test-date\n")))

(for-each
 (lambda (n)
   (unless (test-currency n)
     (eprintf "Error in test-currency for value ~a\n" n)))
 '(0 1 3.14 25.00 -22.34 11.7832 91000000000 25034343434.9933))


(printf "~a passed\n" count)
