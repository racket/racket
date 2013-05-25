#lang racket/base
(require ffi/com
         (only-in ffi/unsafe/com make-com-object)
         racket/system
         setup/dirs)

(define-syntax test
  (syntax-rules ()
    [(_ expect expr)
     (test expect #:alts '() expr)]
    [(_ expect #:alts alts-expr expr)
     (let ([val expr]
	   [ex expect]
	   [alts alts-expr])
       (printf "~s\n" 'expr)
       (unless (or (equal? ex val)
		   (member val alts))
         (error 'test "~s failed: ~e" 'expr val))
       (set! count (add1 count)))]))

(define count 0)

(when (eq? 'windows (system-type))
  (system* (build-path (find-console-bin-dir) "MzCom.exe")
           "/RegServer")
  (define mzcom-progid (string-append "MzCOM.MzObj." (version)))

  (define a-guid-str "{abcdef00-1234-4321-9876-1234567890ab}")
  (define another-guid-str "{0bcdef00-1234-4321-9876-1234567890ab}")
  (define a-guid (string->guid a-guid-str))
  (test #t (guid? a-guid))
  (test #t (iid? a-guid))
  (test #t (clsid? a-guid))
  (test #t (guid=? a-guid (string->iid a-guid-str)))
  (test #t (guid=? a-guid (string->clsid a-guid-str)))
  (test #f (guid=? a-guid (string->iid another-guid-str)))

  (test #t (guid=? (string->clsid "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}")
                   (progid->clsid mzcom-progid)))
  (test mzcom-progid (clsid->progid (string->clsid "{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}")))

  (define mzcom (com-create-instance mzcom-progid))
  (test #t (com-object? mzcom))
  (test #t (com-type? (com-object-type mzcom)))
  (test #t (com-type=? (com-object-type mzcom)
                       (com-object-type mzcom)))
  (test #t (guid=? (progid->clsid mzcom-progid) (com-object-clsid mzcom)))
  (test (void) (com-object-set-clsid! mzcom (progid->clsid mzcom-progid)))
  (test #t (com-object-eq? mzcom mzcom))
  (let ([mzcom2 (make-com-object (com-object-get-iunknown mzcom) #f)])
    (test #t (com-object-eq? mzcom mzcom2))
    (test #t (equal? mzcom mzcom2))
    (test (equal-hash-code mzcom) (equal-hash-code mzcom2))
    (test (equal-secondary-hash-code mzcom) (equal-secondary-hash-code mzcom2)))
  (test '("About" "Eval" "Reset") (com-methods mzcom))
  (test '("About" "Eval" "Reset") (com-methods (com-object-type mzcom)))
  (test '(-> () void) (com-method-type mzcom "About"))
  (test '(-> () void) (com-method-type (com-object-type mzcom) "About"))
  (test '(-> () void) (com-method-type mzcom "Reset"))
  (test '(-> (string) string) (com-method-type mzcom "Eval"))
  (test "3" (com-invoke mzcom "Eval" "(+ 1 2)"))
  (test "4" (com-invoke mzcom "Eval" (type-describe "(+ 2 2)" 'string)))
  (test "5" (com-invoke mzcom "Eval" (type-describe "(+ 3 2)" 'int))) ; description is not used

  (test '() (com-get-properties mzcom))
  (test '() (com-get-properties (com-object-type mzcom)))
  (test '() (com-set-properties mzcom))
  (test '() (com-set-properties (com-object-type mzcom)))

  (test '("SchemeError") (com-events mzcom))
  (test '("SchemeError") (com-events (com-object-type mzcom)))
  (test #f (com-event-type mzcom "SchemeError"))
  (test #f (com-event-type (com-object-type mzcom) "SchemeError"))
  (define recved #f)
  (define exec (com-make-event-executor))
  (test #t (com-event-executor? exec))
  (test (void) (com-register-event-callback mzcom "SchemeError" 
                                            (lambda (msg) (set! recved msg))
                                            exec))
  (test #f (sync/timeout 0 exec))
  (test #t (with-handlers ([exn:fail? (lambda (exn) 
                                        (regexp-match? #rx"COM object exception"
                                                       (exn-message exn)))])
             (com-invoke mzcom "Eval" "bad")))
  (test #f recved)
  (test (void) (com-unregister-event-callback mzcom "SchemeError"))
  (test (void) ((sync exec)))
  (test #t (regexp-match? #rx"bad" recved))

  (test #f (com-iunknown? mzcom))
  (test #t (com-iunknown? (com-object-get-iunknown mzcom)))
  (test #t (com-iunknown? (com-object-get-idispatch mzcom)))
  (test #f (com-idispatch? mzcom))
  (test #t (com-idispatch? (com-object-get-idispatch mzcom)))

  (test (void) (com-release mzcom))

  (define (with-fail-to-no thunk)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (and (regexp-match #rx"released" (exn-message exn))
                                      'no))])
      (thunk)))
  (test 'no (with-fail-to-no (lambda () (com-invoke mzcom "About"))))
  (test 'no (with-fail-to-no (lambda () (com-methods mzcom))))
  (test 'no (with-fail-to-no (lambda () (com-events mzcom))))

  (test com-omit com-omit)

  (let ([c (make-custodian)])
    (define mzcom2 
      (parameterize ([current-custodian c])
        (com-create-instance mzcom-progid)))
    (test '("About" "Eval" "Reset") (com-methods mzcom2))
    (custodian-shutdown-all c)
    (test 'no (with-handlers ([exn:fail? (lambda (exn)
                                           (and (regexp-match #rx"released" (exn-message exn))
                                                'no))])
                (com-invoke mzcom2 "About"))))

  (define ie (com-create-instance "InternetExplorer.Application.1"))
  (test #t (and (member "Visible" (com-get-properties ie)) #t))
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
  (test '(-> () string) #:alts '((-> () any)) (com-get-property-type doc "title"))
  (test '(-> (string) void) #:alts '((-> (any) void)) (com-set-property-type doc "title"))

  (test (void) (com-set-property! ie "Visible" #t))
  (test (void) (com-invoke ie "Quit"))

  (test (void) (com-release doc))
  (test (void) (com-release ie))

  (test #t (type-description? 'int))
  (test #t (type-description? 'long-long))
  (test #t (type-description? 'string))
  (test #t (type-description? 'any))
  (test #t (type-description? 'char))
  (test #f (type-description? 'long))
  (test #f (type-description? '(int)))
  (test #t (type-description? '(opt int)))
  (test #f (type-description? '(opt)))
  (test #f (type-description? '(opt int int)))
  (test #t (type-description? '(box int)))
  (test #f (type-description? '(box)))
  (test #f (type-description? '(box int int)))
  (test #f (type-description? '(-> (int) int)))
  (test #t (type-description? '(array 8 int)))
  (test #f (type-description? '(array 8)))
  (test #f (type-description? '(array)))
  (test #f (type-description? '(array int)))
  (test #t (type-description? '(array 8 (array 4 any))))

  (void))

;; The Excel interface provides many more opportunities for tests:
(define excel (with-handlers ([exn:fail? (lambda (exn)
					   (printf "Excel not available\n")
					   #f)])
			     (com-create-instance "Excel.Application")))
(when excel
  (com-set-property! excel "Visible" #t)
  (define wb (com-get-property excel "Workbooks"))
  (define workbook (com-invoke wb "Add"))
  (define sheets (com-get-property workbook "Worksheets"))
  (define sheet (com-get-property sheets '("Item" "Sheet1")))
  (define range (com-get-property sheet "Cells"))
  (define cell (com-get-property range '("Item" 1 1)))
  (com-get-property cell '("Value" 10))
  (com-set-property! cell '("Value" 10) (seconds->date (current-seconds)))
  (test #t (date? (com-get-property cell '("Value" 10)))))

(printf "~a passed\n" count)
