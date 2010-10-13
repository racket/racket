#lang racket

(require "drracket-test-util.rkt"
         tests/utils/gui
         mred
         framework
         (prefix-in fw: framework))

(define language (make-parameter "<<not a language>>"))

;; set-language : boolean -> void
(define (set-language close-dialog?)
  (set-language-level! (language) close-dialog?))

(define (common)
    (test-expression "(check-expect 1 1)"
		     "The test passed!"
		     #:repl-expected "Both tests passed!")

    (test-expression "(check-within 1 1.1 0.5)"
		     "The test passed!"
		     #:repl-expected "Both tests passed!")

    (test-expression "(check-expect 1 2)"
		     ""
		     #:check-failures-expected
		     (list (make-check-expect-failure "1" "2" 1 0))))

(define (common-*sl)
  (test-expression "(: foo Integer) (define foo 5)"
		   ""
		   #:repl-expected "define: cannot redefine name: foo")
  (test-expression "(: foo Integer) (define foo \"bar\")"
		   ""
		   #:repl-expected "define: cannot redefine name: foo"
		   #:signature-violations-expected
		   (list (make-signature-violation "\"bar\"" 1 7))))

(define (common-DMdA)
  (test-expression "(: foo integer) (define foo 5)"
		   ""
		   #:repl-expected "define: cannot redefine name: foo")
  (test-expression "(: foo integer) (define foo \"bar\")"
		   ""
		   #:repl-expected "define: cannot redefine name: foo"
		   #:signature-violations-expected
		   (list (make-signature-violation "\"bar\"" 1 7))))


;                                                             
;  ;;;                    ;;                                  
;   ;;                    ;;                                  
;   ;;                                                        
;   ;;;;;   ;;;;   ;;;;;;;;;  ;;; ;;   ;;; ;;    ;;;;  ;;; ;; 
;   ;;  ;; ;;  ;; ;;  ;;  ;;   ;;; ;;   ;;; ;;  ;;  ;;  ;;;;; 
;   ;;  ;; ;;  ;; ;;  ;;  ;;   ;;  ;;   ;;  ;;  ;;  ;;  ;;    
;   ;;  ;; ;;;;;;  ;;;;   ;;   ;;  ;;   ;;  ;;  ;;;;;;  ;;    
;   ;;  ;; ;;      ;      ;;   ;;  ;;   ;;  ;;  ;;      ;;    
;   ;;  ;; ;;   ;  ;;;;;  ;;   ;;  ;;   ;;  ;;  ;;   ;  ;;    
;   ;;;;;   ;;;;   ;;;;;;;;;; ;;;; ;;; ;;;; ;;;  ;;;;  ;;;;   
;                 ;;   ;;                                     
;                 ;;   ;;                                     
;                  ;;;;;                                      

(define (beginner)
  (parameterize ([language (list "How to Design Programs" #rx"Beginning Student(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-*sl)))


;                                                                            
;  ;;;                                 ;;;     ;;;                           
;   ;;                       ;          ;;      ;;                           
;   ;;                       ;          ;;      ;;                           
;   ;;;;;   ;;;;   ;;;;;;   ;   ;;;;    ;;;;;   ;;;;;  ;;; ;;  ;;;;  ;;; ;;; 
;   ;;  ;; ;;  ;; ;;  ;;    ;      ;;   ;;  ;;  ;;  ;;  ;;;;; ;;  ;;  ;;  ;  
;   ;;  ;; ;;  ;; ;;  ;;   ;       ;;   ;;  ;;  ;;  ;;  ;;    ;;  ;;  ;;  ;  
;   ;;  ;; ;;;;;;  ;;;;    ;    ;;;;;   ;;  ;;  ;;  ;;  ;;    ;;;;;;   ;;;   
;   ;;  ;; ;;      ;       ;   ;;  ;;   ;;  ;;  ;;  ;;  ;;    ;;       ;;;   
;   ;;  ;; ;;   ;  ;;;;;  ;    ;;  ;;   ;;  ;;  ;;  ;;  ;;    ;;   ;   ;;;   
;   ;;;;;   ;;;;   ;;;;;; ;     ;;;;;;  ;;;;;   ;;;;;  ;;;;    ;;;;     ;    
;                 ;;   ;;;                                                   
;                 ;;   ;;;                                                   
;                  ;;;;;                                                     


(define (beginner/abbrev)
  (parameterize ([language (list "How to Design Programs" 
                                 #rx"Beginning Student with List Abbreviations(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-*sl)))


;                                                                                          
;   ;;                                                      ;;;   ;;                       
;   ;;            ;;                                         ;;   ;;           ;;          
;                 ;;                                         ;;                ;;          
;  ;;;  ;;; ;;   ;;;;;  ;;;;  ;;; ;; ;;; ;;  ;;    ;;;;   ;;;;;  ;;;   ;;;;   ;;;;;  ;;;;  
;   ;;   ;;; ;;   ;;   ;;  ;;  ;;;;;  ;;; ;;; ;;  ;;  ;; ;;  ;;   ;;      ;;   ;;   ;;  ;; 
;   ;;   ;;  ;;   ;;   ;;  ;;  ;;     ;;  ;;  ;;  ;;  ;; ;;  ;;   ;;      ;;   ;;   ;;  ;; 
;   ;;   ;;  ;;   ;;   ;;;;;;  ;;     ;;  ;;  ;;  ;;;;;; ;;  ;;   ;;   ;;;;;   ;;   ;;;;;; 
;   ;;   ;;  ;;   ;;   ;;      ;;     ;;  ;;  ;;  ;;     ;;  ;;   ;;  ;;  ;;   ;;   ;;     
;   ;;   ;;  ;;   ;;   ;;   ;  ;;     ;;  ;;  ;;  ;;   ; ;;  ;;   ;;  ;;  ;;   ;;   ;;   ; 
;  ;;;; ;;;; ;;;   ;;;  ;;;;  ;;;;   ;;;; ;;; ;;;  ;;;;   ;;;;;; ;;;;  ;;;;;;   ;;;  ;;;;  
;                                                                                          
;                                                                                          
;                                                                                          


(define (intermediate)
  (parameterize ([language (list "How to Design Programs" #rx"Intermediate Student(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-*sl)))

;                                                                                    
;                                                                                    
;                                                                                    
;    ;;               ;   ;;;;;;                        ;;;;           ;;;;          
;    ;;              ;;   ;;;;;;                        ;;;;           ;;;;          
;       ;;;; ;;;   ;;;;;  ;;;;;; ;;;;;;;  ;;;;;;; ;;;;  ;;;;;;;     ;;;;;;; ;;;;;;;  
;  ;;;; ;;;;;;;;; ;;;;;;  ;;;;;; ;;;;;;;; ;;;;;;;;;;;;; ;;;;;;;;   ;;;;;;;; ;;;;;;;; 
;  ;;;; ;;;; ;;;;  ;;;;   ;;;;;;     ;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;;;;;;;     ;;;; 
;  ;;;; ;;;; ;;;;  ;;;;  ;; ;;;;  ;;;;;;; ;;;; ;;; ;;;; ;;;; ;;;; ;;;; ;;;;  ;;;;;;; 
;  ;;;; ;;;; ;;;;  ;;;;; ;; ;;;; ;;  ;;;; ;;;; ;;; ;;;; ;;;;;;;;; ;;;;;;;;; ;;  ;;;; 
;  ;;;; ;;;; ;;;;  ;;;;; ;; ;;;; ;;;;;;;; ;;;; ;;; ;;;; ;;;;;;;;   ;;;;;;;; ;;;;;;;; 
;  ;;;; ;;;; ;;;;   ;;;; ;; ;;;;  ;; ;;;; ;;;; ;;; ;;;; ;;;;;;;     ;;;;;;;  ;; ;;;; 
;                        ;;                                                          
;                                                                                    
;                                                                                    


(define (intermediate/lambda)
  (parameterize ([language (list "How to Design Programs" 
                                 #rx"Intermediate Student with lambda(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-*sl)))


;                                                                           
;                                                                           
;                                                                           
;                ;;;;                                                  ;;;; 
;                ;;;;                                                  ;;;; 
;  ;;;;;;;    ;;;;;;; ;;;  ;;; ;;;;;;;  ;;;; ;;;    ;;;;;   ;;;     ;;;;;;; 
;  ;;;;;;;;  ;;;;;;;; ;;;  ;;; ;;;;;;;; ;;;;;;;;;  ;;;;;;  ;;;;;   ;;;;;;;; 
;      ;;;; ;;;;;;;;;  ;;;;;;      ;;;; ;;;; ;;;; ;;;;;;; ;;;; ;; ;;;;;;;;; 
;   ;;;;;;; ;;;; ;;;;  ;;;;;;   ;;;;;;; ;;;; ;;;; ;;;;    ;;;;;;; ;;;; ;;;; 
;  ;;  ;;;; ;;;;;;;;;  ;;;;;;  ;;  ;;;; ;;;; ;;;; ;;;;;;; ;;;;;   ;;;;;;;;; 
;  ;;;;;;;;  ;;;;;;;;   ;;;;   ;;;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;;  ;;;;;;;; 
;   ;; ;;;;   ;;;;;;;   ;;;;    ;; ;;;; ;;;; ;;;;   ;;;;;   ;;;;    ;;;;;;; 
;                                                                           
;                                                                           
;                                                                           


(define (advanced)
  (parameterize ([language (list "How to Design Programs" #rx"Advanced Student(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-*sl)))


(define (DMdA-beginner)
  (parameterize ([language (list "DeinProgramm" #rx"Die Macht der Abstraktion - Anfänger(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-DMdA)))

(define (DMdA-vanilla)
  (parameterize ([language (list "DeinProgramm" #rx"Die Macht der Abstraktion(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-DMdA)))

(define (DMdA-assignments)
  (parameterize ([language (list "DeinProgramm" #rx"Die Macht der Abstraktion mit Zuweisungen(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-DMdA)))

(define (DMdA-advanced)
  (parameterize ([language (list "DeinProgramm" #rx"Die Macht der Abstraktion - fortgeschritten(;|$)")])
    (prepare-for-test-expression)
    (common)
    (common-DMdA)))

(define (prepare-for-test-expression)
  (let ([drs (wait-for-drscheme-frame)])
    (clear-definitions drs)
    (set-language #t)
    (sleep 1) ;; this shouldn't be neccessary....
    (do-execute drs)))

;; test-setting : (-> void) string string string -> void
;; opens the language dialog, runs `set-setting'
;; closes the language dialog, executes,
;; makes sure that `expression' produces
;; `result'. `set-setting' is expected to click around
;; in the language dialog.
;; `setting-name' is used in the error message when the test fails.
(define (test-setting set-setting setting-name expression result)
  (set-language #f)
  (set-setting)
  (let ([f (get-top-level-focus-window)])
    (fw:test:button-push "OK")
    (wait-for-new-frame f))
  (let* ([drs (get-top-level-focus-window)]
         [interactions (send drs get-interactions-text)])
    (clear-definitions drs)
    (type-in-definitions drs expression)
    (do-execute drs)
    (let* ([got (fetch-output/should-be-tested drs)])
      (unless (string=? result got)
        (fprintf (current-error-port)
                 "FAILED: ~s ~s ~s test\n expected: ~s\n      got: ~s\n"
                 (language) setting-name expression result got)))))

(define (fetch-output/should-be-tested . args)
  (regexp-replace (regexp
                   (string-append
                    (regexp-quote "")
                    "$"))
                  (apply fetch-output args)
                  ""))

(define re:out-of-sync
  (regexp
   "WARNING: Interactions window is out of sync with the definitions window\\."))

(define (parse-number txt)
  (cond
    ((string=? txt "No") 0)
    ((string=? txt "One") 1)
    ((string=? txt "Two") 2)
    (else (string->number txt))))

(define (parse-test-failure-header txt)
  (cond
    ((regexp-match #rx"^Ran ([NoOneTwo0-9]+) tests?.\n([NoOneTwo0-9]+) tests? passed.\n([NoOneTwo0-9]+) signature violations?."
                          txt)
     => (lambda (match)
          (let-values (((_ test-count-text test-passed-count-text signature-violations-count-text)
                        (apply values match)))
            (values (parse-number test-count-text)
                    (parse-number test-passed-count-text)
                    (parse-number signature-violations-count-text)))))
    ((regexp-match #rx"^This program must be tested!\n([NoOneTwo0-9]+) signature violations?." txt)
     => (lambda (match)
          (values 0 0 (parse-number (cadr match)))))
    (else
     (error 'parse-test-failure-header "bad test failure header" txt))))

(define (parse-test-failures txt)
  (let-values (((test-count test-passed-count signature-violations-count)
                (parse-test-failure-header txt)))
    (let ((check-failures
           (cond
             ((regexp-match #rx"Check failures:\n(.*)" txt)
              => (lambda (res)
                   (parse-check-failures (cadr res))))
             (else '())))
          (signature-violations
           (cond
             ((regexp-match #rx"Signature violations:\n(.*)" txt)
              => (lambda (res)
                   (parse-signature-violations (cadr res))))
             (else '()))))
      (values test-count test-passed-count signature-violations-count
              check-failures
              signature-violations))))

(define-struct check-expect-failure
  (actual expected line column)
  #:transparent)

(define (parse-check-failures txt)
  (cond
    ((string=? txt "") '())
    ((regexp-match #rx"^Signature violations:" txt)
     '())
    ((regexp-match #rx"^[ \t]*\n(.*)" txt)
     => (lambda (match)
          (parse-check-failures (cadr match))))
    ((regexp-match "^[ \t]+Actual value ([^\n]+) differs from ([^\n]+), the expected value.\nat line ([0-9]+), column ([0-9]+)(.*)"
                   txt)
     => (lambda (match)
          (let-values (((_ actual expected line-text col-text rest) (apply values match)))
            (cons
             (make-check-expect-failure actual expected
                                        (string->number line-text)
                                        (string->number col-text))
             (parse-check-failures rest)))))
    (else 
     (error "unknown check failure" txt (string-ref txt 0)))))

(define-struct signature-violation
  (got line column)
  #:transparent)

(define (parse-signature-violations txt)
  (cond
    ((string=? txt "") '())
    ((regexp-match #rx"^[ \t]*\n(.*)" txt)
     => (lambda (match)
          (parse-signature-violations (cadr match))))
    ((regexp-match "got ([^\n]+), signature at line ([0-9]+), column ([0-9]+)(.*)"
                   txt)
     => (lambda (match)
          (let-values (((_ got line-text col-text rest) (apply values match)))
            (cons
             (make-signature-violation got
                                       (string->number line-text)
                                       (string->number col-text))
             (parse-signature-violations rest)))))
    (else '())))
  

;; test-expression : (union string 'xml 'image (listof (union string 'xml 'image)))
;;                   (union string regexp (string -> boolean)) 
;;                -> void
;; types an expression in the definitions window, executes it and tests the output
;; types an expression in the REPL and tests the output from the REPL.
(define (test-expression expression defs-expected 
			 #:repl-expected (repl-expected defs-expected)
			 #:check-failures-expected (check-failures-expected '())
			 #:signature-violations-expected (signature-violations-expected '()))
  (let* ([drs (wait-for-drscheme-frame)]
         [interactions-text (send drs get-interactions-text)]
         [definitions-text (send drs get-definitions-text)]
         [handle-insertion
          (lambda (item)
            (cond
              [(eq? item 'image)
               (use-get/put-dialog 
                (lambda () (fw:test:menu-select "Insert" "Insert Image..."))
                (simplify-path (build-path (collection-path "icons") "recycle.png")))]
              [(string? item)
               (type-in-definitions drs item)]
              [(eq? item 'xml)
               (fw:test:menu-select "Insert" "Insert XML Box")
               (for-each fw:test:keystroke (string->list "<a><b>"))]
              [else (error 'handle-insertion "unknown thing to insert ~s" item)]))]
         [check-expectation
          (lambda (expected got)
            (cond
              [(string? expected)
               (whitespace-string=? expected got)]
              [(regexp? expected)
               (regexp-match expected got)]
              [(procedure? expected)
               (expected got)]))]
         [make-err-msg
          (lambda (expected)
            (cond
              [(string? expected)
               "FAILED: ~s ~s expected ~s to produce:\n  ~s\ngot:\n  ~s\ninstead\n"]
              [(regexp? expected)
               "FAILED: ~s ~s expected ~s to match ~s, got ~s instead\n"]
              [(procedure? expected)
               "FAILED: ~s ~s expected ~s to pass predicate ~s, got ~s\n"]))])
    (clear-definitions drs)
    (cond
      [(pair? expression) (for-each handle-insertion expression)]
      [else (handle-insertion expression)])
    (do-execute drs)

    (let ([got
           (fetch-output
            drs
            (send interactions-text paragraph-start-position 2)
            (send interactions-text paragraph-end-position
                  (- (send interactions-text last-paragraph) 1)))])
      (when (regexp-match re:out-of-sync got)
        (error 'test-expression "got out of sync message"))
      (unless (check-expectation defs-expected got)
        (printf (make-err-msg defs-expected) 
                'definitions (language) expression defs-expected got)))

    (let ((text
	   (cond
	    ((send (send definitions-text get-tab) get-test-editor)
	     => (lambda (test-editor)
		  (let ((text (send test-editor get-text 0 'eof #t)))
		    (if (string=? text "")
			#f
			text))))
	    (else #f))))
	   
      (cond
       ((and (null? signature-violations-expected)
	     (null? check-failures-expected))
	(when text
	  (printf "FAILED: ~s ~s expected ~s to produce no check failures or signature violations:\ngot:\n~a\ninstead\n"
		  'definitions (language) expression text)))
       (text
	(let-values (((test-count test-passed-count signature-violation-count check-failures signature-violations)
		      (parse-test-failures text)))
	  (when (not (equal? check-failures check-failures-expected))
	    (printf "FAILED: ~s ~s expected ~s to produce check failures:\n~s\ngot:\n~s\ninstead\n"
		    'definitions (language) expression check-failures-expected check-failures))
	  (when (not (equal? signature-violations signature-violations-expected))
	    (printf "FAILED: ~s ~s expected ~s to produce signature violations:\n~s\ngot:\n~s\ninstead\n"
		    'definitions (language) expression signature-violations-expected signature-violations))))
       (else
	(printf "expected ~a check failures and ~a signature violations but got none"
		(length check-failures-expected) (length signature-violations-expected)))))
    ; #### do same for REPL
    
    (let ([s (make-semaphore 0)])
      (queue-callback
       (λ ()
         (send definitions-text select-all)
         (send definitions-text copy)
         (send interactions-text set-position
               (send interactions-text last-position)
               (send interactions-text last-position))
         (send interactions-text paste)
         (semaphore-post s)))
      (semaphore-wait s))
    
    (let ([last-para (send interactions-text last-paragraph)])
      (alt-return-in-interactions drs)
      (wait-for-computation drs)
      (let ([got
             (fetch-output
              drs
              (send interactions-text paragraph-start-position (+ last-para 1))
              (send interactions-text paragraph-end-position
                    (- (send interactions-text last-paragraph) 1)))])
        (when (regexp-match re:out-of-sync got)
          (error 'test-expression "got out of sync message"))
        (unless (check-expectation repl-expected got)
          (printf (make-err-msg repl-expected) 'interactions (language) expression repl-expected got))))))



(define-syntax (go stx)
  (syntax-case stx ()
    [(_ arg)
     (identifier? (syntax arg))
     (syntax (begin (printf ">> starting ~a\n" (syntax->datum #'arg))
                    (arg)
                    (printf ">> finished ~a\n" (syntax->datum #'arg))))]))

(define (run-test)
  (put-preferences '(test:test-window:docked?) '(#t))
  (go beginner)
  (go beginner/abbrev)
  (go intermediate)
  (go intermediate/lambda)
  (go advanced)
  (go DMdA-beginner)
  (go DMdA-vanilla)
  (go DMdA-assignments)
  (go DMdA-advanced))

(fire-up-drscheme-and-run-tests run-test)
