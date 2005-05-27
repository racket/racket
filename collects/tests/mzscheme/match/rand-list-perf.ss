(load-relative "../loadtest.ss")
(SECTION 'MATCH-PERFORMANCE)

(require (lib "pretty.ss"))
(require-for-syntax (lib "pretty.ss"))
(require (lib "include.ss"))
(require (lib "plt-match.ss"))
(require (lib "list.ss"))

(include "match-compile-perf.scm")

(match-performance-test
 (file
  ;; this is the name of the underlying file that handles the 
  ;; persistance of test data between invocations of this performance test
  ;; Each time a test is run one has the option to only compare current
  ;; data with previous data or one can modify the database file by appending
  ;; hopefullly improved data.
  ;; For most purposes it is advisable not to alter the database.
  (name "rand-list-hist.db")
  ;; onlly one of the following options is allowed
  ;; write-new   ;; uncomment to write an new test database 
                 ;; (this requires the whole-list display option below)
  ;; add-results ;; uncomment to append the results of the current test
                 ;; to the database
  )
 (tag
  ;; only one option is allowed 
  ;; this is the name of the tag placed on data generted by this test
  current
  ;; other options are 
  ;; date ;; to tag new date with a date
  ;; time ;; to tag new date with a time
  )
 (display
  ;; only one option is allowed
  ;; remember negative -> good  and positive -> bad 
  ;; positive-change       ;; displays increases in node counts
  ;; other options are:
  ;; negative-change    ;; displays decreases in node counts
  ;; change             ;; to display any change in node count
  percent-change             ;; to display any change in node count
  ;; rt-percent-change             ;; to display any change in node count
  ;; rt-positive-change ;; same as above but for real compile time
  ;; rt-negative-change
  ;; rt-change
  ;; whole-list         ;; display whole database line with current test appended
  ;; last-two ;; display only the current result with the previous result
  )
 (patterns

(pattern
   (list 2 4 6 5 1 7 1 0)
   (list 3 7 3 4 5 7 7 4)
   (list 3 6 9 0 5 9 6 5)
   (list 4 6 8 0 9 0 6 3)
   (list 4 2 8 5 2 9 5 5)
   (list 8 0 1 3 8 0 7 3)
   (list 7 8 6 4 9 2 2 5)
   (list 0 0 7 9 0 3 2 4)
   (list 7 2 1 9 3 6 4 1)
   (list 8 5 6 6 8 3 2 5)
   (list 3 8 1 2 0 3 9 2)
   (list 3 6 2 5 1 6 1 9)
   (list 9 2 8 2 0 5 6 9)
   (list 2 2 7 0 8 9 7 1)
   (list 9 9 6 9 4 7 4 8))
 (pattern
   (list 4 6 5 7 4 7 6 3)
   (list 1 7 8 2 4 6 3 8)
   (list 8 0 1 6 2 8 0 1)
   (list 9 6 3 4 5 7 4 9)
   (list 5 1 7 9 8 5 5 2)
   (list 4 5 6 8 1 9 9 9)
   (list 1 0 8 5 0 8 7 2)
   (list 6 2 8 1 1 2 3 6)
   (list 3 2 7 4 9 4 8 4)
   (list 9 4 4 0 5 3 2 6)
   (list 5 0 2 6 8 1 8 6)
   (list 5 6 9 6 0 2 4 5)
   (list 4 3 1 4 8 9 0 7)
   (list 5 4 0 0 0 2 7 5)
   (list 2 1 3 2 4 1 8 1))
 (pattern
   (list 9 7 7 1 2 3 7 8)
   (list 8 0 4 6 2 4 6 7)
   (list 1 6 8 3 8 7 8 0)
   (list 8 2 4 4 5 4 5 7)
   (list 1 4 8 5 9 7 4 7)
   (list 0 0 6 2 5 2 9 8)
   (list 8 7 1 8 4 1 0 4)
   (list 5 4 0 3 8 5 0 1)
   (list 1 0 9 2 0 3 0 0)
   (list 3 8 2 0 0 3 0 0)
   (list 1 3 8 7 5 0 2 0)
   (list 4 2 5 4 0 5 5 3)
   (list 8 4 6 8 9 8 0 5)
   (list 6 4 7 8 7 8 0 0)
   (list 1 0 8 8 0 2 1 4))
 (pattern
   (list 4 6 8 6 4 5 0 2)
   (list 2 6 2 3 6 4 8 4)
   (list 8 6 2 7 6 2 8 9)
   (list 2 8 8 4 0 9 8 6)
   (list 5 8 3 9 5 3 3 7)
   (list 1 7 1 9 1 1 3 1)
   (list 9 5 9 5 9 9 5 3)
   (list 9 3 7 9 4 7 5 1)
   (list 5 8 1 2 3 6 2 6)
   (list 4 3 5 7 6 8 9 6)
   (list 5 0 3 6 9 8 9 8)
   (list 3 8 9 9 5 4 1 2)
   (list 5 4 5 0 0 7 7 6)
   (list 2 2 4 8 3 3 6 8)
   (list 3 0 5 4 8 6 4 4))
 (pattern
   (list 5 3 3 2 9 4 5 6)
   (list 8 0 7 1 9 4 7 1)
   (list 8 1 1 3 6 8 4 9)
   (list 0 1 3 8 7 7 4 4)
   (list 2 0 7 2 4 2 8 5)
   (list 4 5 8 5 1 5 6 2)
   (list 9 9 7 5 9 1 7 9)
   (list 2 0 0 2 0 4 6 2)
   (list 6 5 6 3 7 5 8 3)
   (list 2 8 8 6 5 6 8 4)
   (list 8 5 2 7 9 9 9 1)
   (list 9 9 3 1 5 2 4 2)
   (list 7 0 5 7 7 5 0 2)
   (list 3 1 8 8 7 6 5 5)
   (list 3 7 5 2 6 4 6 5))
 (pattern
   (list 3 1 9 0 3 3 2 3)
   (list 5 7 0 5 4 2 7 7)
   (list 3 5 8 3 3 3 8 8)
   (list 0 5 3 6 9 9 3 2)
   (list 2 2 3 6 7 7 1 5)
   (list 7 3 2 1 5 9 1 1)
   (list 6 1 4 9 4 4 9 6)
   (list 0 2 4 9 3 9 4 6)
   (list 4 9 4 1 8 5 8 5)
   (list 8 0 9 5 9 2 6 7)
   (list 3 2 8 9 9 8 7 9)
   (list 2 1 0 6 0 6 2 6)
   (list 7 8 8 6 3 8 3 3)
   (list 9 2 8 0 4 7 8 9)
   (list 1 6 8 0 6 5 1 9))
 (pattern
   (list 8 4 5 1 0 9 7 8)
   (list 7 5 6 2 6 9 7 5)
   (list 2 7 7 8 4 7 8 8)
   (list 6 6 8 2 4 2 3 2)
   (list 6 0 3 6 9 1 6 8)
   (list 8 2 2 4 4 9 1 6)
   (list 7 9 6 3 8 4 1 4)
   (list 3 2 9 7 4 2 9 0)
   (list 3 5 8 4 6 7 5 6)
   (list 1 9 1 5 9 4 3 8)
   (list 5 0 3 4 6 7 0 9)
   (list 9 9 8 5 4 0 7 9)
   (list 5 7 5 3 4 0 9 8)
   (list 0 2 3 1 7 7 1 2)
   (list 9 6 8 5 3 9 7 4))

))

(report-errs)

; (define-syntax test-mac
;   (syntax-rules ()
;     ((id to-test eval-to)
;      (with-handlers ([(lambda exn #t)
;                       (lambda (exn) (failed-test (exn-message exn) 
;                                                   (quote to-test)
;                                                  '() eval-to))])
;                     (let ((res to-test))
;                       (if (equal? res eval-to)
;                           (begin (write res) #t)
;                           (failed-test '() (quote to-test) res eval-to)))))))



; (define (failed-test exn-msg test-code result should-have-been)
;   `((Test-Failure)
;     (Code ,test-code)
;     (Expected-Result ,should-have-been)
;     ,(if (null? exn-msg) 
;          `(Actual-Result ,result)
;          `(Exception ,exn-msg))))


; (define-syntax mytest
;   (lambda (stx)
;     (syntax-case stx ()
;       ((_ t result)
;        #`(test #t #,(syntax/loc stx (lambda () (test-mac t result)))))))) 


; (define-syntax pattern-test-gen
;   (lambda (stx)
;     (let ((helper (lambda (stx)
;                     (syntax-case stx (pattern)
;                       ((pattern pats ...)
;                        (let ((patter (map 
;                                       (lambda (x)
;                                         (syntax-case x (list)
;                                           ((list a b c d e f g h) 
;                                            (syntax ((list a b c d e f g z) z)))))
;                                       (syntax->list (syntax (pats ...))))))
;                        #`(let ((matcher 
;                                 (match-lambda #,@patter)))
                           
;                            #,@(map (lambda (x)
;                                      (let ((res (syntax-case x (list)
;                                                   ((list a b c d e f g z) (syntax z)))))
;                                        (quasisyntax/loc 
;                                         x
;                                         (mytest
;                                          (matcher #,x)
;                                          #,res))))
;                                    (syntax->list (syntax (pats ...)))))))))))
;       (syntax-case stx ()
;         ((_ ptg ...)
;          #`(begin #,@(map helper (syntax->list (syntax (ptg ...))))))))))




; (pattern-test-gen

; (pattern
;    (list 2 4 6 5 1 7 1 0)
;    (list 3 7 3 4 5 7 7 4)
;    (list 3 6 9 0 5 9 6 5)
;    (list 4 6 8 0 9 0 6 3)
;    (list 4 2 8 5 2 9 5 5)
;    (list 8 0 1 3 8 0 7 3)
;    (list 7 8 6 4 9 2 2 5)
;    (list 0 0 7 9 0 3 2 4)
;    (list 7 2 1 9 3 6 4 1)
;    (list 8 5 6 6 8 3 2 5)
;    (list 3 8 1 2 0 3 9 2)
;    (list 3 6 2 5 1 6 1 9)
;    (list 9 2 8 2 0 5 6 9)
;    (list 2 2 7 0 8 9 7 1)
;    (list 9 9 6 9 4 7 4 8))
;  (pattern
;    (list 4 6 5 7 4 7 6 3)
;    (list 1 7 8 2 4 6 3 8)
;    (list 8 0 1 6 2 8 0 1)
;    (list 9 6 3 4 5 7 4 9)
;    (list 5 1 7 9 8 5 5 2)
;    (list 4 5 6 8 1 9 9 9)
;    (list 1 0 8 5 0 8 7 2)
;    (list 6 2 8 1 1 2 3 6)
;    (list 3 2 7 4 9 4 8 4)
;    (list 9 4 4 0 5 3 2 6)
;    (list 5 0 2 6 8 1 8 6)
;    (list 5 6 9 6 0 2 4 5)
;    (list 4 3 1 4 8 9 0 7)
;    (list 5 4 0 0 0 2 7 5)
;    (list 2 1 3 2 4 1 8 1))
;  (pattern
;    (list 9 7 7 1 2 3 7 8)
;    (list 8 0 4 6 2 4 6 7)
;    (list 1 6 8 3 8 7 8 0)
;    (list 8 2 4 4 5 4 5 7)
;    (list 1 4 8 5 9 7 4 7)
;    (list 0 0 6 2 5 2 9 8)
;    (list 8 7 1 8 4 1 0 4)
;    (list 5 4 0 3 8 5 0 1)
;    (list 1 0 9 2 0 3 0 0)
;    (list 3 8 2 0 0 3 0 0)
;    (list 1 3 8 7 5 0 2 0)
;    (list 4 2 5 4 0 5 5 3)
;    (list 8 4 6 8 9 8 0 5)
;    (list 6 4 7 8 7 8 0 0)
;    (list 1 0 8 8 0 2 1 4))
;  (pattern
;    (list 4 6 8 6 4 5 0 2)
;    (list 2 6 2 3 6 4 8 4)
;    (list 8 6 2 7 6 2 8 9)
;    (list 2 8 8 4 0 9 8 6)
;    (list 5 8 3 9 5 3 3 7)
;    (list 1 7 1 9 1 1 3 1)
;    (list 9 5 9 5 9 9 5 3)
;    (list 9 3 7 9 4 7 5 1)
;    (list 5 8 1 2 3 6 2 6)
;    (list 4 3 5 7 6 8 9 6)
;    (list 5 0 3 6 9 8 9 8)
;    (list 3 8 9 9 5 4 1 2)
;    (list 5 4 5 0 0 7 7 6)
;    (list 2 2 4 8 3 3 6 8)
;    (list 3 0 5 4 8 6 4 4))
;  (pattern
;    (list 5 3 3 2 9 4 5 6)
;    (list 8 0 7 1 9 4 7 1)
;    (list 8 1 1 3 6 8 4 9)
;    (list 0 1 3 8 7 7 4 4)
;    (list 2 0 7 2 4 2 8 5)
;    (list 4 5 8 5 1 5 6 2)
;    (list 9 9 7 5 9 1 7 9)
;    (list 2 0 0 2 0 4 6 2)
;    (list 6 5 6 3 7 5 8 3)
;    (list 2 8 8 6 5 6 8 4)
;    (list 8 5 2 7 9 9 9 1)
;    (list 9 9 3 1 5 2 4 2)
;    (list 7 0 5 7 7 5 0 2)
;    (list 3 1 8 8 7 6 5 5)
;    (list 3 7 5 2 6 4 6 5))
;  (pattern
;    (list 3 1 9 0 3 3 2 3)
;    (list 5 7 0 5 4 2 7 7)
;    (list 3 5 8 3 3 3 8 8)
;    (list 0 5 3 6 9 9 3 2)
;    (list 2 2 3 6 7 7 1 5)
;    (list 7 3 2 1 5 9 1 1)
;    (list 6 1 4 9 4 4 9 6)
;    (list 0 2 4 9 3 9 4 6)
;    (list 4 9 4 1 8 5 8 5)
;    (list 8 0 9 5 9 2 6 7)
;    (list 3 2 8 9 9 8 7 9)
;    (list 2 1 0 6 0 6 2 6)
;    (list 7 8 8 6 3 8 3 3)
;    (list 9 2 8 0 4 7 8 9)
;    (list 1 6 8 0 6 5 1 9))
;  (pattern
;    (list 8 4 5 1 0 9 7 8)
;    (list 7 5 6 2 6 9 7 5)
;    (list 2 7 7 8 4 7 8 8)
;    (list 6 6 8 2 4 2 3 2)
;    (list 6 0 3 6 9 1 6 8)
;    (list 8 2 2 4 4 9 1 6)
;    (list 7 9 6 3 8 4 1 4)
;    (list 3 2 9 7 4 2 9 0)
;    (list 3 5 8 4 6 7 5 6)
;    (list 1 9 1 5 9 4 3 8)
;    (list 5 0 3 4 6 7 0 9)
;    (list 9 9 8 5 4 0 7 9)
;    (list 5 7 5 3 4 0 9 8)
;    (list 0 2 3 1 7 7 1 2)
;    (list 9 6 8 5 3 9 7 4))

;  )