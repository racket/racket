(load-relative "../loadtest.ss")
(SECTION 'MATCH-PERFORMANCE)

(require (lib "pretty.ss"))
(require-for-syntax (lib "pretty.ss"))
(require (lib "include.ss"))
(require (lib "plt-match.ss"))

(include "match-compile-perf.scm")

(match-performance-test
 (file
  ;; this is the name of the underlying file that handles the 
  ;; persistance of test data between invocations of this performance test
  ;; Each time a test is run one has the option to only compare current
  ;; data with previous data or one can modify the database file by appending
  ;; hopefullly improved data.
  ;; For most purposes it is advisable not to alter the database.
  (name "rand-short-list-hist.db")
  ;; onlly one of the following options is allowed
  ;; write-new   ;; uncomment to write an new test database 
                 ;; (this requires the whole-list display option below)
  ;; add-results ;; uncomment to append the results of the current test
                 ;; to the database
  )
 (tag
  ;; only one option is allowed 
  ;; this is the name of the tag placed on data generted by this test
  currnet
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
  percent-change             ;; to display any change in node count
  ;; rt-positive-change ;; same as above but for real compile time
  ;; rt-negative-change
  ;; rt-change
  ;; whole-list         ;; display whole database line with current test appended
  ;; last-two ;; display only the current result with the previous result
  )
 (patterns
(pattern
   (list 9 8 7 9)
   (list 6 9 4 3)
   (list 6 3 8 7)
   (list 5 2 8 3)
   (list 0 3 9 9)
   (list 7 5 1 7)
   (list 3 7 7 0)
   (list 1 7 3 0)
   (list 7 0 9 3)
   (list 9 3 8 8)
   (list 9 6 7 6)
   (list 9 7 9 1))
 (pattern
   (list 0 1 0 9)
   (list 6 2 6 0)
   (list 1 4 2 3)
   (list 3 5 3 0)
   (list 5 3 5 7)
   (list 8 3 5 9)
   (list 1 4 6 2)
   (list 1 7 4 2)
   (list 0 4 3 7)
   (list 8 0 9 0)
   (list 4 3 3 7)
   (list 8 8 9 5))
 (pattern
   (list 3 4 4 2)
   (list 9 1 3 0)
   (list 5 9 3 7)
   (list 9 7 1 9)
   (list 3 4 8 2)
   (list 4 7 4 0)
   (list 0 9 7 0))
(pattern
   (list 9 8 7 9)
   (list 6 9 4 3)
   (list 6 3 8 7)
   (list 5 2 8 3)
   (list 0 3 9 9)
   (list 7 5 1 7)
   (list 3 7 7 0)
   (list 1 7 3 0)
   (list 7 0 9 3)
   (list 9 3 8 8)
   (list 9 6 7 6)
   (list 9 7 9 1))
 (pattern
   (list 0 1 0 9)
   (list 6 2 6 0)
   (list 1 4 2 3)
   (list 3 5 3 0)
   (list 5 3 5 7)
   (list 8 3 5 9)
   (list 1 4 6 2)
   (list 1 7 4 2)
   (list 0 4 3 7)
   (list 8 0 9 0)
   (list 4 3 3 7)
   (list 8 8 9 5))
 (pattern
   (list 3 4 4 2)
   (list 9 1 3 0)
   (list 5 9 3 7)
   (list 9 7 1 9)
   (list 3 4 8 2)
   (list 4 7 4 0)
   (list 0 9 7 0))

(pattern
   (list 7 8 8 3)
   (list 2 2 5 1)
   (list 4 0 2 9)
   (list 0 7 8 1)
   (list 4 9 0 7)
   (list 4 9 1 0)
   (list 6 7 1 9)
   (list 8 0 1 8)
   (list 9 1 1 1)
   (list 6 8 5 0)
   (list 8 9 1 0)
   (list 6 2 1 2))
 (pattern
   (list 1 2 1 7)
   (list 1 5 8 9)
   (list 2 9 0 1)
   (list 9 4 9 8)
   (list 5 0 2 1)
   (list 8 7 3 8)
   (list 8 7 9 6)
   (list 9 0 8 0)
   (list 2 9 8 5)
   (list 4 6 5 9)
   (list 7 7 2 6)
   (list 1 3 7 9))
 (pattern
   (list 5 9 0 5)
   (list 8 6 3 6)
   (list 3 2 4 2)
   (list 3 2 4 7)
   (list 3 4 3 0)
   (list 2 0 9 9)
   (list 7 3 8 1))

 (pattern
   (list 6 5 0 1)
   (list 4 0 8 4)
   (list 6 1 2 1)
   (list 6 6 3 1)
   (list 8 0 8 1)
   (list 4 3 1 7)
   (list 3 2 6 1)
   (list 5 4 2 3)
   (list 9 2 6 5)
   (list 4 4 1 3)
   (list 8 3 4 4)
   (list 9 0 7 9))
 (pattern
   (list 0 5 3 4)
   (list 9 6 1 4)
   (list 9 0 5 6)
   (list 4 9 2 6)
   (list 3 8 3 8)
   (list 5 5 3 3)
   (list 8 7 9 0)
   (list 7 6 1 7)
   (list 3 4 4 4)
   (list 1 5 9 2)
   (list 7 6 0 4)
   (list 6 2 0 1))
 (pattern
   (list 3 5 9 8)
   (list 0 2 3 1)
   (list 2 4 3 9)
   (list 0 4 9 5)
   (list 9 3 0 2)
   (list 0 9 6 8)
   (list 5 6 2 3))

))

(report-errs)