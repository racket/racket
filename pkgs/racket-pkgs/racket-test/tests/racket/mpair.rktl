
(load-relative "loadtest.rktl")

(Section 'mpair)

(require scheme/mpair)


;; ----------------------------------------
;; mreverse!

(test null mreverse! null)
(test (mlist 1) mreverse! (mlist 1))
(test (mlist 3 2 1) mreverse! (mlist 1 2 3))

(define a (mlist 1 2 3))
(test (mlist 3 2 1) mreverse! a)
(test (mlist 1) values a)

;; ----------------------------------------
;; mappend!

;; no args
(test null mappend!)

;; one arg
(test (mlist 3 4 5) mappend! (mlist 3 4 5))

;; two args
(test (mlist 3 4 5 6) mappend! (mlist 3 4) (mlist 5 6))

(define a (mlist 3 4))
(test (mlist 3 4 5 6) mappend! a (mlist 5 6))
(test (mlist 3 4 5 6) values a)

(test (mlist 3 4 5) mappend! null (mlist 3 4 5))

;; three args
(test (mlist 3 4 5 6 7 8)
        mappend! (mlist 3 4) (mlist 5 6) (mlist 7 8))
(test (mlist 3 4 5 6) mappend! (mlist 3 4) null (mlist 5 6))

(define a2 (mlist 3 4))
(test (mlist 3 4 5 6) mappend! null a2 null (mlist 5 6))
(test (mlist 3 4 5 6) values a2)

(define a3result (mcons 3 'bogus))
(set-mcdr! a3result a3result)
(define a3 (mlist 3))
(test a3result mappend! a3 a3)
(test a3result values a3)

;; ----------------------------------------

(report-errs)
