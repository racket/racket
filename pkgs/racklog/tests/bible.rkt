#lang racket

(require racklog
         rackunit)

;The following is the "Biblical" database from "The Art of
;Prolog", Sterling & Shapiro, ch. 1.

;(%father X Y) :- X is the father of Y.

(define %father
  (%rel ()
    (('terach 'abraham)) (('terach 'nachor)) (('terach 'haran))
    (('abraham 'isaac)) (('haran 'lot)) (('haran 'milcah))
    (('haran 'yiscah))))

;(%mother X Y) :- X is the mother of Y.

(define %mother
  (%rel () (('sarah 'isaac))))

(define %male
  (%rel ()
    (('terach)) (('abraham)) (('isaac)) (('lot)) (('haran)) (('nachor))))

(define %female
  (%rel ()
    (('sarah)) (('milcah)) (('yiscah))))

;AoP, ch. 17.  Finding all the children of a particular
;father.  (%children F CC) :- CC is the list of children
;whose father is F.  First approach: %children-1 uses an
;auxiliary predicate %children-aux, which uses an
;accumulator.

(define %children-1

  (letrec ((children-aux
	     (%rel (x a cc c)
	       ((x a cc)
                 (%father x c) (%not (%member c a)) !
                 (children-aux x (cons c a) cc))
	       ((x cc cc)))))

    (%rel (x cc)
      ((x cc) (children-aux x '() cc)))))

(define terachs-kids-test
  ;find all the children of Terach.  Returns
  ;cc = (abraham nachor haran)
  (lambda ()
    (%which (cc)
      (%children-1 'terach cc))))

(check-equal? (terachs-kids-test)
              `((cc . (haran nachor abraham))))

(define dad-kids-test
  ;find a father and all his children.  Returns
  ;f = terach, cc = (haran nachor abraham).
  ;(%more) fails, showing flaw in %children-1.
  ;see AoP, ch. 17, p. 267
  (lambda ()
    (%which (f cc)
      (%children-1 f cc))))

(check-equal? (dad-kids-test)
              `((f . terach) (cc . (haran nachor abraham))))

(define terachs-kids-test-2
  ;find all the kids of Terach, using %set-of.
  ;returns kk = (abraham nachor haran)
  (lambda ()
    (%let (k)
      (%which (kk)
        (%set-of k (%father 'terach k) kk)))))

(check-equal? (terachs-kids-test-2)
              `((kk . (abraham nachor haran))))

;This is a better definition of the %children predicate.
;Uses set predicate %bag-of

(define %children
  (%rel (x kids c)
    ((kids) (%set-of c (%father x c) kids))))

(define dad-kids-test-2
  ;find each dad-kids combo.
  ;1st soln: dad = terach, kids = (abraham nachor haran)
  ;(%more) gives additional solutions.
  (lambda ()
    (%let (x)
      (%which (dad kids)
        (%set-of x (%free-vars (dad)
                     (%father dad x))
          kids)))))

(check-equal? (dad-kids-test-2)
              `((dad . terach) (kids . (abraham nachor haran))))

(define dad-kids-test-3
  ;looks like dad-kids-test-2, but dad is now
  ;existentially quantified.  returns a set of
  ;kids (i.e., anything with a father)
  (lambda ()
    (%let (x)
      (%which (dad kids)
        (%set-of x (%father dad x)
          kids)))))

(check-equal? (dad-kids-test-3)
              `((dad . _) (kids . (abraham nachor haran isaac lot milcah yiscah))))

(define dad-kids-test-4
  ;find the set of dad-kids.
  ;since dad is existentially quantified,
  ;this gives the wrong answer: it gives
  ;one set containing all the kids
  (lambda ()
    (%let (dad kids x)
      (%which (dad-kids)
	(%set-of (list dad kids)
	  (%set-of x (%father dad x) kids)
	  dad-kids)))))

(check-equal? (dad-kids-test-4)
              `((dad-kids . ((_ (abraham nachor haran isaac lot milcah yiscah))))))

(define dad-kids-test-5
  ;the correct solution.  dad is
  ;identified as a free var.
  ;returns a set of dad-kids, one for
  ;each dad
  (lambda ()
    (%let (dad kids x)
      (%which (dad-kids)
	(%set-of (list dad kids)
	  (%set-of x (%free-vars (dad)
                       (%father dad x))
            kids)
	  dad-kids)))))

(check-equal? (dad-kids-test-5)
              `((dad-kids . ((terach (abraham nachor haran)) (abraham (isaac)) (haran (lot milcah yiscah))))))
