#lang racket

(require racklog
         "./puzzle.rkt"
         rackunit)

;;This example is from Sterling & Shapiro, p. 214.
;;
;;The problem reads: Three friends came first, second and
;;third in a competition.  Each had a different name, liked a
;;different sport, and had a different nationality.  Michael
;;likes basketball, and did better than the American.  Simon,
;;the Israeli, did better than the tennis player.  The
;;cricket player came first.  Who's the Australian?  What
;;sport does Richard play?

(define person
  ;;a structure-builder for persons
  (lambda (name country sport)
    (list 'person name country sport)))

(define %games
  (%rel (clues queries solution the-men
	 n1 n2 n3 c1 c2 c3 s1 s2 s3)
    ((clues queries solution)
     (%= the-men
       (list (person n1 c1 s1) (person n2 c2 s2) (person n3 c3 s3)))
     (%games-clues the-men clues)
     (%games-queries the-men queries solution))))
    
(define %games-clues
  (%rel (the-men clue1-man1 clue1-man2 clue2-man1 clue2-man2 clue3-man)
    ((the-men
       (list
	 (%did-better clue1-man1 clue1-man2 the-men)
	 (%name clue1-man1 'michael)
	 (%sport clue1-man1 'basketball)
	 (%country clue1-man2 'usa)

	 (%did-better clue2-man1 clue2-man2 the-men)
	 (%name clue2-man1 'simon)
	 (%country clue2-man1 'israel)
	 (%sport clue2-man2 'tennis)

	 (%first the-men clue3-man)
	 (%sport clue3-man 'cricket))))))

(define %games-queries
  (%rel (the-men man1 man2 aussies-name dicks-sport)
    ((the-men
       (list
	 (%member man1 the-men)
	 (%country man1 'australia)
	 (%name man1 aussies-name)

	 (%member man2 the-men)
	 (%name man2 'richard)
	 (%sport man2 dicks-sport))
       (list
	 (list aussies-name 'is 'the 'australian)
	 (list 'richard 'plays dicks-sport))))))
	 
(define %did-better
  (%rel (a b c)
    ((a b (list a b c)))
    ((a c (list a b c)))
    ((b c (list a b c)))))

(define %name
  (%rel (name country sport)
    (((person name country sport) name))))

(define %country
  (%rel (name country sport)
    (((person name country sport) country))))

(define %sport
  (%rel (name country sport)
    (((person name country sport) sport))))

(define %first
  (%rel (car cdr)
    (((cons car cdr) car))))

;;With the above as the database, and also loading the file
;;puzzle.scm containing the puzzle solver, we merely need to
;;ask (solve-puzzle %games) to get the solution, which is
;;
;;((michael is the australian) (richard plays tennis))

(check-equal? (solve-puzzle %games)
              '((solution= . ((michael is the australian) (richard plays tennis)))))
