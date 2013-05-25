#lang racket

(require racklog)

;Exercise 14.1 (iv) from Sterling & Shapiro, p. 217-8  

;There are 5 houses, each of a different color and inhabited
;by a man of a different nationality, with a different pet,
;drink and cigarette choice.
;
;1. The Englishman lives in the red house
;2. The Spaniard owns the dog
;3. Coffee is drunk in the green house
;4. The Ukrainian drinks tea
;5. The green house is to the immediate right of the ivory house
;6. The Winston smoker owns snails
;7. Kools are smoked in the yellow house
;8. Milk is drunk in the middle house
;9. The Norwegian lives in the first house on the left
;10. The Chesterfield smoker lives next to the man with the fox
;11. Kools are smoked in the house adjacent to the horse's place
;12. The Lucky Strike smoker drinks orange juice
;13. The Japanese smokes Parliaments
;14. The Norwegian lives next to the blue house

;Who owns the zebra?  Who drinks water?

(define house
  (lambda (hue nation pet drink cigarette)
    (list 'house hue nation pet drink cigarette)))

(define %hue (%rel (h) (((house h (_) (_) (_) (_)) h))))
(define %nation (%rel (n) (((house (_) n (_) (_) (_)) n))))
(define %pet (%rel (p) (((house (_) (_) p (_) (_)) p))))
(define %drink (%rel (d) (((house (_) (_) (_) d (_)) d))))
(define %cigarette (%rel (c) (((house (_) (_) (_) (_) c) c))))

(define %adjacent
  (%rel (a b)
    ((a b (list a b (_) (_) (_))))
    ((a b (list (_) a b (_) (_))))
    ((a b (list (_) (_) a b (_))))
    ((a b (list (_) (_) (_) a b)))))

(define %middle
  (%rel (a)
    ((a (list (_) (_) a (_) (_))))))

(define %houses
  (%rel (row-of-houses clues queries solution
	 h1 h2 h3 h4 h5 n1 n2 n3 n4 n5 p1 p2 p3 p4 p5
	 d1 d2 d3 d4 d5 c1 c2 c3 c4 c5)
    ((clues queries solution)
     (%= row-of-houses
       (list
	 (house h1 n1 p1 d1 c1)
	 (house h2 n2 p2 d2 c2)
	 (house h3 n3 p3 d3 c3)
	 (house h4 n4 p4 d4 c4)
	 (house h5 n5 p5 d5 c5)))
     (%houses-clues row-of-houses clues)
     (%houses-queries row-of-houses queries solution))))

(define %houses-clues
  (%rel (row-of-houses abode1 abode2 abode3 abode4 abode5 abode6 abode7
	 abode8 abode9 abode10 abode11 abode12 abode13 abode14 abode15)
    ((row-of-houses
       (list
	 (%member abode1 row-of-houses)
	 (%nation abode1 'english)
	 (%hue abode1 'red)

	 (%member abode2 row-of-houses)
	 (%nation abode2 'spain)
	 (%pet abode2 'dog)

	 (%member abode3 row-of-houses)
	 (%drink abode3 'coffee)
	 (%hue abode3 'green)

	 (%member abode4 row-of-houses)
	 (%nation abode4 'ukraine)
	 (%drink abode4 'tea)

	 (%member abode5 row-of-houses)
	 (%adjacent abode5 abode3 row-of-houses)
	 (%hue abode5 'ivory)

	 (%member abode6 row-of-houses)
	 (%cigarette abode6 'winston)
	 (%pet abode6 'snail)

	 (%member abode7 row-of-houses)
	 (%cigarette abode7 'kool)
	 (%hue abode7 'yellow)

	 (%= (list (_) (_) abode8 (_) (_)) row-of-houses)
	 (%drink abode8 'milk)

	 (%= (list abode9 (_) (_) (_) (_)) row-of-houses)
	 (%nation abode9 'norway)

	 (%member abode10 row-of-houses)
	 (%member abode11 row-of-houses)
	 (%or (%adjacent abode10 abode11 row-of-houses)
	   (%adjacent abode11 abode10 row-of-houses))
	 (%cigarette abode10 'chesterfield)
	 (%pet abode11 'fox)

	 (%member abode12 row-of-houses)
	 (%or (%adjacent abode7 abode12 row-of-houses) 
	   (%adjacent abode12 abode7 row-of-houses))
	 (%pet abode12 'horse)

	 (%member abode13 row-of-houses)
	 (%cigarette abode13 'lucky-strike)
	 (%drink abode13 'oj)

	 (%member abode14 row-of-houses)
	 (%nation abode14 'japan)
	 (%cigarette abode14 'parliament)

	 (%member abode15 row-of-houses)
	 (%or (%adjacent abode9 abode15 row-of-houses) 
	   (%adjacent abode15 abode9 row-of-houses))
	 (%hue abode15 'blue))))))

(define %houses-queries
  (%rel (row-of-houses abode1 abode2 zebra-owner water-drinker)
    ((row-of-houses
       (list
	 (%member abode1 row-of-houses)
	 (%pet abode1 'zebra)
	 (%nation abode1 zebra-owner)

	 (%member abode2 row-of-houses)
	 (%drink abode2 'water)
	 (%nation abode2 water-drinker))

       (list (list zebra-owner 'owns 'the 'zebra)
	 (list water-drinker 'drinks 'water))))))

;Note: Perhaps there is a way to rewrite the 
;program so that it doesn't rely on the occurs check.

(require "puzzle.rkt" tests/eli-tester)
(use-occurs-check? #t)
(test (solve-puzzle %houses))
