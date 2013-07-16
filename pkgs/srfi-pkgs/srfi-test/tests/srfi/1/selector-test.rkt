;;;
;;; <selector-test.rkt> ---- List selector tests
;;; Time-stamp: <05/12/16 21:13:25 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; SRFI-1 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-1 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-1; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

;; Originally created by:

;; John David Stone
;; Department of Mathematics and Computer Science
;; Grinnell College
;; stone@math.grin.edu

(module selector-test
  mzscheme

  (require rackunit)
  (require srfi/1/selector)

  (provide selector-tests)

  (define selector-tests
    (test-suite
     "List selector tests"

     ;; FIRST

     (test-case
      "first:of-one"
      (check-eq? (first '(hafnium)) 'hafnium))

     (test-case
      "first:of-many"
      (check-eq? (first '(hahnium helium holmium hydrogen indium))
                  'hahnium))

     ;; SECOND

     (test-case
      "second:of-two"
      (check-eq? (second '(iodine iridium)) 'iridium))

     (test-case
      "second:of-many"
      (check-eq? (second '(iron krypton lanthanum lawrencium lead lithium))
                  'krypton))

     ;; THIRD

     (test-case
      "third:of-three"
      (check-eq? (third '(lutetium magnesium manganese))
                  'manganese))

     (test-case
      "third:of-many"
      (check-eq? (third '(mendelevium mercury molybdenum neodymium neon
                                       neptunium nickel))
                  'molybdenum))

     ;; FOURTH

     (test-case
      "fourth:of-four"
      (check-eq? (fourth '(niobium nitrogen nobelium osmium))
                  'osmium))

     (test-case
      "fourth:of-many"
      (check-eq? (fourth '(oxygen palladium phosphorus platinum plutonium
                                   polonium potassium praseodymium))
                  'platinum))

     ;; FIFTH

     (test-case
      "fifth:of-five"
      (check-eq? (fifth '(promethium protatctinium radium radon rhenium))
                  'rhenium))

     (test-case
      "fifth:of-many"
      (check-eq? (fifth '(rhodium rubidium ruthenium rutherfordium samarium
                                   scandium selenium silicon silver))
                  'samarium))

     ;; SIXTH

     (test-case
      "sixth:of-six"
      (check-eq? (sixth '(sodium strontium sulfur tantalum technetium
                                  tellurium))
                  'tellurium))

     (test-case
      "sixth:of-many"
      (check-eq? (sixth '(terbium thallium thorium thulium tin titanium
                                   tungsten uranium vanadium xenon))
                  'titanium))

     ;; SEVENTH

     (test-case
      "seventh:of-seven"
      (check-eq? (seventh '(ytterbium yttrium zinc zirconium acacia abele
                                       ailanthus))
                  'ailanthus))

     (test-case
      "seventh:of-many"
      (check-eq? (seventh '(alder allspice almond apple apricot ash aspen
                                   avocado balsa balsam banyan))
                  'aspen))

     ;; EIGHTH

     (test-case
      "eighth:of-eight"
      (check-eq? (eighth '(basswood bay bayberry beech birch boxwood breadfruit
                                     buckeye))
                  'buckeye))

     (test-case
      "eighth:of-many"
      (check-eq? (eighth '(butternut buttonwood cacao candleberry cashew cassia
                                      catalpa cedar cherry chestnut chinaberry
                                      chinquapin))
                  'cedar))

     ;; NINTH

     (test-case
      "ninth:of-nine"
      (check-eq? (ninth '(cinnamon citron clove coconut cork cottonwood cypress
                                    date dogwood))
                  'dogwood))

     (test-case
      "ninth:of-many"
      (check-eq? (ninth '(ebony elder elm eucalyptus ficus fig fir frankincense
                                 ginkgo grapefruit guava gum hawthorn))
                  'ginkgo))

     ;; TENTH

     (test-case
      "tenth:of-ten"
      (check-eq? (tenth '(hazel hemlock henna hickory holly hornbeam ironwood
                                 juniper kumquat laburnum))
                  'laburnum))

     (test-case
      "tenth:of-many"
      (check-eq? (tenth '(lancewood larch laurel lemon lime linden litchi
                                     locust logwood magnolia mahogany mango
                                     mangrove maple))
                  'magnolia))

     ;; CAR+CDR

     (test-case
      "car+cdr:pair"
      (let-values (((first second) (car+cdr (cons 'a 'b))))
        (check-eq? first 'a)
        (check-eq? second 'b)))

     (test-case
      "car+cdr:list"
      (let-values (((first second) (car+cdr (list 'a 'b))))
        (check-eq? first 'a)
        (check-equal? second (list 'b))))

     ;; TAKE

     (test-case
      "take:all-of-list"
      (check-equal? (take '(medlar mimosa mulberry nutmeg oak) 5)
                     '(medlar mimosa mulberry nutmeg oak)))

     (test-case
      "take:front-of-list"
      (check-equal? (take '(olive orange osier palm papaw peach pear) 5)
                     '(olive orange osier palm papaw)))

     (test-case
      "take:rear-of-list"
      (check-equal?
       (take-right '(pecan persimmon pine pistachio plane plum pomegranite) 5)
       '(pine pistachio plane plum pomegranite)))

     (test-case
      "take:none-of-list"
      (check-true (null? (take '(poplar quince redwood) 0))))

     (test-case
      "take:empty-list"
      (check-true (null? (take '() 0))))

     ;; DROP

     (test-case
      "drop:all-of-list"
      (check-true (null? (drop '(rosewood sandalwood sassfras satinwood senna) 5))))

     (test-case
      "drop:front-of-list"
      (check-equal? (drop '(sequoia serviceberry spruce sycamore tamarack tamarind
                                     tamarugo)
                           5)
                     '(tamarind tamarugo)))

     (test-case
      "drop:rear-of-list"
      (check-equal? (drop-right '(tangerine teak thuja torchwood upas walnut wandoo) 5)
                     '(tangerine teak)))

     (test-case
      "drop:none-of-list"
      (check-equal? (drop '(whitebeam whitethorn wicopy) 0)
                     '(whitebeam whitethorn wicopy)))

     (test-case
      "drop:empty-list"
      (check-true (null? (drop '() 0))))

     ;; TAKE!

     ;; List arguments to linear-update procedures are constructed
     ;; with the LIST procedure rather than as quoted data, since in
     ;; some implementations quoted data are not mutable.

     (test-case
      "take!:all-of-list"
      (check-equal? (take! (list 'willow 'woollybutt 'wychelm 'yellowwood 'yew) 5)
                     '(willow woollybutt wychelm yellowwood yew)))

     (test-case
      "take!:front-of-list"
      (check-equal? (take! (list 'ylang-ylang 'zebrawood 'affenpinscher 'afghan
                                  'airedale 'alsatian 'barbet)
                            5)
                     '(ylang-ylang zebrawood affenpinscher afghan airedale)))

;(test 'take!:rear-of-list
;      (take! (list 'basenji 'basset 'beagle 'bloodhound 'boarhound
;                   'borzoi 'boxer)
;             -5)
;      (lambda (result)
;        (equal? result '(beagle bloodhound boarhound borzoi
;                                boxer))))

     (test-case
      "take!:none-of-list"
      (check-true (null? (take! (list 'briard 'bulldog 'chihuahua) 0))))

     (test-case
      "take!:empty-list"
      (check-true (null? (take! '() 0))))

     ;; DROP-RIGHT!

     (test-case
      "drop-right!:all-of-list"
      (check-true (null? (drop-right! (list 'chow 'collie 'coonhound 'clydesdale 'dachshund)
                                       5))))

     (test-case
      "drop-right!:rear-of-list"
      (check-equal? (drop-right! (list 'groenendael 'harrier 'hound 'husky 'keeshond
                                        'komondor 'kuvasz)
                                  5)
                     '(groenendael harrier)))

     (test-case
      "drop-right!:none-of-list"
      (check-equal? (drop-right! (list 'labrador 'malamute 'malinois) 0)
                     '(labrador malamute malinois)))

     (test-case
      "drop-right!:empty-list"
      (check-true (null? (drop-right! '() 0))))

     ;; LAST

     (test-case
      "last:of-singleton"
      (check-eq? (last '(maltese))
                  'maltese))

     (test-case
      "last:of-longer-list"
      (check-eq? (last '(mastiff newfoundland nizinny otterhound papillon))
                  'papillon))

     ;; LAST-PAIR

     (test-case
      "last-pair:of-singleton"
      (let ((pair '(pekingese)))
        (check-eq? (last-pair pair)
                    pair)))

     (test-case
      "last-pair:of-longer-list"
      (let ((pair '(pointer)))
        (check-eq? (last-pair (cons 'pomeranian
                                     (cons 'poodle
                                           (cons 'pug (cons 'puli pair)))))
                    pair)))

     (test-case
      "last-pair:of-improper-list"
      (let ((pair '(manx . siamese)))
        (check-eq? (last-pair (cons 'abyssinian (cons 'calico pair)))
                    pair)))

     ))
  )

;;; selector-test.rkt ends here
