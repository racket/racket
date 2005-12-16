;;;
;;; <selector-test.ss> ---- List selector tests
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

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (lib "selector.ss" "srfi" "1"))

  (provide selector-tests)

  (define selector-tests
    (make-test-suite
     "List selector tests"

     ;; FIRST

     (make-test-case
      "first:of-one"
      (assert-eq? (first '(hafnium)) 'hafnium))

     (make-test-case
      "first:of-many"
      (assert-eq? (first '(hahnium helium holmium hydrogen indium))
                  'hahnium))

     ;; SECOND

     (make-test-case
      "second:of-two"
      (assert-eq? (second '(iodine iridium)) 'iridium))

     (make-test-case
      "second:of-many"
      (assert-eq? (second '(iron krypton lanthanum lawrencium lead lithium))
                  'krypton))

     ;; THIRD

     (make-test-case
      "third:of-three"
      (assert-eq? (third '(lutetium magnesium manganese))
                  'manganese))

     (make-test-case
      "third:of-many"
      (assert-eq? (third '(mendelevium mercury molybdenum neodymium neon
                                       neptunium nickel))
                  'molybdenum))

     ;; FOURTH

     (make-test-case
      "fourth:of-four"
      (assert-eq? (fourth '(niobium nitrogen nobelium osmium))
                  'osmium))

     (make-test-case
      "fourth:of-many"
      (assert-eq? (fourth '(oxygen palladium phosphorus platinum plutonium
                                   polonium potassium praseodymium))
                  'platinum))

     ;; FIFTH

     (make-test-case
      "fifth:of-five"
      (assert-eq? (fifth '(promethium protatctinium radium radon rhenium))
                  'rhenium))

     (make-test-case
      "fifth:of-many"
      (assert-eq? (fifth '(rhodium rubidium ruthenium rutherfordium samarium
                                   scandium selenium silicon silver))
                  'samarium))

     ;; SIXTH

     (make-test-case
      "sixth:of-six"
      (assert-eq? (sixth '(sodium strontium sulfur tantalum technetium
                                  tellurium))
                  'tellurium))

     (make-test-case
      "sixth:of-many"
      (assert-eq? (sixth '(terbium thallium thorium thulium tin titanium
                                   tungsten uranium vanadium xenon))
                  'titanium))

     ;; SEVENTH

     (make-test-case
      "seventh:of-seven"
      (assert-eq? (seventh '(ytterbium yttrium zinc zirconium acacia abele
                                       ailanthus))
                  'ailanthus))

     (make-test-case
      "seventh:of-many"
      (assert-eq? (seventh '(alder allspice almond apple apricot ash aspen
                                   avocado balsa balsam banyan))
                  'aspen))

     ;; EIGHTH

     (make-test-case
      "eighth:of-eight"
      (assert-eq? (eighth '(basswood bay bayberry beech birch boxwood breadfruit
                                     buckeye))
                  'buckeye))

     (make-test-case
      "eighth:of-many"
      (assert-eq? (eighth '(butternut buttonwood cacao candleberry cashew cassia
                                      catalpa cedar cherry chestnut chinaberry
                                      chinquapin))
                  'cedar))

     ;; NINTH

     (make-test-case
      "ninth:of-nine"
      (assert-eq? (ninth '(cinnamon citron clove coconut cork cottonwood cypress
                                    date dogwood))
                  'dogwood))

     (make-test-case
      "ninth:of-many"
      (assert-eq? (ninth '(ebony elder elm eucalyptus ficus fig fir frankincense
                                 ginkgo grapefruit guava gum hawthorn))
                  'ginkgo))

     ;; TENTH

     (make-test-case
      "tenth:of-ten"
      (assert-eq? (tenth '(hazel hemlock henna hickory holly hornbeam ironwood
                                 juniper kumquat laburnum))
                  'laburnum))

     (make-test-case
      "tenth:of-many"
      (assert-eq? (tenth '(lancewood larch laurel lemon lime linden litchi
                                     locust logwood magnolia mahogany mango
                                     mangrove maple))
                  'magnolia))

     ;; CAR+CDR

     (make-test-case
      "car+cdr:pair"
      (let-values (((first second) (car+cdr (cons 'a 'b))))
        (assert-eq? first 'a)
        (assert-eq? second 'b)))

     (make-test-case
      "car+cdr:list"
      (let-values (((first second) (car+cdr (list 'a 'b))))
        (assert-eq? first 'a)
        (assert-equal? second (list 'b))))

     ;; TAKE

     (make-test-case
      "take:all-of-list"
      (assert-equal? (take '(medlar mimosa mulberry nutmeg oak) 5)
                     '(medlar mimosa mulberry nutmeg oak)))

     (make-test-case
      "take:front-of-list"
      (assert-equal? (take '(olive orange osier palm papaw peach pear) 5)
                     '(olive orange osier palm papaw)))

     (make-test-case
      "take:rear-of-list"
      (assert-equal?
       (take-right '(pecan persimmon pine pistachio plane plum pomegranite) 5)
       '(pine pistachio plane plum pomegranite)))

     (make-test-case
      "take:none-of-list"
      (assert-true (null? (take '(poplar quince redwood) 0))))

     (make-test-case
      "take:empty-list"
      (assert-true (null? (take '() 0))))

     ;; DROP

     (make-test-case
      "drop:all-of-list"
      (assert-true (null? (drop '(rosewood sandalwood sassfras satinwood senna) 5))))

     (make-test-case
      "drop:front-of-list"
      (assert-equal? (drop '(sequoia serviceberry spruce sycamore tamarack tamarind
                                     tamarugo)
                           5)
                     '(tamarind tamarugo)))

     (make-test-case
      "drop:rear-of-list"
      (assert-equal? (drop-right '(tangerine teak thuja torchwood upas walnut wandoo) 5)
                     '(tangerine teak)))

     (make-test-case
      "drop:none-of-list"
      (assert-equal? (drop '(whitebeam whitethorn wicopy) 0)
                     '(whitebeam whitethorn wicopy)))

     (make-test-case
      "drop:empty-list"
      (assert-true (null? (drop '() 0))))

     ;; TAKE!

     ;; List arguments to linear-update procedures are constructed
     ;; with the LIST procedure rather than as quoted data, since in
     ;; some implementations quoted data are not mutable.

     (make-test-case
      "take!:all-of-list"
      (assert-equal? (take! (list 'willow 'woollybutt 'wychelm 'yellowwood 'yew) 5)
                     '(willow woollybutt wychelm yellowwood yew)))

     (make-test-case
      "take!:front-of-list"
      (assert-equal? (take! (list 'ylang-ylang 'zebrawood 'affenpinscher 'afghan
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

     (make-test-case
      "take!:none-of-list"
      (assert-true (null? (take! (list 'briard 'bulldog 'chihuahua) 0))))

     (make-test-case
      "take!:empty-list"
      (assert-true (null? (take! '() 0))))

     ;; DROP-RIGHT!

     (make-test-case
      "drop-right!:all-of-list"
      (assert-true (null? (drop-right! (list 'chow 'collie 'coonhound 'clydesdale 'dachshund)
                                       5))))

     (make-test-case
      "drop-right!:rear-of-list"
      (assert-equal? (drop-right! (list 'groenendael 'harrier 'hound 'husky 'keeshond
                                        'komondor 'kuvasz)
                                  5)
                     '(groenendael harrier)))

     (make-test-case
      "drop-right!:none-of-list"
      (assert-equal? (drop-right! (list 'labrador 'malamute 'malinois) 0)
                     '(labrador malamute malinois)))

     (make-test-case
      "drop-right!:empty-list"
      (assert-true (null? (drop-right! '() 0))))

     ;; LAST

     (make-test-case
      "last:of-singleton"
      (assert-eq? (last '(maltese))
                  'maltese))

     (make-test-case
      "last:of-longer-list"
      (assert-eq? (last '(mastiff newfoundland nizinny otterhound papillon))
                  'papillon))

     ;; LAST-PAIR

     (make-test-case
      "last-pair:of-singleton"
      (let ((pair '(pekingese)))
        (assert-eq? (last-pair pair)
                    pair)))

     (make-test-case
      "last-pair:of-longer-list"
      (let ((pair '(pointer)))
        (assert-eq? (last-pair (cons 'pomeranian
                                     (cons 'poodle
                                           (cons 'pug (cons 'puli pair)))))
                    pair)))

     (make-test-case
      "last-pair:of-improper-list"
      (let ((pair '(manx . siamese)))
        (assert-eq? (last-pair (cons 'abyssinian (cons 'calico pair)))
                    pair)))

     ))
  )

;;; selector-test.ss ends here
