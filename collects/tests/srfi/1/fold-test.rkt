;;;
;;; <fold-test.rkt> ---- Tests for list folds
;;; Time-stamp: <05/12/16 21:19:37 noel>
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

(module fold-test
  mzscheme
  (require rackunit)
  (require (all-except srfi/1/fold map for-each)
   (rename srfi/1/fold s:map map)
   (rename srfi/1/fold s:for-each for-each))

  (provide fold-tests)

  (define fold-tests
    (test-suite
     "Folding list procedures tests"

     ;; UNFOLD

     (test-case
      "unfold:predicate-always-satisfied"
      (check-true (null?
                    (unfold (lambda (seed) #t)
                            (lambda (seed) (* seed 2))
                            (lambda (seed) (* seed 3))
                            1))))

     (test-case
      "unfold:normal-case"
      (check-equal?
       (unfold (lambda (seed) (= seed 729))
               (lambda (seed) (* seed 2))
               (lambda (seed) (* seed 3))
               1)
       '(2 6 18 54 162 486)))

     ;; UNFOLD-RIGHT

     (test-case
      "unfold-right:predicate-always-satisfied"
      (check-equal?
       (unfold-right (lambda (seed) #t)
                     (lambda (seed) (* seed 2))
                     (lambda (seed) (* seed 3))
                     (lambda (seed) (* seed 5))
                     '(1))
       (list 1)))

     (test-case
      "unfold-right:normal-case"
      (check-equal?
       (unfold-right (lambda (seed) (= seed 729))
                     (lambda (seed) (* seed 2))
                     (lambda (seed) (* seed 3))
                     1)
       '(486 162 54 18 6 2)))

     (test-case
      "unfold-right:normal-case-opt-arg"
      (check-equal?
       (unfold-right (lambda (seed) (= seed 729))
                     (lambda (seed) (* seed 2))
                     (lambda (seed) (* seed 3))
                     1
                     '(1))
       '(486 162 54 18 6 2 1)))

     ;; FOLD

     (test-case
      "fold:one-null-list"
      (check = (fold (lambda (alpha beta) (* alpha (+ beta 1))) 13 '()) 13))

     (test-case
      "fold:one-singleton-list"
      (check = (fold (lambda (alpha beta) (* alpha (+ beta 1))) 13 '(15)) 210))

     (test-case
      "fold:one-longer-list"
      (check =
              (fold (lambda (alpha beta) (* alpha (+ beta 1)))
                    13
                    '(15 17 19 21 23))
              32927582))

     (test-case
      "fold:several-null-lists"
      (check-eq? (fold vector 'Chad '() '() '() '() '()) 'Chad))

     (test-case
      "fold:several-singleton-lists"
      (check-equal?
       (fold vector 'Chile '(China) '(Colombia) '(Comoros) '(Congo)
             '(Croatia))
       '#(China Colombia Comoros Congo Croatia Chile)))

     (test-case
      "fold:several-longer-lists"
      (check-equal?
       (fold (lambda (alpha beta gamma delta epsilon zeta)
               (cons (vector alpha beta gamma delta epsilon) zeta))
             '()
             '(Cuba Cyprus Denmark Djibouti Dominica Ecuador Egypt)
             '(Eritrea Estonia Ethiopia Fiji Finland France Gabon)
             '(Gambia Georgia Germany Ghana Greece Grenada
                      Guatemala)
             '(Guinea Guyana Haiti Honduras Hungary Iceland India)
             '(Indonesia Iran Iraq Ireland Israel Italy Jamaica))
       '(#(Egypt Gabon Guatemala India Jamaica)
         #(Ecuador France Grenada Iceland Italy)
         #(Dominica Finland Greece Hungary Israel)
         #(Djibouti Fiji Ghana Honduras Ireland)
         #(Denmark Ethiopia Germany Haiti Iraq)
         #(Cyprus Estonia Georgia Guyana Iran)
         #(Cuba Eritrea Gambia Guinea Indonesia))))

     (test-case
      "fold:lists-of-different-lengths"
      (check-equal?
       (fold (lambda (alpha beta gamma delta)
               (cons (vector alpha beta gamma) delta))
             '()
             '(Japan Jordan Kazakhstan Kenya)
             '(Kiribati Kuwait)
             '(Kyrgyzstan Laos Latvia))
       '(#(Jordan Kuwait Laos)
         #(Japan Kiribati Kyrgyzstan))))

     ;; FOLD-RIGHT

     (test-case
      "fold-right:one-null-list"
      (check = (fold-right (lambda (alpha beta) (* alpha (+ beta 1))) 13 '())
              13))

     (test-case
      "fold-right:one-singleton-list"
      (check = (fold-right (lambda (alpha beta) (* alpha (+ beta 1))) 13 '(15))
              210))

     (test-case
      "fold-right:one-longer-list"
      (check = (fold-right (lambda (alpha beta) (* alpha (+ beta 1)))
                            13
                            '(15 17 19 21 23))
              32868750))

     (test-case
      "fold-right:several-null-lists"
      (check-eq? (fold-right vector 'Lebanon '() '() '() '() '())
                  'Lebanon))

     (test-case
      "fold-right:several-singleton-lists"
      (check-equal?
       (fold-right vector 'Lesotho '(Liberia) '(Libya) '(Liechtenstein)
                   '(Lithuania) '(Luxembourg))
       #(Liberia Libya Liechtenstein Lithuania Luxembourg Lesotho)))

     (test-case
      "fold-right:several-longer-lists"
      (check-equal?
       (fold-right (lambda (alpha beta gamma delta epsilon zeta)
                     (cons (vector alpha beta gamma delta epsilon) zeta))
                   '()
                   '(Macedonia Madagascar Malawi Malaysia Maldives Mali
                               Malta)
                   '(Mauritania Mauritius Mexico Micronesia Moldova Monaco
                                Mongolia)
                   '(Morocco Mozambique Myanmar Namibia Nauru Nepal
                             Netherlands)
                   '(Nicaragua Niger Nigeria Norway Oman Pakistan Palau)
                   '(Panama Paraguay Peru Philippines Poland Portugal
                            Qatar))

       '(#(Macedonia Mauritania Morocco Nicaragua Panama)
         #(Madagascar Mauritius Mozambique Niger Paraguay)
         #(Malawi Mexico Myanmar Nigeria Peru)
         #(Malaysia Micronesia Namibia Norway Philippines)
         #(Maldives Moldova Nauru Oman Poland)
         #(Mali Monaco Nepal Pakistan Portugal)
         #(Malta Mongolia Netherlands Palau Qatar))))

     (test-case
      "fold-right:lists-of-different-lengths"
      (check-equal?
       (fold-right (lambda (alpha beta gamma delta)
                     (cons (vector alpha beta gamma) delta))
                   '()
                   '(Romania Russia Rwanda Senegal)
                   '(Seychelles Singapore)
                   '(Slovakia Slovenia Somalia))
       '(#(Romania Seychelles Slovakia)
         #(Russia Singapore Slovenia))))

     ;; PAIR-FOLD

     (let* ((revappend (lambda (reversend base)
                         (do ((rest reversend (cdr rest))
                              (result base (cons (car rest) result)))
                             ((null? rest) result))))
            (revappall (lambda (first . rest)
                         (let loop ((first first) (rest rest))
                           (if (null? rest)
                               first
                               (revappend first
                                          (loop (car rest)
                                                (cdr rest))))))))
       (test-suite
        "Pair-fold tests"

        (test-case
         "pair-fold:one-null-list"
         (check-equal?
          (pair-fold revappend '(Spain Sudan) '())
          '(Spain Sudan)))

        (test-case
         "pair-fold:one-singleton-list"
         (check-equal?
          (pair-fold revappend '(Suriname Swaziland) '(Sweden))
          '(Sweden Suriname Swaziland)))

        (test-case
         "pair-fold:one-longer-list"
         (check-equal?
          (pair-fold revappend
                     '(Switzerland Syria)
                     '(Taiwan Tajikistan Tanzania Thailand Togo))
          '(Togo Togo Thailand Togo Thailand Tanzania Togo
                 Thailand Tanzania Tajikistan Togo Thailand
                 Tanzania Tajikistan Taiwan Switzerland Syria)))

        (test-case
         "pair-fold:several-null-lists"
         (check-equal?
          (pair-fold revappall '(Tonga Tunisia) '() '() '() '() '())
          '(Tonga Tunisia)))

        (test-case
         "pair-fold:several-singleton-lists"
         (check-equal?
          (pair-fold revappall
                     '(Turkey Turkmenistan)
                     '(Tuvalu)
                     '(Uganda)
                     '(Ukraine)
                     '(Uruguay)
                     '(Uzbekistan))
          '(Tuvalu Uganda Ukraine Uruguay Uzbekistan Turkey
                   Turkmenistan)))

        (test-case
         "pair-fold:several-longer-lists"
         (check-equal?
          (pair-fold revappall
                     '(Vanuatu Venezuela)
                     '(Vietnam Yemen Yugoslavia Zaire Zambia Zimbabwe
                               Agnon)
                     '(Aleixandre Andric Asturias Beckett Bellow
                                  Benavente Bergson)
                     '(Bjornson Brodsky Buck Bunin Camus Canetti
                                Carducci)
                     '(Cela Churchill Deledda Echegary Eliot Elytis
                            Eucken)
                     '(Faulkner Galsworthy Gide Gjellerup Golding
                                Gordimer Hamsun))
          '(Agnon Bergson Carducci Eucken Hamsun Agnon
                  Zimbabwe Bergson Benavente Carducci Canetti
                  Eucken Elytis Hamsun Gordimer Agnon Zimbabwe
                  Zambia Bergson Benavente Bellow Carducci Canetti
                  Camus Eucken Elytis Eliot Hamsun Gordimer
                  Golding Agnon Zimbabwe Zambia Zaire Bergson
                  Benavente Bellow Beckett Carducci Canetti Camus
                  Bunin Eucken Elytis Eliot Echegary Hamsun
                  Gordimer Golding Gjellerup Agnon Zimbabwe Zambia
                  Zaire Yugoslavia Bergson Benavente Bellow
                  Beckett Asturias Carducci Canetti Camus Bunin
                  Buck Eucken Elytis Eliot Echegary Deledda Hamsun
                  Gordimer Golding Gjellerup Gide Agnon Zimbabwe
                  Zambia Zaire Yugoslavia Yemen Bergson Benavente
                  Bellow Beckett Asturias Andric Carducci Canetti
                  Camus Bunin Buck Brodsky Eucken Elytis Eliot
                  Echegary Deledda Churchill Hamsun Gordimer
                  Golding Gjellerup Gide Galsworthy Agnon Zimbabwe
                  Zambia Zaire Yugoslavia Yemen Vietnam Bergson
                  Benavente Bellow Beckett Asturias Andric
                  Aleixandre Carducci Canetti Camus Bunin Buck
                  Brodsky Bjornson Eucken Elytis Eliot Echegary
                  Deledda Churchill Cela Hamsun Gordimer Golding
                  Gjellerup Gide Galsworthy Faulkner Vanuatu
                  Venezuela)))

        (test-case
         "pair-fold:lists-of-different-lengths"
         (check-equal?
          (pair-fold revappall
                     '(Hauptmann Hemingway Hesse)
                     '(Heyse Jensen Jimenez Johnson)
                     '(Karlfeldt Kawabata)
                     '(Kipling Lagerkvist Lagerlof Laxness Lewis))
          '(Johnson Jimenez Jensen Kawabata Lewis Laxness
                    Lagerlof Lagerkvist Johnson Jimenez Jensen Heyse
                    Kawabata Karlfeldt Lewis Laxness Lagerlof
                    Lagerkvist Kipling Hauptmann Hemingway
                    Hesse)))
        ))

     ;; PAIR-FOLD-RIGHT

     (let* ((revappend (lambda (reversend base)
                         (do ((rest reversend (cdr rest))
                              (result base (cons (car rest) result)))
                             ((null? rest) result))))
            (revappall (lambda (first . rest)
                         (let loop ((first first) (rest rest))
                           (if (null? rest)
                               first
                               (revappend first
                                          (loop (car rest)
                                                (cdr rest))))))))
       (test-suite
        "Pair-fold-right tests"
        (test-case
         "pair-fold-right:one-null-list"
         (check-equal?
          (pair-fold-right revappend '(Maeterlinck Mahfouz) '())
          '(Maeterlinck Mahfouz)))

        (test-case
         "pair-fold-right:one-singleton-list"
         (check-equal?
          (pair-fold-right revappend '(Mann Martinson) '(Mauriac))
          '(Mauriac Mann Martinson)))

        (test-case
         "pair-fold-right:one-longer-list"
         (check-equal?
          (pair-fold-right revappend
                           '(Milosz Mistral)
                           '(Mommsen Montale Morrison Neruda Oe))
          '(Oe Neruda Morrison Montale Mommsen Oe Neruda
               Morrison Montale Oe Neruda Morrison Oe Neruda Oe
               Milosz Mistral)))

        (test-case
         "pair-fold-right:several-null-lists"
         (check-equal?
          (pair-fold-right revappall '(Pasternak Paz) '() '() '() '() '())
          '(Pasternak Paz)))

        (test-case
         "pair-fold-right:several-singleton-lists"
         (check-equal?
          (pair-fold-right revappall
                           '(Perse Pirandello)
                           '(Pontoppidan)
                           '(Quasimodo)
                           '(Reymont)
                           '(Rolland)
                           '(Russell))
          '(Pontoppidan Quasimodo Reymont Rolland Russell
                        Perse Pirandello)))

        (test-case
         "pair-fold-right:several-longer-lists"
         (check-equal?
          (pair-fold-right revappall
                           '(Sachs Sartre)
                           '(Seferis Shaw Sholokov Siefert Sienkiewicz
                                     Sillanpaa Simon)
                           '(Singer Solzhenitsyn Soyinka Spitteler
                                    Steinbeck Tagore Undset)
                           '(Walcott White Yeats Anderson Andrews Angelina
                                     Aransas)
                           '(Archer Armstrong Alascosa Austin Bailey
                                    Bandera Bastrop)
                           '(Baylor Bee Bell Bexar Blanco Borden Bosque
                                    Bowie))
          '(Simon Sillanpaa Sienkiewicz Siefert Sholokov
                  Shaw Seferis Undset Tagore Steinbeck Spitteler
                  Soyinka Solzhenitsyn Singer Aransas Angelina
                  Andrews Anderson Yeats White Walcott Bastrop
                  Bandera Bailey Austin Alascosa Armstrong Archer
                  Bowie Bosque Borden Blanco Bexar Bell Bee Baylor
                  Simon Sillanpaa Sienkiewicz Siefert Sholokov
                  Shaw Undset Tagore Steinbeck Spitteler Soyinka
                  Solzhenitsyn Aransas Angelina Andrews Anderson
                  Yeats White Bastrop Bandera Bailey Austin
                  Alascosa Armstrong Bowie Bosque Borden Blanco
                  Bexar Bell Bee Simon Sillanpaa Sienkiewicz
                  Siefert Sholokov Undset Tagore Steinbeck
                  Spitteler Soyinka Aransas Angelina Andrews
                  Anderson Yeats Bastrop Bandera Bailey Austin
                  Alascosa Bowie Bosque Borden Blanco Bexar Bell
                  Simon Sillanpaa Sienkiewicz Siefert Undset
                  Tagore Steinbeck Spitteler Aransas Angelina
                  Andrews Anderson Bastrop Bandera Bailey Austin
                  Bowie Bosque Borden Blanco Bexar Simon Sillanpaa
                  Sienkiewicz Undset Tagore Steinbeck Aransas
                  Angelina Andrews Bastrop Bandera Bailey Bowie
                  Bosque Borden Blanco Simon Sillanpaa Undset
                  Tagore Aransas Angelina Bastrop Bandera Bowie
                  Bosque Borden Simon Undset Aransas Bastrop Bowie
                  Bosque Sachs Sartre)))

        (test-case
         "pair-fold-right:lists-of-different-lengths"
         (check-equal?
          (pair-fold-right revappall
                           '(Brazoria Brazos Brewster)
                           '(Briscoe Brooks Brown Burleson)
                           '(Burnet Caldwell)
                           '(Calhoun Callahan Cameron Camp Carson))
          '(Burleson Brown Brooks Briscoe Caldwell Burnet
                     Carson Camp Cameron Callahan Calhoun Burleson
                     Brown Brooks Caldwell Carson Camp Cameron
                     Callahan Brazoria Brazos Brewster)))
        ))

     ;; REDUCE

     (test-case
      "reduce:null-list"
      (check-true (zero? (reduce (lambda (alpha beta) (* alpha (+ beta 1))) 0 '()))))

     (test-case
      "reduce:singleton-list"
      (check = (reduce (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(25)) 25))

     (test-case
      "reduce:doubleton-list"
      (check =
              (reduce (lambda (alpha beta) (* alpha (+ beta 1)))
                      0
                      '(27 29))
              812))

     (test-case
      "reduce:longer-list"
      (check =
              (reduce (lambda (alpha beta) (* alpha (+ beta 1)))
                      0
                      '(31 33 35 37 39 41 43))
              94118227527))

     ;; REDUCE-RIGHT

     (test-case
      "reduce-right:null-list"
      (check-true (zero? (reduce-right (lambda (alpha beta) (* alpha (+ beta 1))) 0 '()))))

     (test-case
      "reduce-right:singleton-list"
      (check =
              (reduce-right (lambda (alpha beta) (* alpha (+ beta 1))) 0 '(25))
              25))

     (test-case
      "reduce-right:doubleton-list"
      (check =
              (reduce-right (lambda (alpha beta) (* alpha (+ beta 1)))
                            0
                            '(27 29))
              810))

     (test-case
      "reduce-right:longer-list"
      (check =
              (reduce-right (lambda (alpha beta) (* alpha (+ beta 1)))
                            0
                            '(31 33 35 37 39 41 43))
              93259601719))

     ;; APPEND-MAP

     (test-case
      "append-map:one-null-list"
      (check-true (null? (append-map (lambda (element) (list element element)) '()))))

     (test-case
      "append-map:one-singleton-list"
      (check-equal? (append-map (lambda (element) (list element element)) '(Cass))
                     '(Cass Cass)))

     (test-case
      "append-map:one-longer-list"
      (check-equal? (append-map (lambda (element) (list element element))
                                 '(Castro Chambers Cherokee Childress Clay))
                     '(Castro Castro Chambers Chambers Cherokee Cherokee
                              Childress Childress Clay Clay)))

     (test-case
      "append-map:several-null-lists"
      (check-true (null? (append-map (lambda elements (reverse elements))
                                      '() '() '() '() '()))))

     (test-case
      "append-map:several-singleton-lists"
      (check-equal? (append-map (lambda elements (reverse elements))
                                 '(Cochran)
                                 '(Coke)
                                 '(Coleman)
                                 '(Collin)
                                 '(Collingsworth))
                     '(Collingsworth Collin Coleman Coke Cochran)))

     (test-case
      "append-map:several-longer-lists"
      (check-equal?
       (append-map (lambda elements (reverse elements))
                   '(Colorado Comal Comanche Concho Cooke Coryell
                              Cottle)
                   '(Crane Crockett Crosby Culberson Dallam Dallas
                           Dawson)
                   '(Delta Denton Dewitt Dickens Dimmit Donley Duval)
                   '(Eastland Ector Edwards Ellis Erath Falls Fannin)
                   '(Fayette Fisher Floyd Foard Franklin Freestone
                             Frio))
       '(Fayette Eastland Delta Crane Colorado Fisher Ector
                 Denton Crockett Comal Floyd Edwards Dewitt Crosby
                 Comanche Foard Ellis Dickens Culberson Concho
                 Franklin Erath Dimmit Dallam Cooke Freestone Falls
                 Donley Dallas Coryell Frio Fannin Duval Dawson
                 Cottle)))

     ;; APPEND-MAP!

     (test-case
      "append-map!:one-null-list"
      (check-true (null? (append-map! (lambda (element) (list element element))
                                       (list)))))

     (test-case
      "append-map!:one-singleton-list"
      (check-equal?
       (append-map! (lambda (element) (list element element))
                    (list 'Gaines))
       '(Gaines Gaines)))

     (test-case
      "append-map!:one-longer-list"
      (check-equal?
       (append-map! (lambda (element) (list element element))
                    (list 'Galveston 'Garza 'Gillespie 'Glasscock
                          'Goliad))
       '(Galveston Galveston Garza Garza Gillespie
                   Gillespie Glasscock Glasscock Goliad Goliad)))

     (test-case
      "append-map!:several-null-lists"
      (check-true (null? (append-map! (lambda elements (reverse elements))
                                       (list)
                                       (list)
                                       (list)
                                       (list)
                                       (list)))))

     (test-case
      "append-map!:several-singleton-lists"
      (check-equal?
       (append-map! (lambda elements (reverse elements))
                    (list 'Gonzales)
                    (list 'Gray)
                    (list 'Grayson)
                    (list 'Gregg)
                    (list 'Grimes))
       '(Grimes Gregg Grayson Gray Gonzales)))

     (test-case
      "append-map!:several-longer-lists"
      (check-equal?
       (append-map! (lambda elements (reverse elements))
                    (list 'Guadalupe 'Hale 'Hall 'Hamilton 'Hansford
                          'Hardeman 'Hardin)
                    (list 'Harris 'Harrison 'Hartley 'Haskell 'Hays
                          'Hemphill 'Henderson)
                    (list 'Hidalgo 'Hill 'Hockley 'Hood 'Hopkins
                          'Houston 'Howard)
                    (list 'Hudspeth 'Hunt 'Hutchinson 'Irion 'Jack
                          'Jackson 'Jasper)
                    (list 'Jefferson 'Johnson 'Jones 'Karnes 'Kaufman
                          'Kendall 'Kenedy))
       '(Jefferson Hudspeth Hidalgo Harris Guadalupe
                   Johnson Hunt Hill Harrison Hale Jones Hutchinson
                   Hockley Hartley Hall Karnes Irion Hood Haskell
                   Hamilton Kaufman Jack Hopkins Hays Hansford
                   Kendall Jackson Houston Hemphill Hardeman Kenedy
                   Jasper Howard Henderson Hardin)))

     ;; MAP!

     (test-case
      "map!:one-null-list"
      (check-true (null? (map! vector (list)))))

     (test-case
      "map!:one-singleton-list"
      (check-equal?  (map! vector (list 'Kent))
                      '(#(Kent))))

     (test-case
      "map!:one-longer-list"
      (check-equal?
       (map! vector (list 'Kerr 'Kimble 'King 'Kinney 'Kleberg))
       '(#(Kerr) #(Kimble) #(King) #(Kinney) #(Kleberg))))

     (test-case
      "map!:several-null-lists"
      (check-true (null? (map! vector (list) (list) (list) (list) (list)))))

     (test-case
      "map!:several-singleton-lists"
      (check-equal?
       (map! vector
             (list 'Knox)
             (list 'Lamar)
             (list 'Lamb)
             (list 'Lampasas)
             (list 'Lavaca))
       '(#(Knox Lamar Lamb Lampasas Lavaca))))

     (test-case
      "map!:several-longer-lists"
      (check-equal?
       (map! vector
             (list 'Lee 'Leon 'Liberty 'Limestone 'Lipscomb 'Llano
                   'Loving)
             (list 'Lubbock 'Lynn 'McCulloch 'McLennan 'McMullen
                   'Madison 'Marion)
             (list 'Martin 'Mason 'Matagorda 'Maverick 'Medina
                   'Menard 'Midland)
             (list 'Milam 'Mills 'Mitchell 'Montague 'Montgomery
                   'Moore 'Morris)
             (list 'Motley 'Nacogdoches 'Navarro 'Newton 'Nolan
                   'Nueces 'Ochiltree))
       '(#(Lee Lubbock Martin Milam Motley)
         #(Leon Lynn Mason Mills Nacogdoches)
         #(Liberty McCulloch Matagorda Mitchell Navarro)
         #(Limestone McLennan Maverick Montague Newton)
         #(Lipscomb McMullen Medina Montgomery Nolan)
         #(Llano Madison Menard Moore Nueces)
         #(Loving Marion Midland Morris Ochiltree))))

     ;; MAP-IN-ORDER

     (test-case
      "map-in-order:one-null-list"
      (check-true (null? (let ((counter 0))
                            (map-in-order (lambda (element)
                                            (set! counter (+ counter 1))
                                            (cons counter element))
                                          '())))))

     (test-case
      "map-in-order:one-singleton-list"
      (check-equal?
       (let ((counter 0))
         (map-in-order (lambda (element)
                         (set! counter (+ counter 1))
                         (cons counter element))
                       '(Oldham)))
       '((1 . Oldham))))

     (test-case
      "map-in-order:one-longer-list"
      (check-equal?
       (let ((counter 0))
         (map-in-order (lambda (element)
                         (set! counter (+ counter 1))
                         (cons counter element))
                       '(Orange Panola Parker Parmer Pecos)))
       '((1 . Orange)
         (2 . Panola)
         (3 . Parker)
         (4 . Parmer)
         (5 . Pecos))))

     (test-case
      "map-in-order:several-null-lists"
      (check-true (null? (let ((counter 0))
                            (map-in-order (lambda elements
                                            (set! counter (+ counter 1))
                                            (apply vector counter elements))
                                          '() '() '() '() '())))))

     (test-case
      "map-in-order:several-singleton-lists"
      (check-equal?
       (let ((counter 0))
         (map-in-order (lambda elements
                         (set! counter (+ counter 1))
                         (apply vector counter elements))
                       '(Polk)
                       '(Potter)
                       '(Presidio)
                       '(Rains)
                       '(Randall)))
       '(#(1 Polk Potter Presidio Rains Randall))))

     (test-case
      "map-in-order:several-longer-lists"
      (check-equal?
       (let ((counter 0))
         (map-in-order (lambda elements
                         (set! counter (+ counter 1))
                         (apply vector counter elements))
                       '(Reagan Real Reeves Refugio Roberts Robertson
                                Rockwall)
                       '(Runnels Rusk Sabine Schleicher Scurry
                                 Shackelford Shelby)
                       '(Sherman Smith Somervell Starr Stephens
                                 Sterling Stonewall)
                       '(Sutton Swisher Tarrant Taylor Terrell Terry
                                Throckmorton)
                       '(Titus Travis Trinity Tyler Upshur Upton
                               Uvalde)))
       '(#(1 Reagan Runnels Sherman Sutton Titus)
         #(2 Real Rusk Smith Swisher Travis)
         #(3 Reeves Sabine Somervell Tarrant Trinity)
         #(4 Refugio Schleicher Starr Taylor Tyler)
         #(5 Roberts Scurry Stephens Terrell Upshur)
         #(6 Robertson Shackelford Sterling Terry Upton)
         #(7 Rockwall Shelby Stonewall Throckmorton
             Uvalde))))

     ;; PAIR-FOR-EACH

     (test-case
      "pair-for-each:one-null-list"
      (check-true
       (null? (let ((base '()))
                (pair-for-each (lambda (tail)
                                 (set! base (append tail base)))
                               '())
                base))))

     (test-case
      "pair-for-each:one-singleton-list"
      (check-equal?
       (let ((base '()))
         (pair-for-each (lambda (tail)
                          (set! base (append tail base)))
                        '(Victoria))
         base)
       '(Victoria)))

     (test-case
      "pair-for-each:one-longer-list"
      (check-equal?
       (let ((base '()))
         (pair-for-each (lambda (tail)
                          (set! base (append tail base)))
                        '(Walker Waller Ward Washington Webb))
         base)
       '(Webb Washington Webb Ward Washington Webb Waller
              Ward Washington Webb Walker Waller Ward
              Washington Webb)))

     (test-case
      "pair-for-each:several-null-lists"
      (check-true
       (null? (let ((base '()))
                (pair-for-each (lambda tails
                                 (set! base
                                       (cons (apply vector tails) base)))
                               '() '() '() '() '())
                base))))

     (test-case
      "pair-for-each:several-singleton-lists"
      (check-equal?
       (let ((base '()))
         (pair-for-each (lambda tails
                          (set! base
                                (cons (apply vector tails) base)))
                        '(Wharton)
                        '(Wheeler)
                        '(Wichita)
                        '(Wilbarger)
                        '(Willacy))
         base)
       '(#((Wharton) (Wheeler) (Wichita) (Wilbarger)
           (Willacy)))))

     (test-case
      "pair-for-each:several-longer-lists"
      (check-equal?
       (let ((base '()))
         (pair-for-each (lambda tails
                          (set! base
                                (cons (apply vector tails) base)))
                        '(Williamson Wilson Winkler Wise Wood Yoakum
                                     Young)
                        '(Zapata Zavala Admiral Advil Ajax Anacin
                                 Arrid)
                        '(Arnold Ban Barbie Beech Blockbuster Bounce
                                 Breck)
                        '(Budweiser Bufferin BVD Carrier Celeste
                                    Charmin Cheer)
                        '(Cheerios Cinemax Clairol Clorets Combat
                                   Comet Coppertone))
         base)
       '(#((Young) (Arrid) (Breck) (Cheer) (Coppertone))
         #((Yoakum Young) (Anacin Arrid) (Bounce Breck)
           (Charmin Cheer) (Comet Coppertone))
         #((Wood Yoakum Young)
           (Ajax Anacin Arrid)
           (Blockbuster Bounce Breck)
           (Celeste Charmin Cheer)
           (Combat Comet Coppertone))
         #((Wise Wood Yoakum Young)
           (Advil Ajax Anacin Arrid)
           (Beech Blockbuster Bounce Breck)
           (Carrier Celeste Charmin Cheer)
           (Clorets Combat Comet Coppertone))
         #((Winkler Wise Wood Yoakum Young)
           (Admiral Advil Ajax Anacin Arrid)
           (Barbie Beech Blockbuster Bounce Breck)
           (BVD Carrier Celeste Charmin Cheer)
           (Clairol Clorets Combat Comet Coppertone))
         #((Wilson Winkler Wise Wood Yoakum Young)
           (Zavala Admiral Advil Ajax Anacin Arrid)
           (Ban Barbie Beech Blockbuster Bounce Breck)
           (Bufferin BVD Carrier Celeste Charmin Cheer)
           (Cinemax Clairol Clorets Combat Comet
                    Coppertone))
         #((Williamson Wilson Winkler Wise Wood Yoakum
                       Young)
           (Zapata Zavala Admiral Advil Ajax Anacin Arrid)
           (Arnold Ban Barbie Beech Blockbuster Bounce
                   Breck)
           (Budweiser Bufferin BVD Carrier Celeste Charmin
                      Cheer)
           (Cheerios Cinemax Clairol Clorets Combat Comet
                     Coppertone)))))

     ;; FILTER-MAP

     (test-case
      "filter-map:one-null-list"
      (check-true (null? (filter-map values '()))))

     (test-case
      "filter-map:one-singleton-list"
      (check-equal?
       (filter-map values '(Crest))
       '(Crest)))

     (test-case
      "filter-map:one-list-all-elements-removed"
      (check-true
       (null? (filter-map (lambda (x) #f)
                          '(Crisco Degree Doritos Dristan Efferdent)))))

     (test-case
      "filter-map:one-list-some-elements-removed"
      (check-equal?
       (filter-map (lambda (n) (and (even? n) n))
                   '(44 45 46 47 48 49 50))
       '(44 46 48 50)))

     (test-case
      "filter-map:one-list-no-elements-removed"
      (check-equal?
       (filter-map values '(ESPN Everready Excedrin Fab Fantastik))
       '(ESPN Everready Excedrin Fab Fantastik)))

     (test-case
      "filter-map:several-null-lists"
      (check-true (null? (filter-map vector '() '() '() '() '()))))

     (test-case
      "filter-map:several-singleton-lists"
      (check-equal?
       (filter-map vector
                   '(Foamy)
                   '(Gatorade)
                   '(Glad)
                   '(Gleem)
                   '(Halcion))
       '(#(Foamy Gatorade Glad Gleem Halcion))))

     (test-case
      "filter-map:several-lists-all-elements-removed"
      (check-true
       (null?
        (filter-map (lambda arguments #f)
                    '(Hanes HBO Hostess Huggies Ivory Kent Kinney)
                    '(Kleenex Knorr Lee Lenox Lerner Listerine
                              Marlboro)
                    '(Mazola Michelob Midas Miller NBC Newsweek
                             Noxema)
                    '(NutraSweet Oreo Pampers People Planters
                                 Playskool Playtex)
                    '(Prego Prell Prozac Purex Ritz Robitussin
                            Rolaids)))))

     (test-case
      "filter-map:several-lists-some-elements-removed"
      (check-equal?
       (filter-map (lambda arguments
                     (let ((sum (apply + arguments)))
                       (and (odd? sum) sum)))
                   '(51 52 53 54 55 56 57)
                   '(58 59 60 61 62 63 64)
                   '(65 66 67 68 69 70 71)
                   '(72 73 74 75 76 77 78)
                   '(79 80 81 82 83 84 85))
       '(325 335 345 355)))

     (test-case
      "filter-map:several-lists-no-elements-removed"
      (check-equal?
       (filter-map vector
                   '(Ronzoni Ruffles Scotch Skippy SnackWell Snapple
                             Spam)
                   '(Sprite Swanson Thomas Tide Tonka Trojan
                            Tupperware)
                   '(Tylenol Velveeta Vicks Victory Visine Wheaties
                             Wise)
                   '(Wonder Ziploc Abbott Abingdon Ackley Ackworth
                            Adair)
                   '(Adams Adaville Adaza Adel Adelphi Adena Afton))
       '(#(Ronzoni Sprite Tylenol Wonder Adams)
         #(Ruffles Swanson Velveeta Ziploc Adaville)
         #(Scotch Thomas Vicks Abbott Adaza)
         #(Skippy Tide Victory Abingdon Adel)
         #(SnackWell Tonka Visine Ackley Adelphi)
         #(Snapple Trojan Wheaties Ackworth Adena)
         #(Spam Tupperware Wise Adair Afton))))

     ))
  )
;;; fold-test.rkt ends here
