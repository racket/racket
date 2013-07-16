;;;
;;; <delete-test.rkt> ---- List deletion function tests
;;; Time-stamp: <05/12/16 21:16:28 noel>
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

(module delete-test
  mzscheme
  (require rackunit)
  (require (all-except srfi/1/delete member))

  (provide delete-tests)

  (define delete-tests
    (test-suite
     "List deletion tests"

     ;; DELETE

     (test-case
      "delete:null-list"
      (check-true
       (null? (delete '(Fraser . Frederic) '()))))

     (test-case
      "delete:in-singleton-list"
      (check-true
       (null?
        (delete '(Fredericksburg . Frederika)
                '((Fredericksburg . Frederika))))))

     (test-case
      "delete:not-in-singleton-list"
      (check-equal?
       (delete '(Fredonia . Fredsville) '((Freeman . Freeport)))
       '((Freeman . Freeport))))

     (test-case
      "delete:at-beginning-of-longer-list"
      (check-equal?
       (delete '(Fremont . Froelich) '((Fremont . Froelich)
                                       (Fruitland . Fulton)
                                       (Furay . Galbraith)
                                       (Galesburg . Galland)
                                       (Galt . Galva)))
       '((Fruitland . Fulton)
         (Furay . Galbraith)
         (Galesburg . Galland)
         (Galt . Galva))))

     (test-case
      "delete:in-middle-of-longer-list"
      (check-equal?
       (delete '(Gambrill . Garber) '((Gardiner . Gardner)
                                      (Garfield . Garland)
                                      (Garnavillo . Garner)
                                      (Garrison . Garwin)
                                      (Gambrill . Garber)
                                      (Gaza . Geneva)
                                      (Genoa . George)))
       '((Gardiner . Gardner)
         (Garfield . Garland)
         (Garnavillo . Garner)
         (Garrison . Garwin)
         (Gaza . Geneva)
         (Genoa . George))))

     (test-case
      "delete:at-end-of-longer-list"
      (check-equal?
       (delete '(Georgetown . Gerled) '((Germantown . Germanville)
                                        (Giard . Gibbsville)
                                        (Gibson . Gifford)
                                        (Gilbert . Gilbertville)
                                        (Georgetown . Gerled)))
       '((Germantown . Germanville)
         (Giard . Gibbsville)
         (Gibson . Gifford)
         (Gilbert . Gilbertville))))

     (test-case
      "delete:not-in-longer-list"
      (check-equal?
       (delete '(Gilliatt . Gilman) '((Givin . Gladbrook)
                                      (Gladstone . Gladwin)
                                      (Glasgow . Glendon)
                                      (Glenwood . Glidden)
                                      (Goddard . Goldfield)))
       '((Givin . Gladbrook)
         (Gladstone . Gladwin)
         (Glasgow . Glendon)
         (Glenwood . Glidden)
         (Goddard . Goldfield))))

     (test-case
      "delete:several-matches-in-longer-list"
      (check-equal?
       (delete '(Goodell . Gosport) '((Gowrie . Goddard)
                                      (Grable . Graettinger)
                                      (Goodell . Gosport)
                                      (Graf . Grafton)
                                      (Goodell . Gosport)
                                      (Grandview . Granger)
                                      (Goodell . Gosport)))
       '((Gowrie . Goddard)
         (Grable . Graettinger)
         (Graf . Grafton)
         (Grandview . Granger))))

     ;; DELETE!

     (test-case
      "delete!:null-list"
      (check-true (null? (delete! (cons 'Henshaw 'Hentons) (list)))))

     (test-case
      "delete!:in-singleton-list"
      (check-true
       (null?
        (delete! (cons 'Hepburn 'Herndon)
                 (list (cons 'Hepburn 'Herndon))))))

     (test-case
      "delete!:not-in-singleton-list"
      (check-equal?
       (delete! (cons 'Hesper 'Hiattsville)
                (list (cons 'Hiawatha 'Hicks)))
       '((Hiawatha . Hicks))))

     (test-case
      "delete!:at-beginning-of-longer-list"
      (check-equal?
       (delete! (cons 'Highland 'Highlandville)
                (list (cons 'Highland 'Highlandville)
                      (cons 'Highview 'Hills)
                      (cons 'Hillsboro 'Hillsdale)
                      (cons 'Hilltop 'Hinton)
                      (cons 'Hiteman 'Hobarton)))
       '((Highview . Hills)
         (Hillsboro . Hillsdale)
         (Hilltop . Hinton)
         (Hiteman . Hobarton))))

     (test-case
      "delete!:in-middle-of-longer-list"
      (check-equal?
       (delete! (cons 'Hocking 'Holbrook)
                (list (cons 'Holland 'Holmes)
                      (cons 'Holstein 'Homer)
                      (cons 'Homestead 'Hopeville)
                      (cons 'Hopkinton 'Hornick)
                      (cons 'Hocking 'Holbrook)
                      (cons 'Horton 'Hospers)
                      (cons 'Houghton 'Howardville)))
       '((Holland . Holmes)
         (Holstein . Homer)
         (Homestead . Hopeville)
         (Hopkinton . Hornick)
         (Horton . Hospers)
         (Houghton . Howardville))))

     (test-case
      "delete!:at-end-of-longer-list"
      (check-equal?
       (delete! (cons 'Howe 'Hubbard)
                (list (cons 'Hudson 'Hugo)
                      (cons 'Hull 'Humboldt)
                      (cons 'Humeston 'Huntington)
                      (cons 'Hurley 'Huron)
                      (cons 'Howe 'Hubbard)))
       '((Hudson . Hugo)
         (Hull . Humboldt)
         (Humeston . Huntington)
         (Hurley . Huron))))

     (test-case
      "delete!:not-in-longer-list"
      (check-equal?
       (delete! (cons 'Hurstville 'Hutchins)
                (list (cons 'Huxley 'Iconium)
                      (cons 'Illyria 'Imogene)
                      (cons 'Independence 'Indianapolis)
                      (cons 'Indianola 'Industry)
                      (cons 'Inwood 'Ion)))
       '((Huxley . Iconium)
         (Illyria . Imogene)
         (Independence . Indianapolis)
         (Indianola . Industry)
         (Inwood . Ion))))

     (test-case
      "delete!:several-matches-in-longer-list"
      (check-equal?
       (delete! (cons 'Ionia 'Ira)
                (list (cons 'Ireton 'Ironhills)
                      (cons 'Irving 'Irvington)
                      (cons 'Ionia 'Ira)
                      (cons 'Irwin 'Ivester)
                      (cons 'Ionia 'Ira)
                      (cons 'Iveyville 'Ivy)
                      (cons 'Ionia 'Ira)))
       '((Ireton . Ironhills)
         (Irving . Irvington)
         (Irwin . Ivester)
         (Iveyville . Ivy))))

     ;; DELETE-DUPLICATES

     (test-case
      "delete-duplicates:null-list"
      (check-true (null? (delete-duplicates '()))))

     (test-case
      "delete-duplicates:singleton-list"
      (check-equal?
       (delete-duplicates '((Knierim . Knittel)))
       '((Knierim . Knittel))))

     (test-case
      "delete-duplicates:in-doubleton-list"
      (check-equal?
       (delete-duplicates '((Knoke . Knowlton) (Knoke . Knowlton)))
       '((Knoke . Knowlton))))

     (test-case
      "delete-duplicates:none-removed-in-longer-list"
      (check-equal?
       (delete-duplicates '((Knox . Knoxville)
                            (Konigsmark . Kossuth)
                            (Koszta . Lacelle)
                            (Lacey . Lacona)
                            (Ladoga . Ladora)))
       '((Knox . Knoxville)
         (Konigsmark . Kossuth)
         (Koszta . Lacelle)
         (Lacey . Lacona)
         (Ladoga . Ladora))))

     (test-case
      "delete-duplicates:some-removed-in-longer-list"
      (check-equal?
       (delete-duplicates '((Lafayette . Lainsville)
                            (Lakeside . Lakewood)
                            (Lakeside . Lakewood)
                            (Lakonta . Lakota)
                            (Lafayette . Lainsville)
                            (Lamoille . Lamoni)
                            (Lakeside . Lakewood)
                            (Lamont . Lancaster)
                            (Lakeside . Lakewood)))
       '((Lafayette . Lainsville)
         (Lakeside . Lakewood)
         (Lakonta . Lakota)
         (Lamoille . Lamoni)
         (Lamont . Lancaster))))

     (test-case
      "delete-duplicates:all-but-one-removed-in-longer-list"
      (check-equal?
       (delete-duplicates '((Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)))
       '((Lanesboro . Langdon))))

     ;; DELETE-DUPLICATES!

     (test-case
      "delete-duplicates!:null-list"
      (check-true (null? (delete-duplicates! (list)))))

     (test-case
      "delete-duplicates!:singleton-list"
      (check-equal?
       (delete-duplicates! (list (cons 'Lester 'Letts)))
       '((Lester . Letts))))

     (test-case
      "delete-duplicates!:in-doubleton-list"
      (check-equal?
       (delete-duplicates! (list (cons 'Leverette 'Levey)
                                 (cons 'Leverette 'Levey)))
       '((Leverette . Levey))))

     (test-case
      "delete-duplicates!:none-removed-in-longer-list"
      (check-equal?
       (delete-duplicates! (list (cons 'Lewis 'Lexington)
                                 (cons 'Liberty 'Libertyville)
                                 (cons 'Lidderdale 'Lima)
                                 (cons 'Linby 'Lincoln)
                                 (cons 'Linden 'Lineville)))
       '((Lewis . Lexington)
         (Liberty . Libertyville)
         (Lidderdale . Lima)
         (Linby . Lincoln)
         (Linden . Lineville))))

     (test-case
      "delete-duplicates!:some-removed-in-longer-list"
      (check-equal?
       (delete-duplicates! (list (cons 'Lisbon 'Liscomb)
                                 (cons 'Littleport 'Littleton)
                                 (cons 'Littleport 'Littleton)
                                 (cons 'Livermore 'Livingston)
                                 (cons 'Lisbon 'Liscomb)
                                 (cons 'Lockman 'Lockridge)
                                 (cons 'Littleport 'Littleton)
                                 (cons 'Locust 'Logan)
                                 (cons 'Littleport 'Littleton)))
       '((Lisbon . Liscomb)
         (Littleport . Littleton)
         (Livermore . Livingston)
         (Lockman . Lockridge)
         (Locust . Logan))))

     (test-case
      "delete-duplicates!:all-but-one-removed-in-longer-list"
      (check-equal?
       (delete-duplicates! (list (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)))
       '((Logansport . Lohrville))))
     ))

  )
;;; delete-test.rkt ends here
