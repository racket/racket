;;;
;;; <delete-test.ss> ---- List deletion function tests
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

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (all-except (lib "delete.ss" "srfi" "1") member))

  (provide delete-tests)

  (define delete-tests
    (make-test-suite
     "List deletion tests"

     ;; DELETE

     (make-test-case
      "delete:null-list"
      (assert-true
       (null? (delete '(Fraser . Frederic) '()))))

     (make-test-case
      "delete:in-singleton-list"
      (assert-true
       (null?
        (delete '(Fredericksburg . Frederika)
                '((Fredericksburg . Frederika))))))

     (make-test-case
      "delete:not-in-singleton-list"
      (assert-equal?
       (delete '(Fredonia . Fredsville) '((Freeman . Freeport)))
       '((Freeman . Freeport))))

     (make-test-case
      "delete:at-beginning-of-longer-list"
      (assert-equal?
       (delete '(Fremont . Froelich) '((Fremont . Froelich)
                                       (Fruitland . Fulton)
                                       (Furay . Galbraith)
                                       (Galesburg . Galland)
                                       (Galt . Galva)))
       '((Fruitland . Fulton)
         (Furay . Galbraith)
         (Galesburg . Galland)
         (Galt . Galva))))

     (make-test-case
      "delete:in-middle-of-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete:at-end-of-longer-list"
      (assert-equal?
       (delete '(Georgetown . Gerled) '((Germantown . Germanville)
                                        (Giard . Gibbsville)
                                        (Gibson . Gifford)
                                        (Gilbert . Gilbertville)
                                        (Georgetown . Gerled)))
       '((Germantown . Germanville)
         (Giard . Gibbsville)
         (Gibson . Gifford)
         (Gilbert . Gilbertville))))

     (make-test-case
      "delete:not-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete:several-matches-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete!:null-list"
      (assert-true (null? (delete! (cons 'Henshaw 'Hentons) (list)))))

     (make-test-case
      "delete!:in-singleton-list"
      (assert-true
       (null?
        (delete! (cons 'Hepburn 'Herndon)
                 (list (cons 'Hepburn 'Herndon))))))

     (make-test-case
      "delete!:not-in-singleton-list"
      (assert-equal?
       (delete! (cons 'Hesper 'Hiattsville)
                (list (cons 'Hiawatha 'Hicks)))
       '((Hiawatha . Hicks))))

     (make-test-case
      "delete!:at-beginning-of-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete!:in-middle-of-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete!:at-end-of-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete!:not-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete!:several-matches-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete-duplicates:null-list"
      (assert-true (null? (delete-duplicates '()))))

     (make-test-case
      "delete-duplicates:singleton-list"
      (assert-equal?
       (delete-duplicates '((Knierim . Knittel)))
       '((Knierim . Knittel))))

     (make-test-case
      "delete-duplicates:in-doubleton-list"
      (assert-equal?
       (delete-duplicates '((Knoke . Knowlton) (Knoke . Knowlton)))
       '((Knoke . Knowlton))))

     (make-test-case
      "delete-duplicates:none-removed-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete-duplicates:some-removed-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete-duplicates:all-but-one-removed-in-longer-list"
      (assert-equal?
       (delete-duplicates '((Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)
                            (Lanesboro . Langdon)))
       '((Lanesboro . Langdon))))

     ;; DELETE-DUPLICATES!

     (make-test-case
      "delete-duplicates!:null-list"
      (assert-true (null? (delete-duplicates! (list)))))

     (make-test-case
      "delete-duplicates!:singleton-list"
      (assert-equal?
       (delete-duplicates! (list (cons 'Lester 'Letts)))
       '((Lester . Letts))))

     (make-test-case
      "delete-duplicates!:in-doubleton-list"
      (assert-equal?
       (delete-duplicates! (list (cons 'Leverette 'Levey)
                                 (cons 'Leverette 'Levey)))
       '((Leverette . Levey))))

     (make-test-case
      "delete-duplicates!:none-removed-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete-duplicates!:some-removed-in-longer-list"
      (assert-equal?
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

     (make-test-case
      "delete-duplicates!:all-but-one-removed-in-longer-list"
      (assert-equal?
       (delete-duplicates! (list (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)
                                 (cons 'Logansport 'Lohrville)))
       '((Logansport . Lohrville))))
     ))

  )
;;; delete-test.ss ends here
