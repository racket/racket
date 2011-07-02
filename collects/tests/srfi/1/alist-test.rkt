;;;
;;; <alist-test.rkt> ---- Association list tests
;;; Time-stamp: <2008-03-07 16:36:15 nhw>
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

(module alist-test mzscheme
  (require rackunit)
  (require (all-except srfi/1/alist assoc)
           (rename srfi/1/alist s:assoc assoc))

  (provide alist-tests)

  (define alist-tests
    (test-suite
     "Association list tests"

     ;; ALIST-CONS

     (test-case
      "alist-cons:null-list"
      (check-equal? (alist-cons 'Manawa 'Manchester '())
                     '((Manawa . Manchester))))

     (test-case
      "alist-cons:singleton-list"
      (let* ((base '((Manilla . Manly)))
             (result (alist-cons 'Manning 'Manson base)))
        (check-equal? result '((Manning . Manson)
                                (Manilla . Manly)))
        (check-eq? (cdr result) base)))

     (test-case
      "alist-cons:longer-list"
      (let* ((base '((Manteno . Mapleside)
                     (Mapleton . Maquoketa)
                     (Marathon . Marcus)
                     (Marengo . Marietta)
                     (Marion . Mark)))
             (result (alist-cons 'Marne 'Marquette base)))
        (check-equal? result
                       '((Marne . Marquette)
                         (Manteno . Mapleside)
                         (Mapleton . Maquoketa)
                         (Marathon . Marcus)
                         (Marengo . Marietta)
                         (Marion . Mark)))
        (check-eq? (cdr result) base)))

     (test-case
      "alist-cons:longer-list-with-duplicate-key"
      (let* ((base '((Marquisville . Marsh)
                     (Marshalltown . Martelle)
                     (Martensdale . Martinsburg)
                     (Martinstown . Marysville)
                     (Masonville . Massena)
                     (Massey . Massilon)
                     (Matlock . Maud)))
             (result (alist-cons 'Masonville 'Maurice base)))
        (check-equal? result '((Masonville . Maurice)
                                (Marquisville . Marsh)
                                (Marshalltown . Martelle)
                                (Martensdale . Martinsburg)
                                (Martinstown . Marysville)
                                (Masonville . Massena)
                                (Massey . Massilon)
                                (Matlock . Maud)))
        (check-eq? (cdr result) base)))

     ;; ALIST-COPY

     (test-case
      "alist-copy:null-list"
      (check-true (null? (alist-copy '()))))

     (test-case
      "alist-copy:flat-list"
      (let* ((original '((Maxon . Maxwell)
                         (Maynard . Maysville)
                         (McCallsburg . McCausland)
                         (McClelland . McGregor)
                         (McIntire . McNally)))
             (result (alist-copy original)))
        (check-true
         (and (equal? result original)
              (not (eq? result original))
              (not (eq? (car result) (car original)))
              (not (eq? (cdr result) (cdr original)))
              (not (eq? (cadr result) (cadr original)))
              (not (eq? (cddr result) (cddr original)))
              (not (eq? (caddr result) (caddr original)))
              (not (eq? (cdddr result) (cdddr original)))
              (not (eq? (cadddr result) (cadddr original)))
              (not (eq? (cddddr result) (cddddr original)))
              (not (eq? (car (cddddr result))
                        (car (cddddr original))))))))

     (test-case
      "alist-copy:bush"
      (let* ((first '(McPaul))
             (second '(McPherson
                       Mechanicsville
                       Mederville
                       (Mediapolis Medora)
                       ((Mekee Melbourne Melcher))))
             (third 'Melrose)
             (original (list (cons 'Meltonville first)
                             (cons 'Melvin second)
                             (cons 'Menlo third)))
             (result (alist-copy original)))
        (check-true
         (and (equal? result original)
              (not (eq? result original))
              (not (eq? (car result) (car original)))
              (eq? (cdar result) first)
              (not (eq? (cdr result) (cdr original)))
              (not (eq? (cadr result) (cadr original)))
              (eq? (cdadr result) second)
              (not (eq? (cddr result) (cddr original)))
              (not (eq? (caddr result) (caddr original)))
              (eq? (cdaddr result) third)))))

     ;; ALIST-DELETE

     (test-case
      "alist-delete:null-list"
      (check-true (null? (alist-delete 'Mercer '() (lambda (x y) #t)))))

     (test-case
      "alist-delete:singleton-list"
      (check-equal?
       (alist-delete 'Meriden
                     '((Merrill . Merrimac)))
       '((Merrill . Merrimac))))

     (test-case
      "alist-delete:all-elements-removed"
      (check-true
       (null? (alist-delete 'Meservey
                            '((Metz . Meyer)
                              (Middleburg . Middletwon)
                              (Midvale . Midway)
                              (Miles . Milford)
                              (Miller . Millersburg))
                            (lambda (x y) #t)))))

     (test-case
      "alist-delete:some-elements-removed"
      (check-equal?
       (alist-delete 561
                     '((562 . 563)
                       (565 . 564)
                       (566 . 567)
                       (569 . 568)
                       (570 . 571))
                     (lambda (x y) (odd? (+ x y))))
       '((565 . 564) (569 . 568))))

     (test-case
      "alist-delete:no-elements-removed"
      (check-equal?
       (alist-delete 'Millerton
                     '((Millman . Millnerville)
                       (Millville . Milo)
                       (Milton . Minburn)
                       (Minden . Mineola)
                       (Minerva . Mingo))
                     (lambda (x y) #f))
       '((Millman . Millnerville)
         (Millville . Milo)
         (Milton . Minburn)
         (Minden . Mineola)
         (Minerva . Mingo))))

     ;; ALIST-DELETE!


     ;; ALIST-DELETE

     (test-case
      "alist-delete:null-list"
      (check-true (null? (alist-delete '(Reasnor . Redding) '()))))

     (test-case
      "alist-delete:in-singleton-list"
      (check-true (null?
                    (alist-delete '(Redfield . Reeceville)
                                  '(((Redfield . Reeceville) . Reinbeck))))))

     (test-case
      "alist-delete:not-in-singleton-list"
      (check-equal?
       (alist-delete '(Rembrandt . Remsen)
                     '(((Renwick . Republic) . Rhodes)))
       '(((Renwick . Republic) . Rhodes))))

     (test-case
      "alist-delete:at-beginning-of-longer-list"
      (check-equal?
       (alist-delete '(Riceville . Richard)
                     '(((Riceville . Richard) . Richfield)
                       ((Richland . Richmond) . Rickardsville)
                       ((Ricketts . Rider) . Ridgeport)
                       ((Ridgeway . Riggs) . Rinard)
                       ((Ringgold . Ringsted) . Rippey)))
       '(((Richland . Richmond) . Rickardsville)
         ((Ricketts . Rider) . Ridgeport)
         ((Ridgeway . Riggs) . Rinard)
         ((Ringgold . Ringsted) . Rippey))))

     (test-case
      "alist-delete:in-middle-of-longer-list"
      (check-equal?
       (alist-delete '(Ritter . Riverdale)
                     '(((Riverside . Riverton) . Roberts)
                       ((Robertson . Robins) . Robinson)
                       ((Rochester . Rockdale) . Rockford)
                       ((Rockville . Rockwell) . Rodman)
                       ((Ritter . Riverdale) . Rodney)
                       ((Roelyn . Rogers) . Roland)
                       ((Rolfe . Rome) . Roscoe)))
       '(((Riverside . Riverton) . Roberts)
         ((Robertson . Robins) . Robinson)
         ((Rochester . Rockdale) . Rockford)
         ((Rockville . Rockwell) . Rodman)
         ((Roelyn . Rogers) . Roland)
         ((Rolfe . Rome) . Roscoe))))

     (test-case
      "alist-delete:at-end-of-longer-list"
      (check-equal?
       (alist-delete '(Rose . Roselle)
                     '(((Roseville . Ross) . Rosserdale)
                       ((Rossie . Rossville) . Rowan)
                       ((Rowley . Royal) . Rubio)
                       ((Ruble . Rudd) . Runnells)
                       ((Rose . Roselle) . Russell)))
       '(((Roseville . Ross) . Rosserdale)
         ((Rossie . Rossville) . Rowan)
         ((Rowley . Royal) . Rubio)
         ((Ruble . Rudd) . Runnells))))

     (test-case
      "alist-delete:not-in-longer-list"
      (check-equal?
       (alist-delete '(Ruthven . Rutland)
                     '(((Rutledge . Ryan) . Sabula)
                       ((Sageville . Salem) . Salina)
                       ((Salix . Sanborn) . Sandusky)
                       ((Sandyville . Santiago) . Saratoga)
                       ((Sattre . Saude) . Savannah)))
       '(((Rutledge . Ryan) . Sabula)
         ((Sageville . Salem) . Salina)
         ((Salix . Sanborn) . Sandusky)
         ((Sandyville . Santiago) . Saratoga)
         ((Sattre . Saude) . Savannah))))

     (test-case
      "alist-delete:several-matches-in-longer-list"
      (check-equal?
       (alist-delete '(Sawyer . Saylor)
                     '(((Saylorville . Scarville) . Schaller)
                       ((Schleswig . Schley) . Sciola)
                       ((Sawyer . Saylor) . Scranton)
                       ((Searsboro . Sedan) . Selma)
                       ((Sawyer . Saylor) . Seneca)
                       ((Seney . Sewal) . Sexton)
                       ((Sawyer . Saylor) . Seymour)))
       '(((Saylorville . Scarville) . Schaller)
         ((Schleswig . Schley) . Sciola)
         ((Searsboro . Sedan) . Selma)
         ((Seney . Sewal) . Sexton))))

     ;; ALIST-DELETE!


     ))
  )

;;; alist-test.rkt ends here
