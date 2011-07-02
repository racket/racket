;;;
;;; <filter-test.rkt> ---- List filtering and partitioning tests
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

(module filter-test
  mzscheme
  (require rackunit)
  (require (all-except srfi/1/filter member))

  (provide filter-tests)

  (define filter-tests
    (test-suite
     "List filtering tests"

     ;; FILTER

     (test-case
      "filter:null-list"
      (check-true (null? (filter (lambda (x) #t) '()))))

     (test-case
      "filter:singleton-list"
      (check-equal?
       (filter (lambda (x) #t) '(Agency))
       '(Agency)))

     (test-case
      "filter:all-elements-removed"
      (check-true
       (null? (filter (lambda (x) #f)
                      '(Ainsworth Akron Albany Albaton Albia)))))

     (test-case
      "filter:some-elements-removed"
      (check-equal?
       (filter even? '(86 87 88 89 90))
       '(86 88 90)))

     (test-case
      "filter:no-elements-removed"
      (check-equal?
       (filter (lambda (x) #t)
               '(Albion Alburnett Alden Alexander Algona))
       '(Albion Alburnett Alden Alexander Algona)))

     ;; FILTER!

     (test-case
      "filter!:null-list"
      (check-true
       (null? (filter! (lambda (x) #t) (list)))))

     (test-case
      "filter!:singleton-list"
      (check-equal?
       (filter! (lambda (x) #t) (list 'Alice))
       '(Alice)))

     (test-case
      "filter!:all-elements-removed"
      (check-true
       (null? (filter! (lambda (x) #f)
                       (list 'Alleman 'Allendorf 'Allerton 'Allison 'Almont)))))

     (test-case
      "filter!:some-elements-removed"
      (check-equal?
       (filter! even? (list 91 92 93 94 95))
       '(92 94)))

     (test-case
      "filter!:no-elements-removed"
      (check-equal?
       (filter! (lambda (x) #t)
                (list 'Almoral 'Alpha 'Alta 'Alton 'Altoona))
       '(Almoral Alpha Alta Alton Altoona)))

     ;; REMOVE

     (test-case
      "remove:null-list"
      (check-true
       (null? (remove (lambda (x) #t) '()))))

     (test-case
      "remove:singleton-list"
      (check-equal?
       (remove (lambda (x) #f) '(Alvord))
       '(Alvord)))

     (test-case
      "remove:all-elements-removed"
      (check-true
       (null? (remove (lambda (x) #t) '(Amana Amber Ames Amish Anamosa)))))

     (test-case
      "remove:some-elements-removed"
      (check-equal?
       (remove even? '(96 97 98 99 100))
       '(97 99)))

     (test-case
      "remove:no-elements-removed"
      (check-equal?
       (remove (lambda (x) #f)
               '(Anderson Andover Andrew Andrews Angus))
       '(Anderson Andover Andrew Andrews Angus)))

     ;; REMOVE!

     (test-case
      "remove!:null-list"
      (check-true (null? (remove! (lambda (x) #t) (list)))))

     (test-case
      "remove!:singleton-list"
      (check-equal?
       (remove! (lambda (x) #f) (list 'Anita))
       '(Anita)))

     (test-case
      "remove!:all-elements-removed"
      (check-true
       (null?
        (remove! (lambda (x) #t)
                 (list 'Ankeny 'Anthon 'Aplington 'Arcadia 'Archer)))))

     (test-case
      "remove!:some-elements-removed"
      (check-equal?
       (remove! even? (list 101 102 103 104 105))
       '(101 103 105)))

     (test-case
      "remove!:no-elements-removed"
      (check-equal?
       (remove! (lambda (x) #f)
                (list 'Ardon 'Aredale 'Argo 'Argyle 'Arion))
       '(Ardon Aredale Argo Argyle Arion)))

     ;; PARTITION

     (test-case
      "partition:null-list"
      (let-values (((in out) (partition (lambda (x) #f) '())))
        (check-true (and (null? in) (null? out)))))

     (test-case
      "partition:singleton-list"
      (let-values (((in out) (partition (lambda (x) #f) '(Arispe))))
        (check-true (and (null? in) (equal? out '(Arispe))))))

     (test-case
      "partition:all-satisfying"
      (let-values (((in out)
                    (partition (lambda (x) #t)
                               '(Arlington Armstrong Arnold Artesian Arthur))))
        (check-true
         (and (equal? in
                      '(Arlington Armstrong Arnold Artesian Arthur))
              (null? out)))))

     (test-case
      "partition:mixed-starting-in"
      (let-values (((in out)
                    (partition even? '(106 108 109 111 113 114 115 117 118 120))))
        (check-true (and (equal? in '(106 108 114 118 120))
                          (equal? out '(109 111 113 115 117))))))

     (test-case
      "partition:mixed-starting-out"
      (let-values (((in out)
                    (partition even? '(121 122 124 126))))
        (check-true (and (equal? in '(122 124 126))
                          (equal? out '(121))))))

     (test-case
      "partition:none-satisfying"
      (let-values (((in out)
                    (partition (lambda (x) #f)
                               '(Asbury Ashawa Ashland Ashton Aspinwall))))
        (check-true (and (null? in)
                          (equal? out
                                  '(Asbury Ashawa Ashland Ashton Aspinwall))))))

     ;; PARTITION!

     (test-case
      "partition!:null-list"
      (let-values (((in out)
                    (partition! (lambda (x) #f) (list))))
        (check-true (and (null? in) (null? out)))))

     (test-case
      "partition!:singleton-list"
      (let-values (((in out)
                    (partition! (lambda (x) #f) (list 'Astor))))
        (lambda (in out) (and (null? in) (equal? out '(Astor))))))

     (test-case
      "partition!:all-satisfying"
      (let-values (((in out)
                    (partition! (lambda (x) #t)
                                (list 'Atalissa 'Athelstan 'Atkins 'Atlantic
                                      'Attica))))
        (check-true
         (and (equal? in
                      '(Atalissa Athelstan Atkins Atlantic Attica))
              (null? out)))))

     (test-case
      "partition!:mixed-starting-in"
      (let-values (((in out)
                    (partition! odd?
                                (list 127 129 130 132 134 135 136 138 139 141))))
        (check-true
         (and (equal? in '(127 129 135 139 141))
              (equal? out '(130 132 134 136 138))))))

     (test-case
      "partition!:mixed-starting-out"
      (let-values (((in out)
                    (partition! odd? (list 142 143 145 147))))
        (check-true
         (and (equal? in '(143 145 147))
              (equal? out '(142))))))

     (test-case
      "partition!:none-satisfying"
      (let-values (((in out)
                    (partition! (lambda (x) #f)
                                (list 'Auburn 'Audubon 'Augusta 'Aurelia
                                      'Aureola))))
        (check-true
         (and (null? in)
              (equal? out
                      '(Auburn Audubon Augusta Aurelia Aureola))))))

     ))
  )

;;; filter-test.rkt ends here
