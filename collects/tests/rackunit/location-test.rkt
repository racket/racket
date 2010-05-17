;;;
;;; Time-stamp: <2008-07-28 11:14:22 nhw>
;;;
;;; Copyright (C) 2005 by Noel Welsh.
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:
#lang racket/base

(require rackunit
         rackunit/private/location)

(provide location-tests)

(define (read-syntax/lang name port)
  (parameterize ([read-accept-reader #t])
    (read-syntax name port)))

(define location-tests
  (test-suite
   "All tests for location"

   (test-case
    "syntax->location ok"
    (around
     (with-output-to-file "test-file.rkt"
       (lambda () (display "#lang racket\n'foo\n")))
     (let* ([stx (read-syntax/lang (string->path "test-file.rkt")
                                   (open-input-file "test-file.rkt"))]
            [rep (syntax->location stx)])
       (check-equal? (location-source rep)
                     (syntax-source stx))
       (check-equal? (location-position rep)
                     (syntax-position stx))
       (check-equal? (location-span rep)
                     (syntax-span stx)))
     (delete-file "test-file.rkt")))

   (test-case
    "Emacs compatible location strings"
    (check string=?
           (location->string
            (syntax->location
             (datum->syntax
              #f #f
              (list "file.rkt" 42 38 1240 2))))
           "file.rkt:42:38")
    (check string=?
           (location->string
            (syntax->location
             (datum->syntax
              #f #f
              (list (string->path "file.rkt") 42 38 1240 2))))
           "file.rkt:42:38")
    (check string=?
           (location->string
            (syntax->location
             (datum->syntax
              #f #f
              (list #f 42 38 1240 2))))
           "unknown:42:38")
    (check string=?
           (location->string
            (syntax->location
             (datum->syntax
              #f #f
              (list 'foo.rkt 42 38 1240 2))))
           "foo.rkt:42:38")
    (check string=?
           (location->string
            (syntax->location
             (datum->syntax
              #f #f
              (list "foo.rkt" #f #f #f #f))))
           "foo.rkt:?:?"))
   ))
  
