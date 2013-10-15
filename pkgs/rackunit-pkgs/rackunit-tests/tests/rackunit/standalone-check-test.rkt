;;;
;;; Time-stamp: <2008-06-06 15:32:49 noel>
;;;
;;; Copyright (C) by Noel Welsh. 
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


;; Here we check the standalone (not within a test-case or
;; test-suite) semantics of checks.  These tests are not
;; part of the standard test suite and must be run
;; separately.

#lang racket/base

(require rackunit/private/check)

;; This check should succeed
(check = 1 1 0.0)

;; This check should display an error including the message "Outta here!"
(check-pred (lambda (x) (error "Outta here!")) 'foo)


;; This check should display a failure
(check = 1 2 0.0)

;; This check should display "Oh HAI!"
(parameterize
    ([current-check-handler (lambda (e) (display "Oh HAI!\n"))])
  (check = 1 2 0.0))

;; This check should display "I didn't run"
(parameterize
    ([current-check-around (lambda (t) (display "I didn't run\n"))])
  (check = 1 1 0.0))
