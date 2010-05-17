;;;
;;; Time-stamp: <2008-06-19 21:08:18 noel>
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
;;
;;
;; Commentary:
#lang racket/base

(require rackunit
         rackunit/private/text-ui-util)

(provide text-ui-util-tests)

(define text-ui-util-tests
  (test-suite
   "All tests for text-ui-util"
   
   (test-equal?
    "trim-current-directory leaves directories outside the current directory alone"
    (trim-current-directory "/foo/bar/")
    "/foo/bar/")
   
   (test-equal?
    "trim-current-directory strips directory from files in current directory"
    (trim-current-directory
     (path->string (build-path (current-directory) "foo.rkt")))
    "foo.rkt")
   
   (test-equal?
    "trim-current-directory leaves subdirectories alone"
    (trim-current-directory
     (path->string (build-path (current-directory) "foo" "bar.rkt")))
    "foo/bar.rkt")
   ))
