#lang racket/base

;;;
;;; <uri-codec-unit.rkt> ---- En/Decode URLs and form-urlencoded data
;;; Time-stamp: <03/04/25 10:31:31 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of Net.

;;; Net is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; Net is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with Net; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>

(require racket/unit
         "uri-codec-sig.rkt" "uri-codec.rkt")

(define-unit-from-context uri-codec@ uri-codec^)

(provide uri-codec@)