;;;
;;; <qp-unit.rkt> ---- Quoted Printable Implementation
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Francisco Solsona.
;;;
;;; This file was part of mime-plt.

;;; mime-plt is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with mime-plt; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

#lang racket/base

(require racket/unit
         "qp-sig.rkt" "qp.rkt")

(define-unit-from-context qp@ qp^)

(provide qp@)