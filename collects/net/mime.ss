;;;
;;; <mime.ss> ---- MIME support
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Wish Computing.
;;;
;;; This file is part of mime

;;; mime is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; mime is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with mime; see the file COPYING.  If not, write to the Free
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module mime mzscheme
  (require (lib "unit.ss")
           "mime-sig.ss"
           "mime-unit.ss"
           "qp-sig.ss"
           "qp.ss"
           "base64-sig.ss"
           "base64.ss"
           "head-sig.ss"
           "head.ss")

  (define-unit-from-context base64@ base64^)
  (define-unit-from-context qp@ qp^)
  (define-unit-from-context head@ head^)

  (define-compound-unit/infer mime@2 (import) (export mime^)
    (link base64@ qp@ head@ mime@))

  (define-values/invoke-unit/infer mime@2)

  (provide-signature-elements mime^))

;;; mime.ss ends here
