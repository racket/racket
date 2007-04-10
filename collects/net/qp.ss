;;;
;;; <qp.ss> ---- Quoted Printable Encoding/Decoding
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Francisco Solsona.
;;;
;;; This file is part of mime-plt.

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

(module qp mzscheme
  (require (lib "unit.ss") "qp-sig.ss" "qp-unit.ss")

  (define-values/invoke-unit/infer qp@)

  (provide-signature-elements qp^))

;;; qp.ss ends here
