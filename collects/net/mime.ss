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
;;; along with mime; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary:

(module mime mzscheme
  (require (lib "unitsig.ss"))

  (require "mime-sig.ss"
           "mime-unit.ss"
           "qp-sig.ss"
           "qp.ss"
           "base64-sig.ss"
           "base64.ss"
	   "head-sig.ss"
	   "head.ss")

  (define-values/invoke-unit/sig net:mime^
    net:mime@
    #f
    net:base64^ net:qp^ net:head^)

  (provide-signature-elements net:mime^))

;;; mime.ss ends here