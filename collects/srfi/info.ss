;;;
;;; <info.ss> ---- info file for SRFI collection

;;; Time-stamp: <03/03/27 19:29:44 solsona>
;;;
;;; Copyright (C) 2002 by Noel Welsh. 
;;;
;;; This file is part of SRFI.

;;; SRFI is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:
;;
;; This file tell mzc how to compile the srfi collection.  As
;; compilation takes place when installing a .plt it is essential that
;; this file is updated whenever a new release is made.

(module info (lib "infotab.ss" "setup")
  (define name "srfi")
  (define doc.txt "doc.txt")

  (define blurb '("SRFI collects together ports to PLT Scheme of the Scheme Request for Implementation libraries.  For further details about the SRFI process see " (a ((href "http://srfi.schemers.org/")) "http://srfi.schemers.org/")))
  )

;;; info.ss ends here
