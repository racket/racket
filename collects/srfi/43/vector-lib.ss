;;;
;;; <util.ss> ---- Utility functions
;;; Time-stamp: <05/03/07 18:21:41 Zhu Chongkai>
;;;
;;; Copyright (C) 2005-2006 by Zhu Chongkai. 
;;;
;;; This file is part of SRFI-43.

;;; SRFI-43 is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; SRFI-43 is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with SRFI-43; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA

;;; Author: Zhu Chongkai <mrmathematica@yahoo.com>
;;

(module vector-lib mzscheme

  (require "constructors.ss"
	   "predicates.ss"
	   "iteration.ss"
           "searching.ss"
	   (all-except "mutators.ss" vector-fill!)
	   (rename "mutators.ss" s:vector-fill! vector-fill!)
           (all-except "conversion.ss" vector->list)
           (rename "conversion.ss" s:vector->list vector->list))

  (provide 
   (all-from "constructors.ss")
   (all-from "predicates.ss")
   (all-from "iteration.ss")
   (all-from "searching.ss")
   (all-from "mutators.ss")
   (all-from "conversion.ss")))
