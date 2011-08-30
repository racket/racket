;;;
;;; <mime-unit.rkt> ---- MIME support
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Wish Computing.
;;;
;;; This file is part of mime

;;; mime-plt is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; mime-plt is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with mime-plt; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;;
;; Commentary: MIME support for PLT Scheme: an implementation of
;; rfc2045, rfc2046, rfc2047, rfc2048, and rfc2049.

#lang racket/base

(require racket/unit 
         "mime-sig.rkt" "mime.rkt")

(define-unit-from-context mime@ mime^)

(provide mime@)