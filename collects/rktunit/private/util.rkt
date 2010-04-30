;;;
;;; Time-stamp: <2008-07-28 12:51:11 nhw>
;;;
;;; Copyright (C) 2004 by Noel Welsh. 
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; Web testingis distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with Web testing; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

#lang racket/base

(require (for-syntax racket/base)
         mzlib/etc
         "check.rkt"
         "test-suite.rkt"
         "test-case.rkt")

(provide require/expose
         test-suite*
         check-regexp-match)

;; Requires a module and exposes some of its unprovided
;; (non-syntax!)  identifiers.
;; USAGE: (require/expose MODULE-NAME (IDS ...))
;;   where MODULE-NAME is as in the MzRacket manual (i.e.,
;;   a standard module spec) and IDS are the un-provided
;;   identifiers that you wish to expose in the current
;;   module.
(define-syntax (require/expose stx)
  (syntax-case stx ()
    [(_ mod (ids ...))
     (quasisyntax/loc stx
       (begin
         (require (only-in mod))
         (define-values (ids ...)
           (parameterize
               ([current-load-relative-directory
                 #,(datum->syntax
                    stx
                    `(,#'this-expression-source-directory)
                    stx)])
             (let ([ns (module->namespace 'mod)])
               (parameterize ([current-namespace ns])
                 (values
                  (namespace-variable-value 'ids)...)))))))]))

(define-syntax test-suite*
  (syntax-rules ()
    ((test-suite* name (case-name case-body ...) ...)
     (test-suite
      name
      (test-case case-name case-body ...) ...))))

(define-simple-check (check-regexp-match regex string)
  (and (string? string) 
       (regexp-match regex string)))

