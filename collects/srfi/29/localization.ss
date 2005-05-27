;;; <localization.ss> SRFI-29: localization port to PLT Scheme -*- Scheme -*-
;;; Time-stamp: <03/05/08 12:49:08 solsona>
;;;
;;; Copyright (C) Scott G. Miller (2002). All Rights Reserved.
;;;
;;; This file is part of PLT Scheme.

;;; PLT Scheme is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.

;;; PLT Scheme is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public
;;; License along with PLT Scheme; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Author: Scott G. Miller
;;; Modified for PLT Scheme by: Francisco Solsona <solsona@acm.org>

(module localization mzscheme
  (require (lib "etc.ss"))
  (provide current-language current-country current-locale-details
	   load-bundle! store-bundle! declare-bundle!
	   localized-template
	   ;; NOT in SRFI-29, but useful in PLT when using/changing current-locale:
	   re-read-locale
	   )


  (define get-from-locale
    (lambda (what)
      (let ((locale (current-locale)))
	(if (string=? locale "")
	    (case what
	      ((language) 'en) ;; Default language: English
	      ((country) 'us) ;; Default country: US
	      (else null))
	    (let ((len (string-length locale)))
	      (case what
		((language)
		 (if (>= len 2)
		     (string->symbol (substring locale 0 2))
		     'en))
		((country)
		 (if (>= len 5)
		     (string->symbol (substring locale 3 5))
		     'us))
		(else ;; details
		 (if (> len 6)
		     (list (string->symbol (substring locale 6)))
		     null))))))))

  
  (define list-of
    (lambda (pred)
      (lambda (lst)
	(call/cc (lambda (exit)
		   (let loop ((lst lst))
		     (cond ((null? lst) #t)
			   (else (or (pred (car lst))
				     (exit #f))
				 (loop (cdr lst))))))))))


  ;; The association list in which bundles will be stored
  (define *localization-bundles* '())

  (define current-language
    (make-parameter (get-from-locale 'language) (lambda (l)
						  (if (symbol? l) l 'en))))

  (define current-country
    (make-parameter (get-from-locale 'country) (lambda (l)
						 (if (symbol? l) l 'us))))
  
  (define current-locale-details
    (make-parameter (get-from-locale 'details) (lambda (lst)
						 (if ((list-of symbol?) lst)
						     lst
						     null))))

  ;; If you change (current-locale), you don't have to set current-*
  ;; by hand, you can simply call this procedure, and it will update
  ;; those parameters to the values in the new locale.
  (define re-read-locale
    (lambda ()
      (current-language (get-from-locale 'language))
      (current-country (get-from-locale 'country))
      (current-locale-details (get-from-locale 'details))))
  
  ;; System bundles are here:
  (define system-bundles (build-path
			  (collection-path "srfi") "29" "bundles"))

  ;; bundle-specifier: (listof symbol)
  ;; i.e. package + locale, (package-name [language] [country] [details ...])
  (define load-bundle!
    (opt-lambda (bundle-specifier [alternate-path null])
      (let* ((filename (case (length bundle-specifier)
			 ((1) (symbol->string (car bundle-specifier)))
			 ((2) (build-path (symbol->string (cadr bundle-specifier))
					  (symbol->string (car bundle-specifier))))
			 (else (build-path (symbol->string (cadr bundle-specifier))
					   (symbol->string (caddr bundle-specifier))
					   (symbol->string (car bundle-specifier))))))
	     (path (build-path (if (null? alternate-path)
				   system-bundles
				   alternate-path)
			       filename)))
	(and (file-exists? path)
	     ;; Found!
	     (declare-bundle! bundle-specifier
			      (with-input-from-file path read))))))

  (define extract-assoc-list
    (lambda (bundle-specifier)
      (let loop ((bundle *localization-bundles*))
	(cond ((null? bundle) #f)
	      ((equal? (caar bundle) bundle-specifier)
	       (cdar bundle))
	      (else (extract-assoc-list (cdr bundle)))))))
  
  (define store-bundle!
    (opt-lambda (bundle-specifier [alternate-path null])
      (let* ((sub-path (if (null? alternate-path) system-bundles alternate-path))
	     (file (case (length bundle-specifier)
		     ((1) (symbol->string (car bundle-specifier)))
		     ((2)
		      (let* ((dir (build-path sub-path (symbol->string (cadr bundle-specifier))))
			     (file (build-path dir (symbol->string (car bundle-specifier)))))
			(unless (directory-exists? dir)
			  (make-directory dir))
			file))
		     (else
		      (let* ((dir (build-path sub-path (symbol->string (cadr bundle-specifier))))
			     (dir2 (build-path dir (symbol->string (caddr bundle-specifier))))
			     (file (build-path dir2 (symbol->string (car bundle-specifier)))))
			(unless (directory-exists? dir)
			  (make-directory dir))
			(unless (directory-exists? dir2)
			  (make-directory dir2))
			file))))
	     (bundle-assoc-list (extract-assoc-list bundle-specifier)))

	(and bundle-assoc-list
	     ;; Storing bundle!
	     (if (file-exists? file)
		 (if (memq 'write (file-or-directory-permissions file))
		     ;; Replacing the existing bundle
		     (with-output-to-file file (lambda ()
						 (write bundle-assoc-list))
					  'truncate/replace)
		     #f);; should be an exception?
		 (if (let-values ([(base filename directory?)  (split-path file)])
				 (memq 'write (file-or-directory-permissions base)))
		     ;; First time, this bundle is stored here:
		     (with-output-to-file file (lambda ()
						 (write bundle-assoc-list)))
		     #f))))))

  ;; Declare a bundle of templates with a given bundle specifier
  (define declare-bundle!
    (letrec ((remove-old-bundle
	      (lambda (specifier bundle)
		(cond ((null? bundle) '())
		      ((equal? (caar bundle) specifier)
		       (cdr bundle))
		      (else (cons (car bundle)
				  (remove-old-bundle specifier
						     (cdr bundle))))))))
      (lambda (bundle-specifier bundle-assoc-list)
	(set! *localization-bundles*
	      (cons (cons bundle-specifier bundle-assoc-list)
		    (remove-old-bundle bundle-specifier
				       *localization-bundles*))))))

  ;;Retrieve a localized template given its package name and a template name
  (define localized-template
    (letrec ((rdc
	      (lambda (ls)
		(if (null? (cdr ls))
		    '()
		    (cons (car ls) (rdc (cdr ls))))))
	     (find-bundle
	      (lambda (specifier template-name)
		(cond ((assoc specifier *localization-bundles*) =>
		       (lambda (bundle) bundle))
		      ((null? specifier) #f)
		      (else (find-bundle (rdc specifier)
					 template-name))))))
      (lambda (package-name template-name)
	(let loop ((specifier (cons package-name
				    (list (current-language)
					  (current-country)))))
	  (and (not (null? specifier))
	       (let ((bundle (find-bundle specifier template-name)))
		 (and bundle
		      (cond ((assq template-name bundle) => cdr)
			     ((null? (cdr specifier)) #f)
			     (else (loop (rdc specifier)))))))))))

  )