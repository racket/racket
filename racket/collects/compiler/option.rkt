#lang racket/base

(provide somewhat-verbose 
	 ;; default = #f
	 verbose 
	 ;; default = #f
	 
	 setup-prefix 
	 ;; string to embed in public names;
	 ;; used mainly for compiling extensions
	 ;; with the collection name so that 
	 ;; cross-extension conflicts are less
	 ;; likely in architectures that expose
	 ;; the public names of loaded extensions
	 ;; default = ""

	 3m 
	 ;; #t => build for 3m
	 ;; default = #f
	 
	 compile-subcollections
	 ;; #t => compile collection subdirectories
	 ;; default = #t
	 )

(define somewhat-verbose (make-parameter #f))
(define verbose (make-parameter #f))
(define 3m (make-parameter (eq? '3m (system-type 'gc))))

(define setup-prefix (make-parameter ""))

(define compile-subcollections (make-parameter #t))
