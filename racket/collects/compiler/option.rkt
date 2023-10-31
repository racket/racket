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

(define (make-bool-parameter val name)
  (make-parameter val (lambda (v) (and v #t)) name))

(define somewhat-verbose (make-bool-parameter #f 'somewhat-verbose))

(define verbose (make-bool-parameter #f 'verbose))

(define 3m (make-bool-parameter (eq? '3m (system-type 'gc)) '3m))

(define setup-prefix (make-parameter "" #f 'setup-prefix))

(define compile-subcollections (make-bool-parameter #t 'compile-subcollections))
