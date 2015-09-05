#lang racket/base
(require racket/promise
         compiler/module-suffix)

(provide append-zo-suffix
	 append-c-suffix
	 append-constant-pool-suffix
	 append-object-suffix
	 append-extension-suffix
	 
	 extract-base-filename/ss
	 extract-base-filename/c
	 extract-base-filename/kp
	 extract-base-filename/o
	 extract-base-filename/ext)

(define (append-zo-suffix s)
  (path-add-suffix s #".zo"))

(define (append-c-suffix s)
  (path-add-suffix s #".c"))

(define (append-constant-pool-suffix s)
  (path-add-suffix s #".kp"))

(define (append-object-suffix s)
  (path-add-suffix s (case (system-type)
		       [(unix macosx) #".o"]
		       [(windows) #".obj"])))

(define (append-extension-suffix s)
  (path-add-suffix s (system-type 'so-suffix)))

(define (extract-suffix appender)
  (subbytes (path->bytes (appender (bytes->path #"x"))) 1))

(define (extract-rx pat)
  (byte-pregexp (bytes-append #"^(.*)\\.(?i:" pat #")$")))

(define (extract who s program rx kind simple)
  (unless (path-string? s)
    (raise-argument-error who "path-string?" s))
  (cond
   [(regexp-match rx (if (path? s) s (string->path s)))
    => (lambda (m) (bytes->path (cadr m)))]
   [program
    (if simple
        (error program "not a ~a filename (doesn't end with ~a): ~a"
               kind simple s)
        (path-replace-suffix s #""))]
   [else #f]))

(define module-suffix-regexp
  (delay (get-module-suffix-regexp #:group 'libs)))

(define (extract-base-filename/ss s [program #f]
                                  #:module-pattern [module-pattern
                                                    (force module-suffix-regexp)])
  (extract 'extract-base-filename/ss
           s program
           module-pattern
           "Racket"
           #f))

(define (extract-base-filename/c s [program #f])
  (extract 'extract-base-filename/c
           s program
           (extract-rx #"c|cc|cxx|cpp|c[+][+]|m")
           "C"
           ".c, .cc, .cxx, .cpp, .c++, or .m"))

(define (extract-base-filename/kp s [program #f])
  (extract 'extract-base-filename/kp
           s
           program
           (extract-rx #"kp")
           "constant pool"
           ".kp"))

(define (extract-base-filename/o s [program #f])
  (extract 'extract-base-filename/o
           s program
           (extract-rx (case (system-type)
                         [(unix beos macos macosx) #"o"]
                         [(windows) #"obj"]))
           "compiled object"
           (extract-suffix append-object-suffix)))
  
(define (extract-base-filename/ext s [program #f])
  (extract 'extract-base-filename/ext
           s
           program
           (extract-rx (regexp-quote (subbytes (system-type 'so-suffix) 1) #f))
           "Racket extension"
           (extract-suffix append-extension-suffix)))
