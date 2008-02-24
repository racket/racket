; SRFI 59
; Zhu Chongkai   mrmathematica@yahoo.com
; 29-May-2005
(module vicinity mzscheme
  
  (provide (all-defined))
  
  (define (implementation-vicinity)
    (pathname->vicinity
     (simplify-path
      (find-system-path 'exec-file))))
  
  (define (library-vicinity)
    (car (current-library-collection-paths)))
  
  (define (home-vicinity)
    (find-system-path 'home-dir))
  
  (define in-vicinity build-path)
  
  (define (user-vicinity)
    (current-directory))
  
  (define vicinity:suffix?
    (let ((suffi
           (case (system-type)
             ((macos)			'(#\:))
             ((windows)			'(#\\ #\/))
             ((unix oskit macosx)	'(#\/)))))
      (lambda (chr) (and (memv chr suffi) #t))))
  
  (define (pathname->vicinity pathname)
    (call-with-values
     (lambda () (split-path pathname))
     (lambda (base name must-be-dir?) base)))
  
  (define (program-vicinity)
    (current-load-relative-directory))
  
  (define sub-vicinity build-path)
  
  (define (make-vicinity pathname) pathname)
  
  )
