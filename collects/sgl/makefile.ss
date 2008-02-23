(module makefile mzscheme
  (require make
	   setup/dirs
           "make-gl-info.ss")

  (provide pre-installer)
    
  (define dir (build-path "compiled"))
  
  (define (pre-installer home)
    (parameterize ((current-directory (collection-path "sgl"))
		   (make-print-reasons #f)
		   (make-print-checking #f))
      (make/proc
       `((,(build-path dir "gl-info_ss.zo")
           ("make-gl-info.ss" ,(build-path (find-include-dir) "schvers.h"))
           ,(lambda () (make-gl-info dir home)))))))
  )
