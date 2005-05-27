(module makefile mzscheme
  (require (lib "make.ss" "make")
           "make-gl-info.ss")

  (provide pre-installer)
    
  (define dir (build-path "compiled"))
  
  (define (pre-installer home)
    (parameterize ((current-directory (collection-path "sgl"))
		   (make-print-reasons #f)
		   (make-print-checking #f))
      (make/proc
       `((,(build-path dir "gl-info.zo")
           ("make-gl-info.ss" ,(build-path home "include" "schvers.h"))
           ,(lambda () (make-gl-info dir home)))))))
  )
