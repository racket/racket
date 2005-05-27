;; sets directories for Windows installation

(let ([plthome (build-path 
		(current-load-relative-directory) 
		'up 'up)])
  (putenv "PLTHOME" plthome)
  (current-library-collection-paths 
   (list (build-path plthome "collects"))))

