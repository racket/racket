  (define (cw-event . args)
    (apply send-event "CWIE" args))
  (define (cw . args)
    (apply cw-event "MMPR" args))

  (define (ping-cw?)
    (with-handlers ([void (lambda (x) #f)])
      (cw "GDoc")
      #t))
  
 (define (macos-make name base-project tmp-suffix quiet? input-files output-file includes)
    (define src-files (map path->complete-path input-files))
    (define dest-file (path->complete-path output-file))
    (define proj-dir (build-path (collection-path "dynext")))
    (define tmp-proj (build-path proj-dir "building-extension"))
    (define ext-out (build-path proj-dir (format "extension.~a" tmp-suffix)))
    (define debug-out (string-append ext-out ".xSYM"))
    (define data-out (string-append tmp-proj " Data"))
    (define (delete f)
      (cond
        [(file-exists? f) (delete-file f)]
        [(directory-exists? f) (map (lambda (i) (delete (build-path f i))) 
                                    (directory-list f))
                               (delete-directory f)]
        [else (void)]))
    
    (when (string=? (system-library-subpath) "68k-mac")
      (error name "not supported for 68k-mac"))
    
    (delete dest-file)
    (delete tmp-proj)
    (unless (copy-file (build-path proj-dir base-project) 
                       tmp-proj)
         (error name "couldn't create the CodeWarrior project"))
    
    (with-handlers ([void (lambda (exn)
                             (error name "~a" (exn-message exn)))])
     (let ([started? (ping-cw?)])
      (unless started?
        ; Start CW
        (system "CWIE")
        (unless (ping-cw?)
          (sleep 1) ; wait a second...
          (unless (ping-cw?)
            (error name "couldn't start CodeWarrior; try starting CW, then leave it open while compiling"))))
	    ; Open the project
        (cw-event "aevt" "odoc" `#(file ,tmp-proj))
        (cw "SDfP" `#(file ,tmp-proj))
        (cw "AddF" (map (lambda (f) `#(file ,f)) src-files))
        (cw "Make")
    	; Clean up
    	(cw "ClsP")
    	(unless started?
          (cw-event "aevt" "quit")
          (let loop () (sleep 1) (when (ping-cw?) (loop))))))
          
    (delete tmp-proj)
    (delete debug-out)
    (delete data-out)

    (unless (rename-file-or-directory ext-out dest-file)
      (unless (copy-file ext-out dest-file)
      (error name "couldn't move output to destination: ~a" 
      	     output-file))))