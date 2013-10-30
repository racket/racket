#|

# OS X pre-make script
# builds resource files, makes template Starter.app and GRacket.app
#
# the script must be run from the gracket build directory,
# and srcdir must be provided as the first argument

|#

(module osx_appl racket/base

  (require (lib "plist.rkt" "xml")
	   racket/system
	   racket/file
           racket/path)

  (define (try . l)
    (or (ormap (lambda (f)
		 (and (file-exists? f)
		      f))
	       l)
	(car (reverse l))))

  (define rez-path (or (getenv "REZ")
		       (find-executable-path "Rez")
		       (try "/Applications/Xcode.app/Contents/Developer/Tools/Rez"
			    "/Developer/Tools/Rez")))

  (define for-3m? (getenv "BUILDING_3M"))

  (define plthome (build-path (vector-ref (current-command-line-arguments) 0) 'up))
  (define suffix (vector-ref (current-command-line-arguments) 1))

  ; Rez where needed:
  (let* ([cw-path (build-path plthome "src" "mac" "cw")]
	 [rez-it (lambda (app src)
		   (printf "Writing ~a~n" (string-append app ".rsrc.OSX"))
		   (system* rez-path 
			    (path->string (build-path cw-path (string-append src ".r")))
			    "-UseDF" "-o" 
			    (path->string
			     (path-replace-suffix app #".rsrc.OSX"))))])
    ; (rez-it "Racket") ; useless under OS X...
    (rez-it "GRacket" "GRacket"))

  ; make .app templates in the right places:

  (define (realize-template path template-tree)
    (let* ([head-path (build-path path (car template-tree))])
      (when (file-exists? head-path)
	    (error 'realize-template 
		   "Can't create directory \"~a\" because there's a file with that name" 
		   head-path))
      (unless (directory-exists? head-path)
	      (printf "Creating directory: ~a\n" head-path)
	      (make-directory head-path))
      (for-each (lambda (template-tree) (realize-template head-path template-tree))
	        (cdr template-tree))))

  ; a template-tree is (list string<dir-name> (listof template-tree))
  (define app-template-tree
    '("Contents" ("MacOS") ("Resources")))
  (define fw-template-tree
    '("Resources"))

  (define (write-info contents-path info-plist)
    (let* ([info-plist-path (build-path contents-path "Info.plist")])
      (printf "writing file ~a\n" info-plist-path)
      (call-with-output-file info-plist-path
	(lambda (port)
	  (write-plist info-plist port))
	#:exists 'truncate)))

  (define (create-app dest-path app-name icon-src-name pkg-info-string info-plist)
    (let* ([app-path (build-path dest-path 
				 (string-append app-name ".app"))])
      (make-directory* app-path)
      (realize-template app-path app-template-tree)
      (let* ([pkg-info-path (build-path app-path "Contents" "PkgInfo")])
	(printf "writing file ~a\n" pkg-info-path)
	(call-with-output-file pkg-info-path
	  (lambda (port)
	    (fprintf port pkg-info-string))
	  #:exists 'truncate))
      (let* ([contents-path (build-path app-path "Contents")])
	(write-info contents-path info-plist)
	(let* ([icns-src (build-path plthome "src" "mac" "icon" (path-replace-suffix icon-src-name #".icns"))]
	       [icns-dest (build-path contents-path "Resources" (path-replace-suffix (file-name-from-path app-name) #".icns"))])
	  (unless (file-exists? icns-dest)
	    (copy-file icns-src icns-dest))))))

  (define (make-info-plist app-name signature app?)
    `(dict (assoc-pair "CFBundleDevelopmentRegion"
		       "English")
	   (assoc-pair "CFBundleExecutable"
		       ,app-name)
	   (assoc-pair "CFBundleIdentifier"
		       ,(format "org.racket-lang.~a" app-name))
	   ,@(if app?
		 `((assoc-pair "CFBundleIconFile"
			       ,app-name))
		 null)
	   (assoc-pair "CFBundleInfoDictionaryVersion"
		       "6.0")
	   (assoc-pair "CFBundlePackageType"
		       ,(if app? "APPL" "FMWK"))
	   (assoc-pair "CFBundleSignature"
		       ,signature)
	   (assoc-pair "CFBundleVersion"
		       ,(version))
	   (assoc-pair "CFBundleShortVersionString"
		       ,(version))
	   (assoc-pair "NSPrincipalClass" "NSApplicationMain")
	   (assoc-pair "NSHighResolutionCapable" (true))))

    (create-app (build-path (current-directory) (if for-3m? 'up 'same))
                (string-append "GRacket" suffix)
		"GRacket"
		"APPLmReD"
		(make-info-plist (string-append "GRacket" suffix) "mReD" #t))

    (create-app (build-path (current-directory) (if for-3m? 'up 'same))
                "Starter"
                "Starter"
                "APPLMrSt"
                (make-info-plist "Starter" "MrSt" #t)))
