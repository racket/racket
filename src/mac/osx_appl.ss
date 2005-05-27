#!/bin/sh

#|

# OS X pre-make script
# builds resource files, makes template Starter.app and MrEd.app
#
# the script must be run from the mrd build directory,
# and srcdir must be provided as the first argument

PLTHOMEBASE="$1/.."
export PLTHOMEBASE
PLTHOME=
export PLTHOME
PLTCOLLECTS=
export PLTCOLLECTS
DYLD_FRAMEWORK_PATH=${BUILDBASE}/mzscheme:${DYLD_FRAMEWORK_PATH}
export DYLD_FRAMEWORK_PATH
shift 1
exec ${BUILDBASE}/mzscheme/mzscheme -xqr "$0"
echo "Couldn't start MzScheme!"
exit 1

|#

(use-compiled-file-paths null)

(let ([p (getenv "PLTHOMEBASE")])
  (let ([plthome (path->complete-path p)])
    (putenv "PLTHOME" (path->string plthome))
    (current-library-collection-paths (list (build-path plthome "collects")))))

(module osx_appl mzscheme

  (require (lib "plist.ss" "xml")
	   (lib "process.ss")
	   (lib "file.ss"))

  (define rez-path (or (getenv "REZ")
		       "/Developer/Tools/Rez"))

  ; set plthome:
  (define plthome (getenv "PLTHOME"))
  (printf "plthome is ~s~n" plthome)

  (define for-3m? (getenv "BUILDING_3M"))

  ; Rez where needed:
  (let* ([cw-path (build-path plthome "src" "mac" "cw")]
	 [rez-it (lambda (app)
		   (printf "Writing ~a~n" (string-append app ".rsrc.OSX"))
		   (system* rez-path 
			    (path->string (build-path cw-path (string-append app ".r")))
			    "-UseDF" "-o" 
			    (path->string
			     (path-replace-suffix app #".rsrc.OSX"))))])
    ; (rez-it "MzScheme") ; useless under OS X...
    (rez-it "MrEd"))

  ; make .app templates in the right places:

  (define (realize-template path template-tree)
    (let* ([head-path (build-path path (car template-tree))])
      (when (file-exists? head-path)
	    (error 'realize-template 
		   "Can't create directory ~s because there's a file with that name" 
		   head-path))
      (unless (directory-exists? head-path)
	      (printf "Creating directory: ~s~n" head-path)
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
      (printf "writing file ~s~n" info-plist-path)
      (call-with-output-file info-plist-path
	(lambda (port)
	  (write-plist info-plist port))
	'truncate)))

  (define (create-app dest-path app-name icon-src-name pkg-info-string info-plist)
    (let* ([app-path (build-path dest-path 
				 (string-append app-name ".app"))])
      (make-directory* app-path)
      (realize-template app-path app-template-tree)
      (let* ([pkg-info-path (build-path app-path "Contents" "PkgInfo")])
	(printf "writing file ~s~n" pkg-info-path)
	(call-with-output-file pkg-info-path
	  (lambda (port)
	    (fprintf port pkg-info-string))
	  'truncate))
      (let* ([contents-path (build-path app-path "Contents")])
	(write-info contents-path info-plist)
	(let* ([icns-src (build-path plthome "src" "mac" "icon" (path-replace-suffix icon-src-name #".icns"))]
	       [icns-dest (build-path contents-path "Resources" (path-replace-suffix (file-name-from-path app-name) #".icns"))])
	  (unless (file-exists? icns-dest)
	    (copy-file icns-src icns-dest))))))
  
  (define (create-fw dest-path fw-name info-plist)
    (let* ([fw-path (build-path dest-path 
				(string-append fw-name ".framework")
				"Versions"
				(if for-3m?
				    (format "~a_3m" (version))
				    (version)))])
      (make-directory* fw-path)
      (realize-template fw-path fw-template-tree)
      (write-info (build-path fw-path "Resources") info-plist)
      ;; maybe someday we'll have Contents/Resources/English.lproj ?
      (let* ([rsrc-src (build-path "MrEd.rsrc.OSX")]
	     [rsrc-dest (build-path fw-path "Resources" (format "~a.rsrc" fw-name))])
	(when (file-exists? rsrc-dest)
	  (delete-file rsrc-dest))
	(printf "Installing ~a~n" rsrc-dest)
	(copy-file rsrc-src rsrc-dest))))

  (define (make-info-plist app-name signature app?)
    `(dict (assoc-pair "CFBundleDevelopmentRegion"
		       "English")
	   (assoc-pair "CFBundleExecutable"
		       ,app-name)
	   (assoc-pair "CFBundleIdentifier"
		       ,(format "org.plt-scheme.~a" app-name))
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
		       ,(version))))

    (create-app (current-directory)
		(if for-3m? "../MrEd3m" "MrEd")
		"MrEd"
		"APPLmReD"
		(make-info-plist (if for-3m? "MrEd3m" "MrEd") "mReD" #t))

    (create-fw (current-directory)
	       "PLT_MrEd"
		(make-info-plist "PLT_MrEd" "MrEd" #f))

    (create-app (current-directory)
		(if for-3m? "../Starter3m" "Starter")
		"Starter"
		"APPLMrSt"
		(make-info-plist (if for-3m? "Starter3m" "Starter") "MrSt" #t)))

(require osx_appl)

