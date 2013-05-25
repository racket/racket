#lang at-exp racket/base
(require racket/system
         racket/file
         racket/format
         racket/runtime-path)

(provide installer-dmg)

(define hdiutil "/usr/bin/hdiutil")

(define-runtime-path bg-image "macosx-installer/racket-rising.png")

(define (system*/show . l)
  (displayln (apply ~a #:separator " " l))
  (unless (apply system* l)
    (error "failed")))

(define (make-dmg volname src-dir dmg bg)
  (define tmp-dmg (make-temporary-file "~a.dmg"))
  (define work-dir
    (let-values ([(base name dir?) (split-path src-dir)])
      (build-path base "work")))
  (when (file-exists? dmg) (delete-file dmg))
  (delete-directory/files work-dir #:must-exist? #f)
  (make-directory* work-dir)
  (printf "Copying ~a\n" src-dir)
  (copy-directory/files src-dir (build-path work-dir volname)
                        #:keep-modify-seconds? #t)
  (when bg
    (copy-file bg (build-path work-dir "bg.png")))
  ;; The following command should work fine, but it looks like hdiutil in 10.4
  ;; is miscalculating the needed size, making it too big in our case (and too
  ;; small with >8GB images).  It seems that it works to first generate an
  ;; uncompressed image and then convert it to a compressed one.
  ;;   hdiutil create -format UDZO -imagekey zlib-level=9 -ov \
  ;;           -mode 555 -volname volname -srcfolder . dmg
  ;; So, first create an uncompressed image...
  (parameterize ([current-directory work-dir])
    (system*/show hdiutil
                  "create" "-format" "UDRW" "-ov"
                  "-mode" "755" "-volname" volname "-srcfolder" "."
                  tmp-dmg))
  ;; Then do the expected dmg layout...
  (when bg
    (dmg-layout tmp-dmg volname "bg.png"))
  ;; And create the compressed image from the uncompressed image:
  (system*/show hdiutil
                "convert" "-format" "UDBZ" "-imagekey" "zlib-level=9" "-ov"
                tmp-dmg "-o" dmg)
  (delete-file tmp-dmg))

(define (dmg-layout dmg volname bg)
  (define mnt (make-temporary-file "~a-mnt" 'directory))
  (system*/show hdiutil
                "attach" "-readwrite" "-noverify" "-noautoopen"
                "-mountpoint" mnt dmg)
  (define mnt-name (let-values ([(base name dir?) (split-path mnt)]) (path->string name)))
  ;; see also https://github.com/andreyvit/yoursway-create-dmg
  (define script
    @~a{
	tell application "Finder"
	  -- look for a single disk with the mount point as its name
	  -- (maybe this works only on newer osx versions?)
	  set myDisks to every disk
	  set theDMGDisk to ""
	  repeat with d in myDisks
	    if name of d = "@mnt-name"
	      if theDMGDisk = ""
	        set theDMGDisk to d
	      else
	        error "Too many attached DMGs found!"
	      end if
	    end if
	  end repeat
	  if theDMGDisk = "" then error "Attached DMG not found!"
	  -- found a single matching disk, continue
	  tell theDMGDisk
	    open
	    set current view of container window to icon view
	    set toolbar visible of container window to false
	    set statusbar visible of container window to false
	    set bounds of container window to {320, 160, 1000, 540}
	    set theViewOptions to the icon view options of container window
	    set arrangement of theViewOptions to not arranged
	    set icon size of theViewOptions to 128
	    set text size of theViewOptions to 16
	    set background picture of theViewOptions to file "@bg"
	    make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
	    set position of item "@volname" of container window to {170, 180}
	    set position of item "@bg" of container window to {900, 180}
	    set position of item "Applications" of container window to {500, 180}
	    set name of file "@bg" to ".@bg"
	    close
	    open
	    update without registering applications
	    delay 5
	    close
	  end tell
	end tell
     })
  (printf "~a\n" script)
  (parameterize ([current-input-port (open-input-string script)])
    (system* "/usr/bin/osascript"))
  (system*/show "/bin/sync")
  (system*/show "/bin/sync")
  (system*/show "/bin/sync")
  (system*/show "/bin/sync")
  (system*/show hdiutil "detach" mnt)
  (delete-directory mnt))

(define (installer-dmg human-name dir-name)
  (define dmg-name (format "bundle/~a-~a.dmg" dir-name (system-library-subpath #f)))
  (make-dmg human-name "bundle/racket" dmg-name bg-image)
  dmg-name)
