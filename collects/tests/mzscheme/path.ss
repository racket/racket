
(load-relative "loadtest.ss")

(Section 'path)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-/tf p exn?)
  (lambda args
    (with-handlers ([exn? (lambda (x) #f)]
		    [void (lambda (x) 'wrong-exn)])
      (if (void? (apply p args))
	  #t
	  'not-void))))
(define delete-file/tf (lambda (x) ((make-/tf delete-file exn:fail:filesystem?) x)))
(define delete-directory/tf (lambda (x) ((make-/tf delete-directory exn:fail:filesystem?) x)))
(define rename-file-or-directory/tf (lambda (x y) ((make-/tf rename-file-or-directory exn:fail:filesystem?) x y)))
(define make-directory/tf (lambda (x) ((make-/tf make-directory exn:fail:filesystem?) x)))
(define copy-file/tf (lambda (x y) ((make-/tf copy-file exn:fail:filesystem?) x y)))

(test #f relative-path? (current-directory))
(test #t relative-path? "down")
(test #t relative-path? (build-path 'up "down"))
(test #t relative-path? (build-path 'same "down"))
(test #t relative-path? (build-path 'same "down" "deep"))
(test #f relative-path? (build-path (current-directory) 'up "down"))
(test #f relative-path? (build-path (current-directory) 'same "down"))
(test #f relative-path? (build-path (current-directory) 'same "down" "deep"))
(test #f relative-path? (string #\a #\nul #\b))

(arity-test relative-path? 1 1)
(err/rt-test (relative-path? 'a))

(test #t absolute-path? (current-directory))
(test #f absolute-path? (build-path 'up))
(test #f absolute-path? (string #\a #\nul #\b))

(arity-test absolute-path? 1 1)
(err/rt-test (absolute-path? 'a))

(test #t complete-path? (current-directory))
(test #f complete-path? (build-path 'up))
(test #f complete-path? (string #\a #\nul #\b))

(arity-test complete-path? 1 1)
(err/rt-test (complete-path? 'a))

(call-with-output-file "tmp6" void 'replace)
(define existant "tmp6")

(test #t file-exists? existant)

(define deepdir (build-path "down" "deep"))

(when (directory-exists? deepdir) 
  (for-each delete-file (directory-list deepdir))
  (delete-directory deepdir))
(when (directory-exists? "down")
  (for-each delete-file (directory-list "down"))
  (delete-directory "down"))
  
(test #t make-directory/tf "down")
(test #f make-directory/tf "down")
(test #t directory-exists? "down")
(test #f file-exists? "down")

(test #t make-directory/tf deepdir)
(test #f make-directory/tf deepdir)
(test #t directory-exists? deepdir)
(test #f file-exists? deepdir)

(test #t file-exists? (build-path "down" 'up existant))
(test #t file-exists? (build-path deepdir 'up 'up existant))
(test #t file-exists? (build-path 'same deepdir 'same 'up 'same 'up existant))

(test #f file-exists? (build-path "down" existant))
(test #f file-exists? (build-path deepdir 'up existant))
(test #f file-exists? (build-path 'same deepdir 'same 'same 'up existant))

(delete-file "tmp6")

(test #f file-exists? (build-path "down" 'up "badfile"))
(test #f file-exists? (build-path deepdir 'up 'up "badfile"))
(test #f file-exists? (build-path 'same deepdir 'same 'up 'same 'up "badfile"))

(err/rt-test (open-output-file (build-path "wrong" "down" "tmp8"))
	    exn:fail:filesystem?)
(err/rt-test (open-output-file (build-path deepdir "wrong" "tmp7"))
	    exn:fail:filesystem?)

(define start-time (current-seconds))
(let ([p (open-output-file "tmp5" 'replace)])
  (display "123456789" p)
  (close-output-port p))
(close-output-port (open-output-file (build-path "down" "tmp8") 'replace))
(close-output-port (open-output-file (build-path deepdir "tmp7") 'replace))
(define end-time (current-seconds))

(map
 (lambda (f)
   (let ([time (seconds->date (file-or-directory-modify-seconds f))]
	 [start (seconds->date start-time)]
	 [end (seconds->date end-time)])
     (test #t = (date-year start) (date-year time) (date-year end))
     (test #t = (date-month start) (date-month time) (date-month end))
     (test #t = (date-day start) (date-day time) (date-day end))
     (test #t = (date-week-day start) (date-week-day time) (date-week-day end))
     (test #t = (date-year-day start) (date-year-day time) (date-year-day end))
     (test #t = (date-hour start) (date-hour time) (date-hour end))
     (test #t <= (date-minute start) (date-minute time) (date-minute end))
     (test #t <= (- (date-second start) 5) (date-second time) (+ (date-second end) 5))))
 (list "tmp5"
       "down"
       (build-path "down" "tmp8")
       (build-path deepdir "tmp7")))

(test 'no-exists 'no-file-for-seconds (with-handlers ([void (lambda (x) 'no-exists)]) (file-or-directory-modify-seconds "non-existent-file")))
(map
 (lambda (f)
   (test #t number? (file-or-directory-modify-seconds f)))
 (filesystem-root-list))

(test #t file-exists? "tmp5")
(test #t file-exists? (build-path "down" "tmp8"))
(test #t file-exists? (build-path deepdir "tmp7"))

(test #t copy-file/tf "tmp5" "tmp5y")
(test #f copy-file/tf "tmp5" "tmp5y")
(test #f copy-file/tf "tmp5" "down")
(test #f copy-file/tf "tmp5" (build-path deepdir "moredeep" "tmp5y"))
(test (file-size "tmp5") file-size "tmp5y")
(delete-file "tmp5y")

(test #t rename-file-or-directory/tf "tmp5" "tmp5x")
(test #f rename-file-or-directory/tf "tmp5" "tmp5x")
(close-output-port (open-output-file "tmp5"))
(test #t file-exists? "tmp5")
(test #t file-exists? "tmp5x")
(test #f rename-file-or-directory/tf "tmp5" "tmp5x")
(test #f rename-file-or-directory/tf "tmp5" "down")
(delete-file "tmp5")
(test #f file-exists? "tmp5")
(test #t rename-file-or-directory/tf (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #f rename-file-or-directory/tf (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #t rename-file-or-directory/tf (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))
(test #f rename-file-or-directory/tf (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))

(test #t make-directory/tf "downx")
(test #f rename-file-or-directory/tf "down" "downx")
(test #t delete-directory/tf "downx")

(test #t rename-file-or-directory/tf "down" "downx")
(test #t directory-exists? "downx")
(test #f directory-exists? "down")
(test #t file-exists? (build-path "downx" "tmp8x"))
(test #f file-exists? (build-path "down" "tmp8x"))
(test #f rename-file-or-directory/tf "down" "downx")
(test #t rename-file-or-directory/tf "downx" "down")
(test #t file-exists? (build-path "down" "tmp8x"))

(test #t rename-file-or-directory/tf (build-path deepdir "tmp7x") "tmp7x")
(test #f rename-file-or-directory/tf (build-path deepdir "tmp7x") "tmp7x")
(test #t rename-file-or-directory/tf "tmp7x" (build-path deepdir "tmp7x"))
(test #f rename-file-or-directory/tf "tmp7x" (build-path deepdir "tmp7x"))

(test #f not (member (bytes->path #"tmp5x") (directory-list)))
(test #t 'directory-list 
      (let ([l (directory-list "down")])
	(or (equal? l (map bytes->path '(#"deep" #"tmp8x")))
	    (equal? l (map bytes->path '(#"tmp8x" #"deep"))))))
(test (list (bytes->path #"tmp7x")) directory-list deepdir)

(test #f delete-directory/tf deepdir)
(test #f delete-directory/tf "down")

(test #t delete-file/tf (build-path deepdir "tmp7x"))
(test #f delete-file/tf (build-path deepdir "tmp7x"))
(test #t delete-file/tf (build-path "down" "tmp8x"))
(test #f delete-file/tf (build-path "down" "tmp8x"))
(test #t delete-file/tf "tmp5x")
(test #f delete-file/tf "tmp5x")

(test #f delete-directory/tf "down")
(test #t delete-directory/tf deepdir)
(test #f delete-directory/tf deepdir)
(test #t delete-directory/tf "down")
(test #f delete-directory/tf "down")

; Redefine these per-platform
(define drives null)
(define nondrive-roots (list "/"))
(define -a (list "a"))
(define a/b (list "a/b" "a//b"))
(define a/b/c (list "a/b/c" "a//b/c"))
(define /a/b (list "/a/b"))
(define a/../b (list "a/../b"))
(define a/./b (list "a/./b"))
(define a/../../b (list "a/../../b"))
(define trail-sep "/")

(define add-slashes
  (lambda (l)
    (if (null? l)
	null
	(let loop ([s (car l)][rest (add-slashes (cdr l))])
	  (let ([naya (regexp-replace "/" s "\\")])
	    (if (string=? naya s)
		(cons s rest)
		(loop naya (cons s rest))))))))

(when (eq? (system-type) 'windows)
      (set! drives (list "c:" "c:/" "//hello/start" "//hello/start/"))
      (set! nondrive-roots null)
      (for-each
       (lambda (var)
	 (eval `(set! ,var (add-slashes ,var))))
       '(-a a/b a/b/c /a/b a/../b a/./b a/../../b)))


(when (eq? (system-type) 'macos)
      (set! drives null)
      (set! nondrive-roots (filesystem-root-list))
      (set! -a (list ":a"))
	  (set! a/b (list ":a:b"))
	  (set! a/b/c (list ":a:b:c"))
      (set! /a/b (list "a:b"))
      (set! a/../b (list ":a::b"))
      (set! a/./b null)
	  (set! a/../../b (list ":a:::b"))
	  (set! trail-sep ":"))

(define roots (append drives nondrive-roots))

(define a/ (map (lambda (s) (string-append s trail-sep)) -a))
(define a/b/ (map (lambda (s) (string-append s trail-sep)) a/b))
(define a/b/c/ (map (lambda (s) (string-append s trail-sep)) a/b/c))
(define /a/b/ (map (lambda (s) (string-append s trail-sep)) /a/b))

(define absols (append roots /a/b /a/b/))
(define nondrive-absols (append nondrive-roots /a/b /a/b/))
(define rels (append -a a/ a/b a/b/ a/b/c a/b/c/ a/../b a/./b a/../../b))

(define i (lambda (x) x))

(test #f ormap i (map relative-path? roots))
(test #t andmap i (map relative-path? a/b))
(test #f ormap i (map relative-path? /a/b))

(test #t andmap i (map absolute-path? roots))
(test #f ormap i (map absolute-path? a/b))

(test #t andmap i (map complete-path? drives))
(test #t andmap i (map complete-path? nondrive-roots))
(test #f ormap i (map complete-path? a/b))

(for-each
 (lambda (abs)
   (for-each
    (lambda (rel)
      (test #t path? (build-path abs rel))
      (for-each
       (lambda (rel2)
	 (test #t path? (build-path abs rel rel2)))
       rels))
    rels))
 absols)

(for-each
 (lambda (drive)
   (for-each
    (lambda (root)
      (test #t path? (build-path drive root))
      (for-each
       (lambda (rel)
	 (test #t path? (build-path drive root rel)))
       rels))
    nondrive-absols))
 drives)

(for-each
 (lambda (rel)
   (test (build-path (current-directory) rel)
	 path->complete-path rel))
 rels)

(define (test-path expect f . args)
  (test (normal-case-path (expand-path expect))
	(or (object-name f) 'unknown)
	(normal-case-path (expand-path (apply f args)))))

(for-each
 (lambda (absol)
   (let ([cabsol (path->complete-path absol)])
     (for-each
      (lambda (rel)
	(test-path (build-path cabsol rel) path->complete-path rel cabsol)
	(test-path (build-path cabsol rel rel) path->complete-path rel (build-path cabsol rel))
	(err/rt-test (path->complete-path rel rel) exn:fail:contract?))
      rels)))
 absols)

(for-each
 (lambda (drive)
   (for-each
    (lambda (rel)
      (unless (relative-path? rel)
	      (test-path (build-path (current-drive) rel)
			 path->complete-path rel))
      (test-path (build-path drive rel) path->complete-path rel drive)
      (test-path (if (relative-path? rel)
		     (build-path drive rel rel)
		     (build-path drive rel))
		 path->complete-path rel (build-path drive rel)))
    (append rels nondrive-absols)))
 drives)

(for-each
 (lambda (drive)
   (test (string->path drive) path->complete-path drive)
   (test (string->path drive) path->complete-path drive drive))
 drives)

(unless (eq? (system-type) 'macos)
 (for-each
  (lambda (abs1)
    (for-each
     (lambda (abs2)
       (err/rt-test (build-path abs1 abs2) exn:fail:contract?))
     absols))
  nondrive-roots))

(for-each
 (lambda (root)
   (let-values ([(base name dir?) (split-path root)])
     (when (eq? (system-type) 'macos)
       (test root 'split-path name))
     (test #f 'split-path base)
     (test #t 'split-path dir?)))
 roots)

(let ([check-a/b
       (lambda (a/b end/?)
	 (for-each
	  (lambda (path)
	    (let*-values ([(base name dir?) (split-path path)]
			  [(base2 name2 dir?2) (split-path base)])
	       (test #"b" subbytes (path->bytes name) 0 1)
	       (test end/? 'split-path dir?)
	       (test #"a" subbytes (path->bytes name2) 0 1)
	       (test 'relative 'split-path base2)
	       (test #t 'split-path dir?2)
	       (for-each 
		(lambda (root)
		  (let ([bigpath (build-path root path)])
		    (let*-values ([(base name dir?) (split-path bigpath)]
				  [(base2 name2 dir?2) (split-path base)]
				  [(base3 name3 dir?3) (split-path base2)])
		       (test #f 'split-path base3)
		       (test #t 'split-path dir?3))))
		roots)))
	  a/b))])
  (check-a/b a/b #f)
  (check-a/b a/b/ #t))

(arity-test split-path 1 1)

(arity-test path->complete-path 1 2)
(err/rt-test (path->complete-path 1))
(err/rt-test (path->complete-path "a" 1))

(test-path (build-path "a" "b") simplify-path (build-path "a" "b"))
(let ([full-path
       (lambda args (apply build-path (current-directory) args))])
  (unless (equal? (build-path "a" "b") (build-path "a" 'same "b"))
    (test-path (full-path "a" "b") simplify-path (build-path "a" 'same "b")))
  (test-path (full-path "a" "b") simplify-path (build-path "a" 'same "noexistsdir" 'up "b"))
  (test-path (path->directory-path (full-path "a" "b")) 
             simplify-path (build-path "a" 'same "noexistsdir" 'same 'up "b" 'same 'same))
  (test-path (path->directory-path (full-path "a" "b"))
             simplify-path (build-path 'same "noexistsdir" 'same 'up "a" 'same "b" 'same 'same)))
(test (build-path "x" "y") simplify-path (build-path "x" "z" 'up "y") #f)
(test (build-path 'up "x" "y") simplify-path (build-path 'up "x" "z" 'up "y") #f)
(test (build-path 'up "x" "y") simplify-path (build-path 'up 'same "x" "z" 'up "y") #f)
(test (build-path 'up 'up "x" "y") simplify-path (build-path 'up 'same 'up "x" "z" 'up "y") #f)
(test (build-path 'same) simplify-path (build-path 'same) #f)
(test (build-path 'same) simplify-path (build-path 'same 'same 'same) #f)
(test (build-path 'same) simplify-path (build-path 'same "a" 'same 'up 'same) #f)
(arity-test simplify-path 1 2)

(arity-test expand-path 1 1)
(arity-test resolve-path 1 1)

(map
 (lambda (f)
   (err/rt-test (f (string #\a #\nul #\b)) exn:fail:contract?))
 (list build-path split-path file-exists? directory-exists?
       delete-file directory-list make-directory delete-directory
       file-or-directory-modify-seconds file-or-directory-permissions 
       expand-path resolve-path simplify-path path->complete-path
       open-input-file open-output-file))
(map 
 (lambda (f)
   (err/rt-test (f (string #\a #\nul #\b) "a") exn:fail:contract?)
   (err/rt-test (f "a" (string #\a #\nul #\b)) exn:fail:contract?))
 (list rename-file-or-directory path->complete-path))

; normal-case-path now checks for pathness:
(err/rt-test (normal-case-path (string #\a #\nul #\b)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; \\?\ paths in Windows

(when (eq? 'windows (system-type))
  (let ([here (regexp-replace
	       #rx"[\\]$"
	       (string-append
		"\\\\?\\"
		(path->string
		 (normal-case-path (simplify-path (expand-path (current-directory))))))
	       "")]
	[get-base (lambda (s)
		    (let-values ([(base name dir?) (split-path s)]) (list base name)))]
	[drive (path->string (current-drive))])

    (test #t directory-exists? here)
    (test #t directory-exists? (string-append here "\\")) ; trailing separator is ok for dir
    (test #f directory-exists? (string-append here "\\\\")) ; extra isn't ok

    (let ([dir (ormap (lambda (f)
			(and (directory-exists? f) f))
		      (directory-list))])
      (when dir
	(map (lambda (sep)
	       (let ([dirstr (string-append here sep (path->string dir))])
		 (test #t directory-exists? dirstr)
		 (test #t directory-exists? (string-append dirstr "\\"))
		 (test #f directory-exists? (string-append dirstr "\\\\"))
		 (test (file-or-directory-modify-seconds dir) file-or-directory-modify-seconds dirstr)
		 ))
	     '("\\" "\\\\"))))

    (let ([file (ormap (lambda (f)
			 (and (file-exists? f) f))
		       (directory-list))])
      (when file
	(map (lambda (sep)
	       (let ([filestr (string-append here sep (path->string file))])
		 (test #t file-exists? filestr)
		 (test #f file-exists? (string-append filestr "\\"))
		 (test (file-or-directory-modify-seconds file) file-or-directory-modify-seconds filestr)
		 (test (file-size file) file-size filestr)
		 ))
	     '("\\" "\\\\"))))

    ;; Check drive and path->complete-path:
    (parameterize ([current-directory "\\\\?\\"])
      (test (string->path "\\\\?\\") current-drive)
      (test (string->path "\\\\?\\\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\x\\y\\"])
      (test (string->path "\\\\?\\") current-drive)
      (test (string->path "\\\\?\\\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\d:\\foo"])
      ;; because it simplifies, \\?\ goes away
      (test (string->path "d:\\") current-drive)
      (test (string->path "d:\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\UNC\\foo\\bar\\baz"])
      ;; because it simplifies, \\?\ goes away
      (test (string->path "\\\\foo\\bar") current-drive)
      (test (string->path "\\\\foo\\bar\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\REL\\"])
      (test (string->path "\\\\?\\") current-drive)
      (test (string->path "\\\\?\\\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\REL\\a\\\\"])
      (test (string->path "\\\\?\\REL\\a\\\\") current-drive)
      (test (string->path "\\\\?\\REL\\a\\\\\\a") path->complete-path "\\a")
      )

    (test (bytes->path #"c:\\a") path->complete-path "\\a" "c:")
    (test (bytes->path #"\\\\foo\\bar\\a") path->complete-path "\\a" "\\\\foo\\bar\\a")

    ;; Build path collapses .. and . in added parts and cleans up slashes and trailers:
    (test (string->path "\\\\?\\c:\\a\\b") build-path "\\\\?\\c:\\a" 'same "b")
    (test (string->path "\\\\?\\c:\\a\\b") build-path "\\\\?\\c:\\a\\" 'same "b")
    (test (string->path "\\\\?\\c:\\a\\.\\b") build-path "\\\\?\\c:\\a\\." 'same "b")
    (test (string->path "\\\\?\\c:\\a\\.\\b") build-path "\\\\?\\c:\\a\\.\\" 'same "b")
    (test (string->path "\\\\?\\c:\\b") build-path "\\\\?\\c:\\a\\" 'up "b")
    (test (string->path "\\\\?\\c:\\a\\b") build-path "\\\\?\\c:\\a\\.\\" 'up "b")
    (test (string->path "\\\\?\\c:\\b") build-path "\\\\?\\c:\\a\\.\\" 'up 'up 'up "b")
    (test (string->path "\\\\?\\c:\\a\\b\\c\\d\\f\\") build-path "\\\\?\\c:\\a" "b//c" "d//\\f///")
    (test (string->path "\\\\?\\c:\\a\\foo") build-path "\\\\?\\c:\\a" "foo...  ")

    (test (string->path "\\\\?\\REL\\\\a") build-path "." "\\\\?\\REL\\a")
    (test (string->path "\\\\?\\REL\\\\a") build-path 'same "\\\\?\\REL\\a")
    (test (string->path "\\\\?\\REL\\\\a\\") build-path "\\\\?\\REL\\a" 'same)
    (test (string->path ".") build-path "\\\\?\\REL\\a" 'up)
    (test (string->path "\\\\?\\REL\\\\apple") build-path "\\\\?\\REL\\a" 'up "apple")
    (test (string->path ".") build-path "\\\\?\\REL\\a" 'up 'same)

    (test (string->path "\\\\?\\REL\\..\\\\a") build-path 'up "\\\\?\\REL\\a")
    (test (string->path "\\\\?\\REL\\..\\\\..") build-path 'up "\\\\?\\REL\\\\..")
    (test (string->path "\\\\?\\REL\\..\\..") build-path 'up "\\\\?\\REL\\..")
    (test (string->path "\\\\?\\REL\\..\\..") build-path "\\\\?\\REL\\.." 'up)
    (test (string->path "\\\\?\\REL\\..\\..\\..") build-path 'up "\\\\?\\REL\\.." 'up)
    (test (string->path "\\\\?\\REL\\..\\..\\\\..") build-path 'up "\\\\?\\REL\\.." "\\\\?\\REL\\\\..")
    (test (string->path "\\\\?\\REL\\..\\..") build-path 'up "\\\\?\\REL\\.." "\\\\?\\REL\\\\.." 'up)
    (test (string->path "\\\\?\\REL\\\\..\\") build-path "\\\\?\\REL\\\\.." "\\\\?\\REL\\\\.." 'up)
    (test (string->path "\\\\?\\REL\\\\x\\") build-path "x/y" "\\\\?\\REL\\..")
    (test (string->path "\\\\?\\c:\\x\\") build-path "c:x/y" "\\\\?\\REL\\..")

    (test-values (list (string->path "\\\\?\\REL\\..")
		       (string->path "\\\\?\\REL\\\\a")
		       #f)
		 (lambda () (split-path "\\\\?\\REL\\..\\a")))
    (test-values (list (string->path "\\\\?\\REL\\..")
		       (string->path "\\\\?\\REL\\\\a")
		       #f)
		 (lambda () (split-path "\\\\?\\REL\\..\\\\a")))
    (test-values (list (string->path "\\\\?\\REL\\b\\")
		       (string->path "\\\\?\\REL\\\\a")
		       #f)
		 (lambda () (split-path "\\\\?\\REL\\b\\a")))

    (test (string->path "\\\\?\\UNC\\f\\g") build-path "//f/g/h" "\\\\?\\REL\\..")
    (test (string->path "\\\\?\\UNC\\f\\g\\h\\") build-path "//f/g/h/i" "\\\\?\\REL\\..")
    (test (string->path "\\\\?\\UNC\\f\\g\\h\\..") build-path "//f/g/h" "\\\\?\\REL\\\\..")
    (test (string->path "\\\\?\\UNC\\f\\g\\h\\..") build-path "//f/g//h" "\\\\?\\REL\\\\..")
    (test (string->path "\\\\?\\UNC\\f\\g\\i\\x") build-path "//f/g/h/../i" "\\\\?\\REL\\x")
    (test (string->path "\\\\?\\UNC\\f\\g\\h\\i\\x") build-path "//f/g/../h/i" "\\\\?\\REL\\x")

    (test (string->path (string-append "\\\\?\\" drive "f\\g\\")) build-path "/f/g/h" "\\\\?\\REL\\..")
    (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x")) build-path "/f/g/h" "\\\\?\\REL\\x")
    (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x")) build-path "//f//g/h" "\\\\?\\REL\\x")
    (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x")) build-path "/f//g////h" "\\\\?\\REL\\x")
    (test (string->path (string-append "\\\\?\\" drive)) build-path "/f//g////h" "\\\\?\\REL\\..\\..\\..\\..")
    (test (string->path (string-append "\\\\?\\" drive "?\\g\\h\\x")) build-path "//?/g/h" "\\\\?\\REL\\x")

    ;; Allow \\?\ as a drive to add an absolute path:
    (test (string->path "\\\\?\\c:\\b") build-path "\\\\?\\c:\\" "\\b")
    (test (string->path "\\\\?\\c:\\b") build-path "\\\\?\\c:\\\\" "\\b")
    (test (string->path "\\\\?\\c:\\b\\") build-path "\\\\?\\c:\\\\" "\\b\\")
    (test (string->path "\\\\?\\UNC\\goo\\bar\\b") build-path "\\\\?\\UNC\\goo\\bar" "\\b")
    (test (string->path "\\\\?\\\\b") build-path "\\\\?\\" "\\b")
    (test (string->path "\\\\?\\\\b\\") build-path "\\\\?\\" "\\b\\")
    (err/rt-test (build-path "\\\\?\\c:" "\\b") exn:fail:contract?)
    
    ;; Don't allow path addition on bad \\?\ to change the root:
    (test (string->path "\\\\?\\\\c") build-path "\\\\?\\" "c")
    (test (string->path "\\\\?\\\\UNC") build-path "\\\\?\\" "UNC")
    (test (string->path "\\\\?\\\\UNC\\s\\y") build-path "\\\\?\\UNC" "s/y")
    (test (string->path "\\\\?\\\\UNC\\s\\y") build-path "\\\\?\\UNC\\" "s/y")
    (test (string->path "\\\\?\\\\REL\\s\\y") build-path "\\\\?\\REL" "s/y")
    (test (string->path "\\\\?\\\\REL\\s\\y") build-path "\\\\?\\REL\\" "s/y")
    (test (string->path "\\\\?\\REL\\\\\\s\\y") build-path "\\\\?\\REL\\\\" "s/y")
    (test (string->path "\\\\?\\REL\\x\\\\\\z") build-path "\\\\?\\REL\\x\\\\" "z")
    (test (string->path "/apple\\x") build-path "//apple" "x")
    (test (string->path "\\\\?") build-path "\\\\" "?")
    (test (string->path "\\?\\") build-path "\\\\" "?\\")
    (test (string->path "\\?\\a") build-path "\\\\" "?" "a")
    (test (string->path "\\?\\a") build-path "\\\\?" "a")
    (test (string->path "\\?\\a\\") build-path "\\\\?" "a\\")
    (test (string->path "\\\\?\\\\c:") build-path "\\\\?\\" "\\\\?\\REL\\c:")
    (test (string->path "\\\\?\\\\c:\\a") build-path "\\\\?\\" "\\\\?\\REL\\c:\\a")
    (test (string->path "\\\\?\\\\REL\\b") build-path "\\\\?\\" "\\\\?\\REL\\REL\\b")
    (test (string->path "\\\\?\\\\host\\vol\\a\\") build-path "\\\\?\\" "\\\\?\\REL\\\\host\\vol\\a\\")

    ;; UNC paths can't have "?" for machine or "/" in machine part:
    (test (list (string->path "/?/") (string->path "x")) get-base "//?/x")
    (test (list (string->path "\\\\?\\UNC\\a/b\\") (string->path "x")) get-base "\\\\?\\UNC\\a/b\\x")

    ;; Split path must treat \\?\ part as a root:
    (test (list (string->path "\\\\?\\c:\\") (string->path "a")) get-base "\\\\?\\c:\\a")
    (test (list (string->path "\\\\?\\") (string->path "\\\\?\\REL\\\\c:")) get-base "\\\\?\\c:")
    (test (list #f (string->path "\\\\?\\c:\\")) get-base "\\\\?\\c:\\")
    (test (list #f (string->path "\\\\?\\c:\\\\")) get-base "\\\\?\\c:\\\\")
    (test (list (string->path "\\\\?\\c:\\") (string->path "a")) get-base "\\\\?\\c:\\\\a")
    (test (list #f (string->path "\\\\?\\UNC\\mach\\vol")) get-base "\\\\?\\UNC\\mach\\vol")
    (test (list #f (string->path "\\\\?\\UNC\\mach\\\\vol")) get-base "\\\\?\\UNC\\mach\\\\vol")
    (test (list #f (string->path "\\\\?\\UNC\\\\mach\\vol")) get-base "\\\\?\\UNC\\\\mach\\vol")
    (test (list (string->path "\\\\?\\") (string->path "c")) get-base "\\\\?\\c")
    (test (list (string->path "\\\\?\\UNC\\") (string->path "x")) get-base "\\\\?\\UNC\\x")
    (test (list (string->path "\\\\?\\") (string->path "UNC")) get-base "\\\\?\\UNC\\")
    (test (list #f (string->path "\\\\?\\UNC\\\\")) get-base "\\\\?\\UNC\\\\")
    (test (list #f (string->path "\\\\?\\xyz\\\\")) get-base "\\\\?\\xyz\\\\")
    (test (list (string->path "\\\\?\\c:\\a\\\\") (string->path "b")) get-base "\\\\?\\c:\\a\\\\\\b\\")
    (test (list #f (string->path "\\\\?\\c:\\a\\\\\\")) get-base "\\\\?\\c:\\a\\\\\\")
    (test (list (string->path "\\\\?\\UNC\\") (string->path "\\\\?\\REL\\\\x/y")) get-base "\\\\?\\UNC\\x/y")
    (test (list #f (string->path "\\\\?\\UNC\\x\\y")) get-base "\\\\?\\UNC\\x\\y")
    (test (list (string->path "\\\\?\\REL\\\\x\\y ") (string->path "z")) get-base "x/y /z")
    (test (list (string->path "\\\\?\\REL\\\\y ") (string->path "z")) get-base "x/../y /z")
    (test (list (string->path "\\\\?\\REL\\..\\\\y ") (string->path "z")) get-base "../y /z")
    (test (list (string->path "\\\\?\\c:\\y ") (string->path "z")) get-base "c:/y /z")
    (test (list (string->path "\\\\?\\c:\\y ") (string->path "z")) get-base "c:/../y /z")
    (test (list (string->path "../aux/") (string->path "z")) get-base "../aux/z")
    (test (list (string->path "../aux.m/") (string->path "z")) get-base "../aux.m/z")
    (test (list (string->path "..") (string->path "\\\\?\\REL\\\\aux.m")) get-base "../aux.m/")
    (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux.m")) get-base "c:/aux.m/")
    (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux.m.p")) get-base "c:/aux.m.p/")
    (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux:m")) get-base "c:/aux:m/")
    (test (list (string->path "..") (string->path "aux.m")) get-base "../aux.m")
    
    ;; simplify-path leaves literal . and .. alone:
    (test (string->path "\\\\?\\c:\\b\\.\\..\\a") simplify-path "\\\\?\\c:\\b\\.\\..\\a")
    (test (string->path "\\\\?\\c:\\B\\.\\..\\a") normal-case-path "\\\\?\\c:\\B\\.\\..\\a")
    (test (string->path "\\\\?\\UNC\\foo\\A") normal-case-path "\\\\?\\UNC\\foo\\A")

    ;; expand-path removes redundant backslashes, and
    (test (string->path "\\\\?\\\\UNC\\x\\y") expand-path "\\\\?\\\\UNC\\x\\y")
    (test (string->path "\\\\?\\c:\\") expand-path "\\\\?\\c:\\\\")

    ;; expand-path removes redundant backslashes, and
    ;; simplify-path uses expand-path under Windows:
    (let ([go
           (lambda (expand-path)
             (test (string->path "c:\\") expand-path "c:")
             (test (string->path "C:/") expand-path "C://")
             (test (string->path "C:/a/") expand-path "C://a//")
             (test (string->path "\\\\?\\c:\\a\\.") expand-path "\\\\?\\c:\\\\a\\\\.")
             (test (string->path "\\\\?\\c:\\a\\\\") expand-path "\\\\?\\c:\\a\\\\")
             (test (string->path "\\\\?\\c:\\a\\.") expand-path "\\\\?\\c:\\a\\\\.")
             (test (string->path "\\\\?\\UNC\\a\\b\\.") expand-path "\\\\?\\UNC\\\\a\\b\\.")
             (test (string->path "\\\\?\\UNC\\a\\b\\.") expand-path "\\\\?\\UNC\\\\a\\b\\\\.")
             (test (string->path "\\\\?\\") expand-path "\\\\?\\\\"))])
      (go expand-path)
      (go simplify-path))

    (test (bytes->path #"..") simplify-path "\\\\?\\REL\\..")
    (test (bytes->path #"..") simplify-path "\\\\?\\REL\\..\\")
    (test (bytes->path #"\\\\foo\\bar\\") expand-path "\\\\foo\\bar\\")
    (test (bytes->path #"\\\\foo\\bar") simplify-path "\\\\foo\\bar\\")
    (test (bytes->path #"\\\\foo\\bar") simplify-path "\\\\?\\UNC\\foo\\bar")
    (test (bytes->path #"\\\\foo\\bar") simplify-path "\\\\?\\UNC\\foo\\bar\\")
    (test (bytes->path #"\\\\?\\UNC\\foo\\bar\\..") simplify-path "\\\\?\\UNC\\foo\\bar\\..")
    (test (bytes->path #"\\\\?\\UNC\\foo\\bar\\..\\") simplify-path "\\\\?\\UNC\\foo\\bar\\..\\")
    (test (bytes->path #"a") simplify-path "\\\\?\\REL\\a")
    (test (bytes->path #"a") simplify-path "\\\\?\\REL\\\\a")
    (test (bytes->path #"a\\") simplify-path "\\\\?\\REL\\\\a\\")
    (test (bytes->path #"\\\\?\\REL\\\\a/") simplify-path "\\\\?\\REL\\\\a/")
    (test (bytes->path #"\\\\?\\REL\\\\..") simplify-path "\\\\?\\REL\\\\..")
    (test (bytes->path #"\\\\?\\REL\\\\..\\") simplify-path "\\\\?\\REL\\\\..\\")
    (test (bytes->path #"a \\b") simplify-path "\\\\?\\REL\\\\a \\b")
    (test (bytes->path #"\\\\?\\REL\\\\aux.bad\\b") simplify-path "\\\\?\\REL\\aux.bad\\b")
    (test (bytes->path #"\\\\?\\REL\\\\a\\b  ") simplify-path "\\\\?\\REL\\a\\b  ")
    (test (bytes->path #"\\\\?\\REL\\\\.\\b") simplify-path "\\\\?\\REL\\.\\b")
    (test (bytes->path #"\\\\?\\REL\\\\.") simplify-path "\\\\?\\REL\\.")
    (test (bytes->path #"\\\\?\\REL\\\\:\\b") simplify-path "\\\\?\\REL\\:\\b")
    (test (bytes->path #"\\\\?\\REL\\\\:") simplify-path "\\\\?\\REL\\:")
    (test (bytes->path #"\\\\?\\REL") simplify-path "\\\\?\\REL")
    (test (bytes->path #"C:\\a\\b") simplify-path "\\\\?\\C:\\a\\b")
    (test (bytes->path #"C:\\a") simplify-path "\\\\?\\C:\\a")
    (test (bytes->path #"\\\\?\\C:\\a ") simplify-path "\\\\?\\C:\\a ")
    (test (bytes->path #"\\\\?\\C:a\\b") simplify-path "\\\\?\\C:a\\b")
    (test (bytes->path #"\\\\?\\C:") simplify-path "\\\\?\\C:")
    (test (bytes->path #"\\\\?\\a\\y") simplify-path "\\\\?\\a\\y")
    (test (bytes->path #"\\\\?\\REL\\a\\y\\\\") simplify-path "\\\\?\\REL\\a\\y\\\\")
    (test (bytes->path #"\\\\?\\REL\\a\\\\\\y") simplify-path "\\\\?\\REL\\a\\\\\\y")

    (let ([dir (build-path here "tmp78")])
      (unless (directory-exists? dir)
	(make-directory dir))
      (close-output-port (open-output-file (build-path here "tmp78" "\\\\?\\REL\\aux")
					   'replace))
      (test (list (string->path "\\\\?\\REL\\\\aux")) directory-list dir)
      (delete-file (build-path here "tmp78" "\\\\?\\REL\\aux"))
      (delete-directory dir))

    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~ paths and other subtleties in Unix

(test (and (memq (system-type) '(unix macosx)) #t) absolute-path? "~")
(when (absolute-path? "~")
  (test #t relative-path? "./~")
  (test (bytes->path #"./~") bytes->path-element #"~")
  (test #"~" path-element->bytes (bytes->path #"./~"))
  (test #"~" path-element->bytes (bytes->path #"./~/"))
  (test #"a" path-element->bytes (bytes->path #"a////////////"))
  (test (bytes->path #"./~me") bytes->path-element #"~me")
  (test #"~me" path-element->bytes (bytes->path #"./~me"))
  (err/rt-test (path-element->bytes (bytes->path #"x/y")))
  (err/rt-test (path-element->bytes (bytes->path #"x/~me")))
  (err/rt-test (path-element->bytes (bytes->path #"/me")))
  (err/rt-test (path-element->bytes (bytes->path #"/")))
  (err/rt-test (bytes->path-element #"./~"))
  (err/rt-test (bytes->path-element #"x/y"))
  (err/rt-test (bytes->path-element #"/x"))
  (err/rt-test (bytes->path-element #"/"))
  (test (bytes->path #"~") simplify-path (bytes->path #"~"))
  (test (bytes->path #"~") simplify-path (bytes->path #"~/"))
  (test (bytes->path #"~") simplify-path (bytes->path #"~/."))
  (test (bytes->path #"./~") simplify-path (bytes->path #"./~"))
  (test (bytes->path #"./~/") simplify-path (bytes->path #"./~/"))
  (test (bytes->path #"~/../..") simplify-path (bytes->path #"~/../..") #f)
  (test (bytes->path #"~/..") simplify-path (bytes->path #"~/../x/..") #f)
  (test (bytes->path #"..") simplify-path (bytes->path #"../x/..") #f)
  (test (bytes->path #"x/") simplify-path (bytes->path #"x/~/..") #f)
  (test (bytes->path #".") simplify-path (bytes->path #"./") #f)
  (test (bytes->path #".") simplify-path (bytes->path #".//") #f)
  (test (bytes->path #"..") simplify-path (bytes->path #"../") #f)
  (test (bytes->path #"..") simplify-path (bytes->path #"..//") #f)
  (test (bytes->path #"..") simplify-path (bytes->path #"..//./") #f)
  (test (bytes->path #"x/") path->directory-path (bytes->path #"x"))
  (test (bytes->path #"x/") path->directory-path (bytes->path #"x/"))
  (test (bytes->path #"x/.") path->directory-path (bytes->path #"x/."))
  (test (bytes->path #"x/./") path->directory-path (bytes->path #"x/./"))
  (test (bytes->path #"x/..") path->directory-path (bytes->path #"x/.."))
  (test (bytes->path #"x/../") path->directory-path (bytes->path #"x/../"))
  (test (bytes->path #".") path->directory-path (bytes->path #"."))
  (test (bytes->path #"./") path->directory-path (bytes->path #"./"))
  (test (bytes->path #"..") path->directory-path (bytes->path #".."))
  (test (bytes->path #"../") path->directory-path (bytes->path #"../"))
  (test (bytes->path #"~") path->directory-path (bytes->path #"~"))
  (test (bytes->path #"~me") path->directory-path (bytes->path #"~me"))
  (test (bytes->path #"~/") path->directory-path (bytes->path #"~/"))
  (test (bytes->path #"~me/") path->directory-path (bytes->path #"~me/"))
  (test (bytes->path #"./~/") path->directory-path (bytes->path #"./~"))
  (test-values (list #f (bytes->path #"~me") #t) (lambda () (split-path (bytes->path #"~me"))))
  (test-values (list #f (bytes->path #"~me") #t) (lambda () (split-path (bytes->path #"~me/"))))
  (test-values (list 'relative (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"./~me"))))
  (test-values (list 'relative (bytes->path #"./~me") #t) (lambda () (split-path (bytes->path #"./~me/"))))
  (test-values (list (bytes->path #"./.") (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"././~me"))))
  (test-values (list 'relative (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #".//~me"))))
  (test-values (list (bytes->path #"y/x/") (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"y/x/~me"))))
  (test-values (list (bytes->path #"y/x/") (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"y//x//~me"))))
  (test-values (list (bytes->path #"y/x/.") (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"y/x/./~me"))))
  (test-values (list (bytes->path #"x/") (bytes->path #"./~me") #f) (lambda () (split-path (bytes->path #"x/~me"))))
  (test-values (list (bytes->path #"x/") (bytes->path #"./~me") #t) (lambda () (split-path (bytes->path #"x/~me/"))))
  (test-values (list (bytes->path #"x/.") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"x/./y"))))
  (test-values (list (bytes->path #"x/..") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"x/../y"))))
  (test-values (list (bytes->path #"~me") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"~me/y"))))
  (test-values (list (bytes->path #"~me/y/") (bytes->path #"z") #f) (lambda () (split-path (bytes->path #"~me/y/z"))))
  (test (bytes->path #"/home/mflatt/~") build-path (bytes->path #"/home/mflatt") (bytes->path #"./~"))
  (test (bytes->path #"/home/mflatt/././~") build-path (bytes->path #"/home/mflatt") (bytes->path #"././~"))
  (test (bytes->path #"./~") build-path (bytes->path #"./~"))
  (let ([dir "tmp79"])
    (unless (directory-exists? dir)
      (make-directory dir))
    (close-output-port (open-output-file "tmp79/~me" 'replace))
    (test (list (bytes->path #"./~me")) directory-list dir)
    (delete-file (build-path "tmp79" (bytes->path #"./~me")))
    (delete-directory dir))
  (void))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
