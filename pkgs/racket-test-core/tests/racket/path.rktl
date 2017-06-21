
(load-relative "loadtest.rktl")

(Section 'path)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test #t path<? (bytes->path #"a") (bytes->path #"b"))
(test #f path<? (bytes->path #"b") (bytes->path #"a"))
(test #t path<? (bytes->path #"a") (bytes->path #"b") (bytes->path #"c"))
(test #f path<? (bytes->path #"a") (bytes->path #"c") (bytes->path #"b"))
(test #t path<? (bytes->path #"a") (bytes->path #"aa"))
(test #f path<? (bytes->path #"aa") (bytes->path #"a"))

(define (test-basic-extension path-replace-extension
                              path-add-extension)
  (test (string->path "x.zo") path-replace-extension "x.rkt" ".zo")
  (test (string->path "x.zo") path-replace-extension "x.rkt" #".zo")
  (test (string->path "x.zo") path-replace-extension "x" #".zo")
  (test (string->path "x.o.zo") path-replace-extension "x.o.rkt" #".zo")
  (test (string->some-system-path "p/x.zo" 'unix)
        path-replace-extension (string->some-system-path "p/x.rkt" 'unix) ".zo")
  (test (string->some-system-path "p/x.zo" 'windows)
        path-replace-extension (string->some-system-path "p/x.rkt" 'windows) ".zo")
  (test (string->path "x_rkt.zo") path-add-extension "x.rkt" ".zo")
  (test (string->path "x_rkt.zo") path-add-extension "x.rkt" #".zo")
  (test (string->path "x.zo") path-add-extension "x" #".zo")
  (test (string->path "x.o_rkt.zo") path-add-extension "x.o.rkt" #".zo")
  (test (string->some-system-path "p/x.zo" 'unix)
        path-add-extension (string->some-system-path "p/x" 'unix) ".zo")
  (test (string->some-system-path "p/x.zo" 'windows)
        path-add-extension (string->some-system-path "p/x" 'windows) ".zo"))

(test-basic-extension path-replace-extension
                      path-add-extension)
(test-basic-extension path-replace-suffix
                      path-add-suffix)

(test (string->path ".zo.y") path-replace-extension ".zo" ".y")
(test (string->path ".zo.y") path-replace-extension ".zo" #".y")
(test (string->path ".zo") path-replace-extension ".zo" "")
(test (string->path ".zo") path-replace-extension ".zo" #"")
(test (string->path ".zo.y") path-add-extension ".zo" ".y")
(test (string->path ".zo.y") path-add-extension ".zo" #".y")
(test (string->path ".tar_gz.y") path-add-extension ".tar.gz" ".y")
(test (string->path ".tar_gz.y") path-add-extension ".tar.gz" #".y")
(test (string->some-system-path "p/x.tar.gz" 'unix)
      path-add-extension (string->some-system-path "p/x.tar" 'unix) ".gz" #".")
(test (string->some-system-path "p/x.tar.gz" 'windows)
      path-add-extension (string->some-system-path "p/x.tar" 'windows) ".gz" ".")
(err/rt-test (path-add-extension "x" ".zip" #f))

(test (string->path ".y") path-replace-suffix ".zo" ".y")
(test (string->path ".y") path-replace-suffix ".zo" #".y")
(test (string->path "_zo.y") path-add-suffix ".zo" ".y")
(test (string->path "_zo.y") path-add-suffix ".zo" #".y")
(err/rt-test (path-replace-suffix ".zo" ""))
(err/rt-test (path-replace-suffix ".zo" #""))
(test (string->path ".tar_gz.y") path-add-suffix ".tar.gz" ".y")
(test (string->path ".tar_gz.y") path-add-suffix ".tar.gz" #".y")

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
(define rename-file-or-directory/exists/tf (lambda (x y) ((make-/tf rename-file-or-directory exn:fail:filesystem:exists?) x y)))
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

(define original-dir (current-directory))
(define work-dir (make-temporary-file "path~a" 'directory))
(current-directory work-dir)

(call-with-output-file "tmp6" void #:exists 'replace)
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
(let ([p (open-output-file "tmp5" #:exists 'replace)])
  (display "123456789" p)
  (close-output-port p))
(close-output-port (open-output-file (build-path "down" "tmp8") #:exists 'replace))
(close-output-port (open-output-file (build-path deepdir "tmp7") #:exists 'replace))
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
(err/rt-test (copy-file "tmp5" "tmp5y") exn:fail:filesystem:exists?)
(err/rt-test (copy-file "no-such-tmp5" "tmp5y") (lambda (x) (not (exn:fail:filesystem:exists? x))))
(delete-file "tmp5y")

(test #t rename-file-or-directory/tf "tmp5" "tmp5x")
(test #f rename-file-or-directory/tf "tmp5" "tmp5x")
(close-output-port (open-output-file "tmp5"))
(test #t file-exists? "tmp5")
(test #t file-exists? "tmp5x")
(test #f rename-file-or-directory/exists/tf "tmp5" "tmp5x")
(test #f rename-file-or-directory/exists/tf "tmp5" "down")
(delete-file "tmp5")
(test #f file-exists? "tmp5")
(test #t rename-file-or-directory/tf (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #f rename-file-or-directory/tf (build-path "down" "tmp8") (build-path "down" "tmp8x"))
(test #t rename-file-or-directory/tf (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))
(test #f rename-file-or-directory/tf (build-path deepdir "tmp7") (build-path deepdir "tmp7x"))

(test #t make-directory/tf "downx")
(test #f rename-file-or-directory/exists/tf "down" "downx")
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
(test #t 'directory-list 
      (let ([l (directory-list "down" #:build? #t)]
            [l2 (list (build-path "down" "deep")
                      (build-path "down" "tmp8x"))])
        (or (equal? l l2)
            (equal? l (reverse l2)))))
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

(current-directory original-dir)
(delete-directory work-dir)

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
  (test (normal-case-path (cleanse-path expect))
	(or (object-name f) 'unknown)
	(normal-case-path (cleanse-path (apply f args)))))

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
(test (path->directory-path (build-path 'same)) simplify-path (build-path 'same) #f)
(test (path->directory-path (build-path 'same)) simplify-path (build-path 'same 'same 'same) #f)
(test (path->directory-path (build-path 'same)) simplify-path (build-path 'same "a" 'same 'up 'same) #f)
(arity-test simplify-path 1 2)

(arity-test cleanse-path 1 1)
(arity-test expand-user-path 1 1)
(arity-test resolve-path 1 1)

(when (eq? 'unix (system-path-convention-type))
  (test #t complete-path? (expand-user-path "~/something"))
  (test #t complete-path? (expand-user-path (string->path "~/something"))))

(map
 (lambda (f)
   (err/rt-test (f (string #\a #\nul #\b)) exn:fail:contract?))
 (list build-path split-path file-exists? directory-exists?
       delete-file directory-list make-directory delete-directory
       file-or-directory-modify-seconds file-or-directory-permissions 
       cleanse-path resolve-path simplify-path path->complete-path
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
		 (normal-case-path (simplify-path (cleanse-path (current-directory))))))
	       "")]
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
      (test (string->path "\\\\?\\\\\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\x\\y\\"])
      (test (string->path "\\\\?\\") current-drive)
      (test (string->path "\\\\?\\\\\\a") path->complete-path "\\a")
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
      (test (string->path "\\\\?\\\\\\a") path->complete-path "\\a")
      )
    (parameterize ([current-directory "\\\\?\\REL\\a\\\\"])
      (test (string->path "\\\\?\\REL\\a\\\\") current-drive)
      (test (string->path "\\\\?\\REL\\a\\\\\\a") path->complete-path "\\a")
      )

    (let ([dir (build-path here "tmp78")])
      (unless (directory-exists? dir)
	(make-directory dir))
      (close-output-port (open-output-file (build-path here "tmp78" "\\\\?\\REL\\aux")
					   #:exists 'replace))
      (test (list (string->path "\\\\?\\REL\\\\aux")) directory-list dir)
      (delete-file (build-path here "tmp78" "\\\\?\\REL\\aux"))
      (delete-directory dir))

    ))

(let* ([bytes->path (lambda (p)
                      (bytes->path p 'windows))]
       [string->path (if (eq? (system-path-convention-type) 'windows)
                         string->path
                         (lambda (p)
                           (bytes->path (string->bytes/latin-1 p))))]
       [coerce (if (eq? (system-path-convention-type) 'windows)
                   values
                   string->path)]
       [get-base (lambda (s)
		    (let-values ([(base name dir?) (split-path s)]) (list base name)))])
  (test (bytes->path #"c:\\a") path->complete-path (coerce "\\a") (coerce "c:"))
  (test (bytes->path #"\\\\foo\\bar\\a") path->complete-path (coerce "\\a") (coerce "\\\\foo\\bar\\a"))
  
  ;; Build path collapses .. and . in added parts and cleans up slashes and trailers:
  (test (string->path "\\\\?\\c:\\a\\b") build-path (coerce "\\\\?\\c:\\a") 'same (coerce "b"))
  (test (string->path "\\\\?\\c:\\a\\b") build-path (coerce "\\\\?\\c:\\a\\") 'same (coerce "b"))
  (test (string->path "\\\\?\\c:\\a\\.\\b") build-path (coerce "\\\\?\\c:\\a\\.") 'same (coerce "b"))
  (test (string->path "\\\\?\\c:\\a\\.\\b") build-path (coerce "\\\\?\\c:\\a\\.\\") 'same (coerce "b"))
  (test (string->path "\\\\?\\c:\\b") build-path (coerce "\\\\?\\c:\\a\\") 'up (coerce "b"))
  (test (string->path "\\\\?\\c:\\a\\b") build-path (coerce "\\\\?\\c:\\a\\.\\") 'up (coerce "b"))
  (test (string->path "\\\\?\\c:\\b") build-path (coerce "\\\\?\\c:\\a\\.\\") 'up 'up 'up (coerce "b"))
  (test (string->path "\\\\?\\c:\\a\\b\\c\\d\\f\\") build-path (coerce "\\\\?\\c:\\a") (coerce "b//c") (coerce "d//\\f///"))
  (test (string->path "\\\\?\\c:\\a\\foo") build-path (coerce "\\\\?\\c:\\a") (coerce "foo...  "))

  (test (string->path "\\\\?\\REL\\\\a") build-path (coerce ".") (coerce "\\\\?\\REL\\a"))
  (test (string->path "\\\\?\\REL\\\\a") build-path 'same (coerce "\\\\?\\REL\\a"))
  (test (string->path "\\\\?\\REL\\\\a\\") build-path (coerce "\\\\?\\REL\\a") 'same)
  (test (string->path ".") build-path (coerce "\\\\?\\REL\\a") 'up)
  (test (string->path "\\\\?\\REL\\\\apple") build-path (coerce "\\\\?\\REL\\a") 'up (coerce "apple"))
  (test (string->path ".") build-path (coerce "\\\\?\\REL\\a") 'up 'same)

  (test (string->path "\\\\?\\REL\\..\\\\a") build-path 'up (coerce "\\\\?\\REL\\a"))
  (test (string->path "\\\\?\\REL\\..\\\\..") build-path 'up (coerce "\\\\?\\REL\\\\.."))
  (test (string->path "\\\\?\\REL\\..\\..\\") build-path 'up (coerce "\\\\?\\REL\\.."))
  (test (string->path "\\\\?\\REL\\..\\..\\") build-path (coerce "\\\\?\\REL\\..") 'up)
  (test (string->path "\\\\?\\REL\\..\\..\\..\\") build-path 'up (coerce "\\\\?\\REL\\..") 'up)
  (test (string->path "\\\\?\\REL\\..\\..\\\\..") build-path 'up (coerce "\\\\?\\REL\\..") (coerce "\\\\?\\REL\\\\.."))
  (test (string->path "\\\\?\\REL\\..\\..\\") build-path 'up (coerce "\\\\?\\REL\\..") (coerce "\\\\?\\REL\\\\..") 'up)
  (test (string->path "\\\\?\\REL\\\\..\\") build-path (coerce "\\\\?\\REL\\\\..") (coerce "\\\\?\\REL\\\\..") 'up)
  (test (string->path "\\\\?\\REL\\\\x\\") build-path (coerce "x/y") (coerce "\\\\?\\REL\\.."))
  (test (string->path "\\\\?\\c:\\x\\") build-path (coerce "c:x/y") (coerce "\\\\?\\REL\\.."))

  (test-values (list (string->path "\\\\?\\REL\\..")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\REL\\..\\a"))))
  (test-values (list (string->path "\\\\?\\REL\\..")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\REL\\..\\\\a"))))
  (test-values (list (string->path "\\\\?\\REL\\b\\")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\REL\\b\\a"))))

  (test (string->path "\\\\?\\RED\\\\a\\") build-path (coerce "\\\\?\\RED\\a") 'same)
  (test (string->path "\\") build-path (coerce "\\\\?\\RED\\a") 'up)
  (test (string->path "\\\\?\\RED\\\\apple") build-path (coerce "\\\\?\\RED\\a") 'up (coerce "apple"))
  (test (string->path "\\") build-path (coerce "\\\\?\\RED\\a") 'up 'same)

  (test (string->path "\\") build-path (coerce "\\\\?\\RED\\..") 'up)
  (test (string->path "\\\\?\\RED\\\\..\\") build-path (coerce "\\\\?\\RED\\\\..") (coerce "\\\\?\\RED\\\\..") 'up)
  (test (string->path "\\\\?\\RED\\\\x\\y\\..") build-path (coerce "/x/y") (coerce "\\\\?\\RED\\.."))
  (test (string->path "\\\\?\\c:\\x\\y\\..") build-path (coerce "c:x/y") (coerce "\\\\?\\RED\\.."))

  (test-values (list (string->path "\\\\?\\RED\\..\\")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\RED\\..\\a"))))
  (test-values (list (string->path "\\\\?\\RED\\..\\")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\RED\\..\\\\a"))))
  (test-values (list (string->path "\\\\?\\RED\\b\\")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\RED\\b\\a"))))
  (test-values (list (string->path "\\")
                     (string->path "\\\\?\\REL\\\\..")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\RED\\.."))))
  (test-values (list (string->path "\\")
                     (string->path "\\\\?\\REL\\\\a")
                     #f)
               (lambda () (split-path (coerce "\\\\?\\RED\\a"))))

  (test (string->path "\\\\?\\UNC\\f\\g\\") build-path (coerce "//f/g/h") (coerce "\\\\?\\REL\\.."))
  (test (string->path "\\\\?\\UNC\\f\\g\\h\\") build-path (coerce "//f/g/h/i") (coerce "\\\\?\\REL\\.."))
  (test (string->path "\\\\?\\UNC\\f\\g\\h\\..") build-path (coerce "//f/g/h") (coerce "\\\\?\\REL\\\\.."))
  (test (string->path "\\\\?\\UNC\\f\\g\\h\\..") build-path (coerce "//f/g//h") (coerce "\\\\?\\REL\\\\.."))
  (test (string->path "\\\\?\\UNC\\f\\g\\i\\x") build-path (coerce "//f/g/h/../i") (coerce "\\\\?\\REL\\x"))
  (test (string->path "\\\\?\\UNC\\f\\g\\h\\i\\x") build-path (coerce "//f/g/../h/i") (coerce "\\\\?\\REL\\x"))

  (let ([go (lambda (drive build-path simple)
              (test (string->path (string-append "\\\\?\\" drive "f\\g\\")) 
                    build-path (coerce "/f/g/h") (coerce "\\\\?\\REL\\.."))
              (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x")) 
                    build-path (coerce "/f/g/h") (coerce "\\\\?\\REL\\x"))
              (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x"))
                    build-path (coerce "//f//g/h") (coerce "\\\\?\\REL\\x"))
              (test (string->path (string-append "\\\\?\\" drive "f\\g\\h\\x"))
                    build-path (coerce "/f//g////h") (coerce "\\\\?\\REL\\x"))
              (test (string->path simple) 
                    build-path (coerce "/f//g////h") (coerce "\\\\?\\REL\\..\\..\\..\\.."))
              (test (string->path (string-append "\\\\?\\" drive "?\\g\\h\\x")) 
                    build-path (coerce "//?/g/h") (coerce "\\\\?\\REL\\x")))])
    (go "RED\\\\" build-path "\\")
    (go "C:\\" (lambda args (path->complete-path (apply build-path args) (coerce "C:"))) "C:\\"))
    
  ;; Allow \\?\ as a drive to add an absolute path:
  (test (string->path "\\\\?\\c:\\b") build-path (coerce "\\\\?\\c:\\") (coerce "\\b"))
  (test (string->path "\\\\?\\c:\\b") build-path (coerce "\\\\?\\c:\\\\") (coerce "\\b"))
  (test (string->path "\\\\?\\c:\\b\\") build-path (coerce "\\\\?\\c:\\\\") (coerce "\\b\\"))
  (test (string->path "\\\\?\\UNC\\goo\\bar\\b") build-path (coerce "\\\\?\\UNC\\goo\\bar") (coerce "\\b"))
  (test (string->path "\\\\?\\\\\\b") build-path (coerce "\\\\?\\") (coerce "\\b"))
  (test (string->path "\\\\?\\\\\\b\\") build-path (coerce "\\\\?\\") (coerce "\\b\\"))
  (err/rt-test (build-path "\\\\?\\c:" (coerce "\\b")) exn:fail:contract?)
  
  ;; Don't allow path addition on bad \\?\ to change the root:
  (test (string->path "\\\\?\\\\\\c") build-path (coerce "\\\\?\\") (coerce "c"))
  (test (string->path "\\\\?\\\\\\c:") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\c:"))
  (test (string->path "\\\\?\\\\\\UNC") build-path (coerce "\\\\?\\") (coerce "UNC"))
  (test (string->path "\\\\?\\\\\\UNC\\s\\y") build-path (coerce "\\\\?\\UNC") (coerce "s/y"))
  (test (string->path "\\\\?\\\\\\UNC\\s\\y") build-path (coerce "\\\\?\\UNC\\") (coerce "s/y"))
  (test (string->path "\\\\?\\\\\\UNC\\s\\y") build-path (coerce "\\\\?\\UNC\\s") (coerce "y"))
  (test (string->path "\\\\?\\\\\\REL\\s\\y") build-path (coerce "\\\\?\\REL") (coerce "s/y"))
  (test (string->path "\\\\?\\\\\\REL\\s\\y") build-path (coerce "\\\\?\\REL\\") (coerce "s/y"))
  (test (string->path "\\\\?\\REL\\\\\\s\\y") build-path (coerce "\\\\?\\REL\\\\") (coerce "s/y"))
  (test (string->path "\\\\?\\REL\\x\\\\\\z") build-path (coerce "\\\\?\\REL\\x\\\\") (coerce "z"))
  (test (string->path "/apple\\x") build-path (coerce "//apple") (coerce "x"))
  (test (string->path "\\\\?") build-path (coerce "\\\\") (coerce "?"))
  (test (string->path "\\?\\") build-path (coerce "\\\\") (coerce "?\\"))
  (test (string->path "\\?\\a") build-path (coerce "\\\\") (coerce "?") (coerce "a"))
  (test (string->path "\\?\\a") build-path (coerce "\\\\?") (coerce "a"))
  (test (string->path "\\?\\a\\") build-path (coerce "\\\\?") (coerce "a\\"))
  (test (string->path "\\\\?\\\\\\c:") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\c:"))
  (test (string->path "\\\\?\\\\\\c:\\") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\c:\\"))
  (test (string->path "\\\\?\\\\\\c:\\a") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\c:\\a"))
  (test (string->path "\\\\?\\\\\\REL\\b") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\REL\\b"))
  (test (string->path "\\\\?\\\\\\host\\vol\\a\\") build-path (coerce "\\\\?\\") (coerce "\\\\?\\REL\\\\host\\vol\\a\\"))

  ;; UNC paths can't have "?" for machine or "/" in machine part:
  (test (list (string->path "/?/") (string->path "x")) get-base (coerce "//?/x"))
  (test (list (string->path "\\\\?\\UNC\\a/b\\") (string->path "x")) get-base (coerce "\\\\?\\UNC\\a/b\\x"))

  ;; Split path must treat \\?\ part as a root:
  (test (list (string->path "\\\\?\\c:\\") (string->path "a")) get-base (coerce "\\\\?\\c:\\a"))
  (test (list (string->path "\\\\?\\") (string->path "\\\\?\\REL\\\\c:")) get-base (coerce "\\\\?\\c:"))
  (test (list #f (string->path "\\\\?\\c:\\")) get-base (coerce "\\\\?\\c:\\"))
  (test (list #f (string->path "\\\\?\\c:\\\\")) get-base (coerce "\\\\?\\c:\\\\"))
  (test (list (string->path "\\\\?\\c:\\") (string->path "a")) get-base (coerce "\\\\?\\c:\\\\a"))
  (test (list #f (string->path "\\\\?\\UNC\\mach\\vol")) get-base (coerce "\\\\?\\UNC\\mach\\vol"))
  (test (list #f (string->path "\\\\?\\UNC\\mach\\\\vol")) get-base (coerce "\\\\?\\UNC\\mach\\\\vol"))
  (test (list #f (string->path "\\\\?\\UNC\\\\mach\\vol")) get-base (coerce "\\\\?\\UNC\\\\mach\\vol"))
  (test (list (string->path "\\\\?\\") (string->path "c")) get-base (coerce "\\\\?\\c"))
  (test (list (string->path "\\\\?\\UNC\\") (string->path "x")) get-base (coerce "\\\\?\\UNC\\x"))
  (test (list (string->path "\\\\?\\") (string->path "UNC")) get-base (coerce "\\\\?\\UNC\\"))
  (test (list #f (string->path "\\\\?\\UNC\\\\")) get-base (coerce "\\\\?\\UNC\\\\"))
  (test (list #f (string->path "\\\\?\\xyz\\\\")) get-base (coerce "\\\\?\\xyz\\\\"))
  (test (list (string->path "\\\\?\\c:\\a\\\\") (string->path "b")) get-base (coerce "\\\\?\\c:\\a\\\\\\b\\"))
  (test (list #f (string->path "\\\\?\\c:\\a\\\\\\")) get-base (coerce "\\\\?\\c:\\a\\\\\\"))
  (test (list (string->path "\\\\?\\UNC\\") (string->path "\\\\?\\REL\\\\x/y")) get-base (coerce "\\\\?\\UNC\\x/y"))
  (test (list #f (string->path "\\\\?\\UNC\\x\\y")) get-base (coerce "\\\\?\\UNC\\x\\y"))
  (test (list (string->path "\\\\?\\REL\\\\x\\y ") (string->path "z")) get-base (coerce "x/y /z"))
  (test (list (string->path "\\\\?\\REL\\\\y ") (string->path "z")) get-base (coerce "x/../y /z"))
  (test (list (string->path "\\\\?\\REL\\..\\\\y ") (string->path "z")) get-base (coerce "../y /z"))
  (test (list (string->path "\\\\?\\c:\\y ") (string->path "z")) get-base (coerce "c:/y /z"))
  (test (list (string->path "\\\\?\\c:\\y ") (string->path "z")) get-base (coerce "c:/../y /z"))
  (test (list (string->path "../aux/") (string->path "z")) get-base (coerce "../aux/z"))
  (test (list (string->path "../aux.m/") (string->path "z")) get-base (coerce "../aux.m/z"))
  (test (list (string->path "../") (string->path "\\\\?\\REL\\\\aux.m")) get-base (coerce "../aux.m/"))
  (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux.m")) get-base (coerce "c:/aux.m/"))
  (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux.m.p")) get-base (coerce "c:/aux.m.p/"))
  (test (list (string->path "c:/") (string->path "\\\\?\\REL\\\\aux:m")) get-base (coerce "c:/aux:m/"))
  (test (list (string->path "../") (string->path "aux.m")) get-base (coerce "../aux.m"))

  ;; simplify-path leaves literal . and .. alone:
  (test (string->path "\\\\?\\c:\\b\\.\\..\\a") simplify-path (coerce "\\\\?\\c:\\b\\.\\..\\a") #f)
  (test (string->path "\\\\?\\c:\\B\\.\\..\\a") normal-case-path (coerce "\\\\?\\c:\\B\\.\\..\\a"))
  (test (string->path "\\\\?\\UNC\\foo\\A") normal-case-path (coerce "\\\\?\\UNC\\foo\\A"))
  (test (string->path "\\\\?\\RED\\..\\..") normal-case-path (coerce "\\\\?\\RED\\..\\.."))

  ;; cleanse-path removes redundant backslashes
  (test (string->path "\\\\?\\\\UNC\\x\\y") cleanse-path (coerce "\\\\?\\\\UNC\\x\\y"))
  (test (string->path "\\\\?\\c:\\") cleanse-path (coerce "\\\\?\\c:\\\\"))

  ;; cleanse-path removes redundant backslashes, and
  ;; simplify-path uses cleanse-path under Windows:
  (let ([go
         (lambda (cleanse-path)
           (test (string->path "c:\\") cleanse-path (coerce "c:"))
           (test (string->path "\\\\?\\c:\\a\\.") cleanse-path (coerce "\\\\?\\c:\\\\a\\\\."))
           (test (string->path "\\\\?\\c:\\a\\\\") cleanse-path (coerce "\\\\?\\c:\\a\\\\"))
           (test (string->path "\\\\?\\c:\\a\\.") cleanse-path (coerce "\\\\?\\c:\\a\\\\."))
           (test (string->path "\\\\?\\UNC\\a\\b\\.") cleanse-path (coerce "\\\\?\\UNC\\\\a\\b\\."))
           (test (string->path "\\\\?\\UNC\\a\\b\\.") cleanse-path (coerce "\\\\?\\UNC\\\\a\\b\\\\."))
           (test (string->path "\\\\?\\RED\\\\..") cleanse-path (coerce "\\\\?\\RED\\.."))
           (test (string->path "\\\\?\\") cleanse-path (coerce "\\\\?\\\\")))])
    (go cleanse-path)
    (test (string->path "\\\\?\\c:") cleanse-path (coerce "\\\\?\\c:"))
    (when (eq? 'windows (system-type))
      (go simplify-path))
    (go (lambda (p) (simplify-path p #f)))
    (test (string->path "a\\b") simplify-path (coerce "a/b") #f)
    (test (string->path "a\\b\\") simplify-path (coerce "a/b/") #f)
    (test (string->path "C:\\") simplify-path (coerce "C://") #f)
    (test (string->path "C:\\a\\") simplify-path (coerce "C://a//") #f)
    (test (string->path "\\\\?\\\\\\c:") simplify-path (coerce "\\\\?\\c:") #f)
    (test (string->path "\\\\?\\\\\\c:") simplify-path (coerce "\\\\?\\\\c:") #f)
    (test (string->path "\\\\?\\\\\\c:") simplify-path (coerce "\\\\?\\\\\\c:") #f))

  (test (bytes->path #"\\\\f\\g\\") simplify-path (coerce "\\\\f\\g") #f)
  (test (bytes->path #"\\\\f\\g\\") simplify-path (coerce "//f/g") #f)

  (test (bytes->path #"\\\\?\\\\\\c:\\") simplify-path (coerce "\\\\?\\\\\\c:\\") #f)
  (test (bytes->path #"\\\\?\\\\\\c:") simplify-path (coerce "\\\\?\\\\\\c:") #f)

  (when (eq? 'windows (system-type))
    (test (bytes->path #"\\\\?\\c:\\a\\b//c\\d") cleanse-path (coerce "\\\\?\\c:\\a\\b//c\\d")))

  (test (bytes->path #"\\\\?\\UNC\\a\\b/c\\") simplify-path (coerce "\\\\?\\\\UNC\\a\\b/c") #f)
  (test (bytes->path #"\\\\a\\b\\") simplify-path (coerce "\\\\?\\\\UNC\\a\\b") #f)
  (test (bytes->path #"\\\\?\\UNC\\a\\b/c\\") simplify-path (coerce "\\\\?\\UnC\\a\\b/c") #f)
  (test (bytes->path #"\\\\a\\b\\") simplify-path (coerce "\\\\?\\UnC\\a\\b") #f)
  (test (bytes->path #"\\\\?\\c:\\a/b") simplify-path (coerce "\\\\?\\c:\\a/b") #f)

  (test (bytes->path #"..\\") simplify-path (coerce "\\\\?\\REL\\..") #f)
  (test (bytes->path #"..\\") simplify-path (coerce "\\\\?\\REL\\..\\") #f)
  (when (eq? 'windows (system-type))
    (test (bytes->path #"\\\\foo\\bar\\") cleanse-path (coerce "\\\\foo\\bar\\")))
  (test (bytes->path #"\\\\foo\\bar\\") simplify-path (coerce "\\\\foo\\bar\\") #f)
  (test (bytes->path #"\\\\foo\\bar\\") simplify-path (coerce "\\\\?\\UNC\\foo\\bar") #f)
  (test (bytes->path #"\\\\foo\\bar\\") simplify-path (coerce "\\\\?\\UNC\\foo\\bar\\") #f)
  (test (bytes->path #"\\\\?\\UNC\\foo\\bar\\..") simplify-path (coerce "\\\\?\\UNC\\foo\\bar\\..") #f)
  (test (bytes->path #"\\\\?\\UNC\\foo\\bar\\..\\") simplify-path (coerce "\\\\?\\UNC\\foo\\bar\\..\\") #f)
  (test (bytes->path #"a") simplify-path (coerce "\\\\?\\REL\\a") #f)
  (test (bytes->path #"a") simplify-path (coerce "\\\\?\\REL\\\\a") #f)
  (test (bytes->path #"a\\") simplify-path (coerce "\\\\?\\REL\\\\a\\") #f)
  (test (bytes->path #"\\\\?\\REL\\\\a/") simplify-path (coerce "\\\\?\\REL\\\\a/") #f)
  (test (bytes->path #"..\\") simplify-path (coerce "\\\\?\\REL\\..") #F)
  (test (bytes->path #"\\\\?\\REL\\\\..") simplify-path (coerce "\\\\?\\REL\\\\..") #F)
  (test (bytes->path #"\\\\?\\REL\\\\..\\") simplify-path (coerce "\\\\?\\REL\\\\..\\") #F)
  (test (bytes->path #"a \\b") simplify-path (coerce "\\\\?\\REL\\\\a \\b") #f)
  (test (bytes->path #"\\\\?\\REL\\\\aux.bad\\b") simplify-path (coerce "\\\\?\\REL\\aux.bad\\b") #f)
  (test (bytes->path #"\\\\?\\REL\\\\a\\b  ") simplify-path (coerce "\\\\?\\REL\\a\\b  ") #f)
  (test (bytes->path #"\\\\?\\REL\\\\.\\b") simplify-path (coerce "\\\\?\\REL\\.\\b") #f)
  (test (bytes->path #"\\\\?\\REL\\\\.") simplify-path (coerce "\\\\?\\REL\\.") #F)
  (test (bytes->path #"\\\\?\\REL\\\\:\\b") simplify-path (coerce "\\\\?\\REL\\:\\b") #f)
  (test (bytes->path #"\\\\?\\REL\\\\:") simplify-path (coerce "\\\\?\\REL\\:") #F)
  (test (bytes->path #"\\\\?\\\\\\REL") simplify-path (coerce "\\\\?\\REL") #F)
  (test (bytes->path #"\\\\?\\\\\\REL") simplify-path (coerce "\\\\?\\\\REL") #F)
  (test (bytes->path #"\\\\?\\\\\\REL") simplify-path (coerce "\\\\?\\\\\\REL") #F)
  (test (bytes->path #"C:\\a\\b") simplify-path (coerce "\\\\?\\C:\\a\\b") #f)
  (test (bytes->path #"C:\\a") simplify-path (coerce "\\\\?\\C:\\a") #f)
  (test (bytes->path #"\\\\?\\C:\\a ") simplify-path (coerce "\\\\?\\C:\\a ") #f)
  (test (bytes->path #"\\\\?\\\\\\C:a\\b") simplify-path (coerce "\\\\?\\C:a\\b") #f)
  (test (bytes->path #"\\\\?\\\\\\C:a\\b") simplify-path (coerce "\\\\?\\\\C:a\\b") #f)
  (test (bytes->path #"\\\\?\\\\\\C:") simplify-path (coerce "\\\\?\\C:") #f)
  (test (bytes->path #"\\\\?\\\\\\C:") simplify-path (coerce "\\\\?\\\\C:") #f)
  (test (bytes->path #"\\\\?\\\\\\a\\y") simplify-path (coerce "\\\\?\\a\\y") #f)
  (test (bytes->path #"\\\\?\\\\\\a\\y") simplify-path (coerce "\\\\?\\\\a\\y") #f)
  (test (bytes->path #"\\\\?\\\\\\a\\y") simplify-path (coerce "\\\\?\\\\\\a\\y") #f)
  (test (bytes->path #"\\\\?\\REL\\a\\y\\\\") simplify-path (coerce "\\\\?\\REL\\a\\y\\\\") #f)
  (test (bytes->path #"\\\\?\\REL\\a\\\\\\y") simplify-path (coerce "\\\\?\\REL\\a\\\\\\y") #f)
  (test (bytes->path #"\\a") simplify-path (coerce "\\\\?\\RED\\a") #f)
  (test (bytes->path #"\\a") simplify-path (coerce "\\\\?\\RED\\\\a") #f)
  (test (bytes->path #"\\a\\b") simplify-path (coerce "\\\\?\\RED\\\\a\\\\b") #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~ paths (no longer special) and other subtleties in Unix

(test #f absolute-path? "~")
(test #f absolute-path? (bytes->path #"~" 'unix))
(test #f absolute-path? (bytes->path #"~" 'windows))

(define (test-~-paths kind fs-ok?)
  (let* ([use-fs? (and fs-ok?
                       (eq? kind (system-path-convention-type)))]
         [bytes->path (if use-fs?
                          bytes->path
                          (lambda (s) (bytes->path s kind)))]
         
         [bytes->path-element (if use-fs?
                                  bytes->path-element
                                  (lambda (s) (bytes->path-element s kind)))])
    (test #t relative-path? (bytes->path #"./~"))
    (test (bytes->path #"~") bytes->path-element #"~")
    (test #"~" path-element->bytes (bytes->path #"~"))
    (test #"a" path-element->bytes (bytes->path #"a////////////"))
    (test (bytes->path #"~me") bytes->path-element #"~me")
    (test #"~me" path-element->bytes (bytes->path #"~me"))
    (err/rt-test (path-element->bytes (bytes->path #"x/y")))
    (err/rt-test (path-element->bytes (bytes->path #"./~")))
    (err/rt-test (path-element->bytes (bytes->path #"./~me")))
    (err/rt-test (path-element->bytes (bytes->path #"x/~me")))
    (err/rt-test (path-element->bytes (bytes->path #"/me")))
    (err/rt-test (path-element->bytes (bytes->path #"/")))
    (err/rt-test (bytes->path-element #"./~"))
    (err/rt-test (bytes->path-element #"x/y"))
    (err/rt-test (bytes->path-element #"/x"))
    (err/rt-test (bytes->path-element #"/"))
    (unless use-fs?
      (test (bytes->path #"~") simplify-path (bytes->path #"~") use-fs?)
      (test (bytes->path #"~/") simplify-path (bytes->path #"~/") use-fs?)
      (test (bytes->path #"~/") simplify-path (bytes->path #"~/.") use-fs?)
      (test (bytes->path #"~") simplify-path (bytes->path #"./~") use-fs?)
      (test (bytes->path #"~/") simplify-path (bytes->path #"./~/") use-fs?))
    (test (bytes->path #"../") simplify-path (bytes->path #"~/../..") #f)
    (test (bytes->path #"./") simplify-path (bytes->path #"~/../x/..") #f)
    (test (bytes->path #"../") simplify-path (bytes->path #"../x/..") #f)
    (test (bytes->path #"x/") simplify-path (bytes->path #"x/~/..") #f)
    (test (bytes->path #"./") simplify-path (bytes->path #"./") #f)
    (test (bytes->path #"./") simplify-path (bytes->path #".//") #f)
    (test (bytes->path #"../") simplify-path (bytes->path #"../") #f)
    (test (bytes->path #"../") simplify-path (bytes->path #"..//") #f)
    (test (bytes->path #"../") simplify-path (bytes->path #"..//./") #f)
    (test (bytes->path #"x/") path->directory-path (bytes->path #"x"))
    (test (bytes->path #"x/") path->directory-path (bytes->path #"x/"))
    (test (bytes->path #"x/./") path->directory-path (bytes->path #"x/."))
    (test (bytes->path #"x/./") path->directory-path (bytes->path #"x/./"))
    (test (bytes->path #"x/../") path->directory-path (bytes->path #"x/.."))
    (test (bytes->path #"x/../") path->directory-path (bytes->path #"x/../"))
    (test (bytes->path #"./") path->directory-path (bytes->path #"."))
    (test (bytes->path #"./") path->directory-path (bytes->path #"./"))
    (test (bytes->path #"../") path->directory-path (bytes->path #".."))
    (test (bytes->path #"../") path->directory-path (bytes->path #"../"))
    (test (bytes->path #"~/") path->directory-path (bytes->path #"~"))
    (test (bytes->path #"~me/") path->directory-path (bytes->path #"~me"))
    (test (bytes->path #"~/") path->directory-path (bytes->path #"~/"))
    (test (bytes->path #"~me/") path->directory-path (bytes->path #"~me/"))
    (test (bytes->path #"./~/") path->directory-path (bytes->path #"./~"))
    (test-values (list 'relative (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"~me"))))
    (test-values (list 'relative (bytes->path #"~me") #t) (lambda () (split-path (bytes->path #"~me/"))))
    (test-values (list (bytes->path #"./") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"./~me"))))
    (test-values (list (bytes->path #"./") (bytes->path #"~me") #t) (lambda () (split-path (bytes->path #"./~me/"))))
    (test-values (list (bytes->path #"././") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"././~me"))))
    (test-values (list (bytes->path #"./") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #".//~me"))))
    (test-values (list (bytes->path #"y/x/") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"y/x/~me"))))
    (test-values (list (bytes->path #"y/x/") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"y//x//~me"))))
    (test-values (list (bytes->path #"y/x/./") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"y/x/./~me"))))
    (test-values (list (bytes->path #"x/") (bytes->path #"~me") #f) (lambda () (split-path (bytes->path #"x/~me"))))
    (test-values (list (bytes->path #"x/") (bytes->path #"~me") #t) (lambda () (split-path (bytes->path #"x/~me/"))))
    (test-values (list (bytes->path #"x/./") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"x/./y"))))
    (test-values (list (bytes->path #"x/../") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"x/../y"))))
    (test-values (list (bytes->path #"~me/") (bytes->path #"y") #f) (lambda () (split-path (bytes->path #"~me/y"))))
    (test-values (list (bytes->path #"~me/y/") (bytes->path #"z") #f) (lambda () (split-path (bytes->path #"~me/y/z"))))
    (test (bytes->path #"/home/mflatt/./~") build-path (bytes->path #"/home/mflatt") (bytes->path #"./~"))
    (test (bytes->path #"/home/mflatt/././~") build-path (bytes->path #"/home/mflatt") (bytes->path #"././~"))
    (test (bytes->path #"./~") build-path (bytes->path #"./~"))
    (when use-fs?
      (let ([dir (make-temporary-file "tmp79~a" 'directory)])
        (unless (directory-exists? dir)
          (make-directory dir))
        (close-output-port (open-output-file (build-path dir "~me") #:exists 'replace))
        (test (list (bytes->path #"~me")) directory-list dir)
        (delete-file (build-path dir (bytes->path #"~me")))
        (delete-directory dir)))
    (void)))

(test-~-paths 'unix #t)
(when (eq? 'unix (system-path-convention-type))
  (test-~-paths 'unix #f))

;; Assuming a reasonable locale...
(test "Apple" path-element->string (string->path-element "Apple"))
(test "Apple" path-element->string (bytes->path-element #"Apple"))

(err/rt-test (path-element->bytes (string->path ".")))
(err/rt-test (path-element->bytes (string->path "..")))
(err/rt-test (bytes->path-element #"." 'unix))
(err/rt-test (bytes->path-element #".." 'unix))
(err/rt-test (bytes->path-element "a/b" 'unix))
(err/rt-test (bytes->path-element "a\\b" 'windows))

(err/rt-test (bytes->path-element #""))
(err/rt-test (string->path-element ""))

(test #"\\\\?\\REL\\\\a/b" path->bytes (bytes->path-element #"a/b" 'windows))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([reroot-path/u
       (lambda (a b)
         (bytes->string/utf-8
          (path->bytes
           (reroot-path (bytes->path (string->bytes/utf-8 a) 'unix)
                        (bytes->path (string->bytes/utf-8 b) 'unix)))))])
  (test "b/x/a" reroot-path/u "/x/a" "b")
  (test "b" reroot-path/u "/" "b")
  (test "b/x/y/z" reroot-path/u "//x//y//z" "b")
  (test "/tmp/b/x/y/z" reroot-path/u "//x//y//z" "/tmp/b"))

(let ([reroot-path/w
       (lambda (a b)
         (bytes->string/utf-8
          (path->bytes
           (reroot-path (bytes->path (string->bytes/utf-8 a) 'windows)
                        (bytes->path (string->bytes/utf-8 b) 'windows)))))])
  (test "b\\c\\x\\a" reroot-path/w "c:/x/a" "b")
  (test "b\\z\\" reroot-path/w "z:/" "b")
  (test "b\\UNC\\machine\\folder\\a" reroot-path/w "//machine/folder/a" "b")
  (test "q:/tmp/b\\x\\y\\z" reroot-path/w "x://y//z" "q:/tmp/b")
  (test "\\\\?\\q:\\tmp\\b\\c\\x/y" reroot-path/w "\\\\?\\c:\\x/y" "q:/tmp/b")
  (test "\\\\?\\q:\\tmp\\b\\UNC\\machine\\path\\x/y\\z" 
        reroot-path/w "\\\\?\\UNC\\machine\\path\\x/y\\z" "q:/tmp/b"))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
