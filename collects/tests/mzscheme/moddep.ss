;; FIXME: this file needs to test resolve-module-path for planet paths

(load-relative "loadtest.ss")

(Section 'moddep)

(require syntax/moddep)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resolve-module-path[-index]

(define (test-rmp expect path rel-to)
  (test expect resolve-module-path path rel-to)
  (let ([pi-rel-to (if (path? rel-to) 
		       rel-to 
		       (build-path (current-directory) "self"))])
    (test expect resolve-module-path-index 
	  (module-path-index-join path (module-path-index-join #f #f))
	  pi-rel-to)
    (test expect resolve-module-path-index 
	  (module-path-index-join path 
				  (module-path-index-join
				   "other.ss"
				   (module-path-index-join #f #f)))
	  pi-rel-to)))

(test-rmp (build-path (current-directory) "apple.ss") "apple.ss" #f)
(test-rmp (build-path (current-directory) "apple.ss") "apple.ss" (build-path (current-directory) "x.ss"))
(test-rmp (build-path (current-directory) "apple.ss") "apple.ss" current-directory)
(test-rmp (build-path (current-directory) 'up "apple.ss") "../apple.ss" #f)
(test-rmp (build-path (current-directory) 'up 'up "apple.ss") "../../apple.ss" #f)
(test-rmp (build-path (current-directory) 'same "apple.ss") "./apple.ss" #f)
(test-rmp (build-path (current-directory) "down" "apple.ss") "down/apple.ss" #f)

(test (build-path (current-directory) 'up 'up "apple.ss")
      resolve-module-path-index 
      (module-path-index-join "../apple.ss"
			      (module-path-index-join "../other.ss"
						      (module-path-index-join #f #f)))
      (build-path (current-directory) "f.ss"))
(test (build-path (current-directory) "only.ss")
      resolve-module-path-index 
      (module-path-index-join #f #f)
      (build-path (current-directory) "only.ss"))

(let ([mzlib (collection-path "mzlib")]
      [syntax (collection-path "syntax")])
  (test-rmp (build-path mzlib "x.ss") '(lib "x.ss") #f)
  (test-rmp (build-path mzlib "x.ss") '(lib "x.ss" "mzlib") #f)
  (test-rmp (build-path syntax "x.ss") '(lib "x.ss" "syntax") #f)
  (test-rmp (build-path syntax "private" "x.ss") '(lib "x.ss" "syntax" "private") #f)
  (test-rmp (build-path (current-directory) "x.ss") `(file ,(path->string (build-path (current-directory) "x.ss"))) #f)
  (test-rmp (build-path (current-directory) "x.ss") (build-path (current-directory) "x.ss") #f)
  (test-rmp (build-path (current-directory) "x.ss") (build-path "x.ss") #f)
  (void))


 
 
(err/rt-test (resolve-module-path "apple.ss" 'no))
(err/rt-test (resolve-module-path "/apple.ss" #f))
(err/rt-test (resolve-module-path "apple.ss/" #f))
(err/rt-test (resolve-module-path "app\u00E9le.ss" #f))

(err/rt-test (resolve-module-path-index "apple.ss" #f))
(err/rt-test (resolve-module-path-index (module-path-index-join #f #f) #f) exn:fail?)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collapse-module-path[-index]

(define (test-cmp expect path rel-to)
  (test expect collapse-module-path path rel-to)
  (test expect collapse-module-path-index 
	(module-path-index-join path (module-path-index-join #f #f))
	rel-to)
  (test expect collapse-module-path-index 
	(module-path-index-join path 
				(module-path-index-join
				 "other.ss"
				 (module-path-index-join #f #f)))
	rel-to))

(test-cmp '(lib "nonesuch/x.ss") "x.ss" '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "nonesuch/x.ss") "x.ss" (lambda () '(lib "y.ss" "nonesuch")))
(test-cmp '(lib "nonesuch/down/x.ss") "down/x.ss" '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "mzlib/x.ss") '(lib "x.ss") '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "nonesuch/x.ss") "../x.ss" '(lib "y.ss" "nonesuch/private"))
(test-cmp '(lib "nonesuch/x.ss") "../x.ss" '(lib "private/y.ss" "nonesuch"))
(test-cmp '(lib "alsonot/private/x.ss") '(lib "x.ss" "alsonot" "private") '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "x" "nonesuch") "x" '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "x" "nonesuch") "x" 'nonesuch/y)
(test-cmp '(lib "x" "nonesuch") "x" 'nonesuch)
(test-cmp '(lib "nonesuch/y.ss") 'nonesuch/y (current-directory))
(test-cmp '(lib "mzlib/nonesuch.ss") '(lib "nonesuch.ss") (current-directory))

(test-cmp (build-path (current-directory) "x.ss") "x.ss" (build-path (current-directory) "other"))
(test-cmp (build-path (current-directory) "x.ss")
	  "x.ss" 
	  `(file ,(path->string (build-path (current-directory) "other"))))
(test-cmp (build-path (current-directory) "x.ss")
	  (build-path "x.ss")
	  `(file ,(path->string (build-path (current-directory) "other"))))

(test-cmp '(planet "x.ss" ("usr" "pkg.plt" 1)) "x.ss" '(planet "y.ss" ("usr" "pkg.plt" 1)))
(test-cmp '(planet "x.ss" ("usr" "pkg.plt" 1 0)) "x.ss" (lambda () '(planet "y.ss" ("usr" "pkg.plt" 1 0))))
(test-cmp '(planet "x.ss" ("a" "p.plt" 1) "path") "path/x.ss" '(planet "z.ss" ("a" "p.plt" 1)))
(test-cmp '(planet "x.ss" ("a" "p.plt" 2) "path" "qq") "qq/x.ss" '(planet "path/z.ss" ("a" "p.plt" 2)))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(planet "o.ss" ("a" "q.plt" 54 3)))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(lib "o.ss" "nonesuch"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(file "q.ss"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "m/z:2/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 5)) '(planet "m/z:2:5/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 (= 5))) '(planet "m/z:2:=5/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 5)) '(planet "m/z:2:>=5/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 (- 5))) '(planet "m/z:2:<=5/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 (7 99))) '(planet "m/z:2:7-99/x.ss") (build-path "yikes"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2 (7 99))) '(planet "m/z:2:7-99/x.ss") (build-path "yikes"))

(test-cmp '(planet "utils.ss" ("untyped" "unlib.plt" 3 (= 6))) 
          "../utils.ss" '(planet "doc/stuff.ss" ("untyped" "unlib.plt" 3 (= 6))))
(test-cmp '(planet "utils.ss" ("untyped" "unlib.plt" 3 (= 6))) 
          "../utils.ss" '(planet "stuff.ss" ("untyped" "unlib.plt" 3 (= 6)) "doc"))
(test-cmp '(planet "utils.ss" ("untyped" "unlib.plt" 3 (= 6)) "down") 
          "../down/utils.ss" '(planet "stuff.ss" ("untyped" "unlib.plt" 3 (= 6)) "doc"))
(test-cmp '(planet "utils.ss" ("untyped" "unlib.plt" 3 (= 6)) "down") 
          "../down/utils.ss" '(planet untyped/unlib:3:=6/doc/stuff))

(test-cmp (build-path 'same "x.ss") "x.ss" (build-path 'same))

(test '(lib "bar/foo.ss")
      collapse-module-path-index 
      (module-path-index-join '(lib "foo.ss" "bar") (make-resolved-module-path 'nowhere))
      (current-directory))
(test (build-path (find-system-path 'temp-dir) "data.ss")
      collapse-module-path-index 
      (module-path-index-join '"data.ss" (make-resolved-module-path
                                          (build-path (find-system-path 'temp-dir) "prog.ss")))
      (current-directory))

;; Try path cases that don't fit UTF-8 (and therefore would go wrong as a string):
(let ([dir (build-path (current-directory) (bytes->path #"\xFF"))])
  (test-cmp (build-path dir "x.ss")
	    "x.ss" 
	    (build-path dir "other")))
(test-cmp (build-path (current-directory) (bytes->path #"\xFF"))
	  (bytes->path #"\xFF")
	  `(file ,(path->string (build-path (current-directory) "other"))))

(test '(lib "alsonot/x.ss")
      collapse-module-path-index 
      (module-path-index-join "x.ss"
			      (module-path-index-join
			       '(lib "y.ss" "alsonot")
			       (module-path-index-join #f #f)))
      '(lib "w.ss" "nonesuch"))

(err/rt-test (collapse-module-path "apple.ss" '(no)))
(err/rt-test (collapse-module-path "/apple.ss" (current-directory)))
(err/rt-test (collapse-module-path-index "apple.ss" (current-directory)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

