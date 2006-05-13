;; FIXME: this file needs to test resolve-module-path for planet paths

(load-relative "loadtest.ss")

(SECTION 'MODDEP)

(require (lib "moddep.ss" "syntax"))

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

(test-cmp '(lib "x.ss" "nonesuch") "x.ss" '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "x.ss" "nonesuch") "x.ss" (lambda () '(lib "y.ss" "nonesuch")))
(test-cmp '(lib "down/x.ss" "nonesuch") "down/x.ss" '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "x.ss" "mzlib") '(lib "x.ss") '(lib "y.ss" "nonesuch"))
(test-cmp '(lib "../x.ss" "nonesuch/private") "../x.ss" '(lib "y.ss" "nonesuch/private"))
(test-cmp '(lib "private/../x.ss" "nonesuch") "../x.ss" '(lib "private/y.ss" "nonesuch"))
(test-cmp '(lib "private/x.ss" "alsonot") '(lib "x.ss" "alsonot" "private") '(lib "y.ss" "nonesuch"))

(test-cmp (build-path (current-directory) "x.ss") "x.ss" (build-path (current-directory) "other"))
(test-cmp `(file ,(path->string (build-path (current-directory) "x.ss")))
	  "x.ss" 
	  `(file ,(path->string (build-path (current-directory) "other"))))
(test-cmp (build-path (current-directory) "x.ss")
	  (build-path "x.ss")
	  `(file ,(path->string (build-path (current-directory) "other"))))

(test-cmp '(planet "x.ss" ("usr" "pkg.plt" 1)) "x.ss" '(planet "y.ss" ("usr" "pkg.plt" 1)))
(test-cmp '(planet "x.ss" ("usr" "pkg.plt" 1 0)) "x.ss" (lambda () '(planet "y.ss" ("usr" "pkg.plt" 1 0))))
(test-cmp '(planet "path/x.ss" ("a" "p.plt" 1)) "path/x.ss" '(planet "z.ss" ("a" "p.plt" 1)))
(test-cmp '(planet "path/../x.ss" ("a" "p.plt" 2)) "../x.ss" '(planet "path/z.ss" ("a" "p.plt" 2)))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(planet "o.ss" ("a" "q.plt" 54 3)))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(lib "o.ss" "nonesuch"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) '(file "q.ss"))
(test-cmp '(planet "x.ss" ("m" "z.plt" 2)) '(planet "x.ss" ("m" "z.plt" 2)) "where/in/the/world/cs.ss")

(test-cmp "./x.ss" "x.ss" ".")

;; Try path cases that don't fit UTF-8 (and therefore would go wrong as a string):
(let ([dir (build-path (current-directory) (bytes->path #"\xFF"))])
  (test-cmp (build-path dir "x.ss")
	    "x.ss" 
	    (build-path dir "other")))
(test-cmp (build-path (current-directory) (bytes->path #"\xFF"))
	  (bytes->path #"\xFF")
	  `(file ,(path->string (build-path (current-directory) "other"))))

(test '(lib "x.ss" "alsonot")
      collapse-module-path-index 
      (module-path-index-join "x.ss"
			      (module-path-index-join
			       '(lib "y.ss" "alsonot")
			       (module-path-index-join #f #f)))
      '(lib "w.ss" "nonesuch"))

(err/rt-test (collapse-module-path "apple.ss" 'no))
(err/rt-test (collapse-module-path "/apple.ss" (current-directory)))
(err/rt-test (collapse-module-path-index "apple.ss" (current-directory)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

