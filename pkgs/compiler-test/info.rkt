#lang info

(define collection 'multi)

(define deps '("base" "icons"))

(define pkg-desc "tests for \"compiler-lib\"")

(define pkg-authors '(mflatt))
(define build-deps '("compiler-lib"
                     "eli-tester"
                     "rackunit-lib"
                     "net-lib"
                     "scheme-lib"
                     "compatibility-lib"
                     "gui-lib"
                     "htdp-lib"
                     "plai-lib"
                     "rackunit-lib"
		     "dynext-lib"
		     "mzscheme-lib"))
(define update-implies '("compiler-lib"))
