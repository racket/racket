#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(this-test-is-run-by-the-main-test)

(pkg-tests
 (with-fake-root
  (shelly-case
   "planet compatibility tests - no deps"
   $ "raco pkg install planet-dyoo-stardate1"
   $ "racket -e '(require dyoo/stardate1/main)'"))

 (with-fake-root
  (shelly-case
   "planet compatibility tests - deps"
   $ "raco pkg install --deps search-auto planet-dyoo-union-find1"
   $ "racket -e '(require dyoo/union-find1/test-union-find)'")))
