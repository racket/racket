#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "reading and writing configs"
   $ "raco pkg config catalogs" =stdout> "https://pkg.racket-lang.org\nhttps://planet-compat.racket-lang.org\n"
   $ "raco pkg config --set catalogs http://localhost:9000"
   $ "raco pkg config catalogs" =stdout> "http://localhost:9000\n")))
