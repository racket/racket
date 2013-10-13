#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "reading and writing configs"
   $ "raco pkg config catalogs" =stdout> "http://pkgs.racket-lang.org\nhttp://planet-compats.racket-lang.org\n"
   $ "raco pkg config -u --set catalogs http://localhost:9000"
   $ "raco pkg config -u catalogs" =stdout> "http://localhost:9000\n"

   ;; can set default scope:
   $ "raco pkg config --set -u default-scope installation"
   $ "raco pkg config -u default-scope" =stdout> "installation\n"
   $ "raco pkg config -i default-scope" =stdout> "user\n"
   $ "raco pkg config default-scope" =stdout> "user\n"
   $ "raco pkg config -u catalogs" =stdout> "http://localhost:9000\n")))
