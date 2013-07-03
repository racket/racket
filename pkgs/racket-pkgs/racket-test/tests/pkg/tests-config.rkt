#lang racket/base
(require "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "reading and writing configs"
   $ "raco pkg config catalogs" =stdout> "https://pkg.racket-lang.org\nhttps://planet-compat.racket-lang.org\n"
   $ "raco pkg config -s --set catalogs http://localhost:9000"
   $ "raco pkg config -s catalogs" =stdout> "http://localhost:9000\n"

   ;; shared value inherited as user:
   $ "raco pkg config catalogs" =stdout> "http://localhost:9000\n"

   ;; user separate from shared:
   $ "raco pkg config --set -u catalogs http://localhost:0999"
   $ "raco pkg config -u catalogs" =stdout> "http://localhost:0999\n"
   $ "raco pkg config -s catalogs" =stdout> "http://localhost:9000\n"

   ;; can set default scope:
   $ "raco pkg config --set -u default-scope shared"
   $ "raco pkg config -u default-scope" =stdout> "shared\n"
   $ "raco pkg config -s default-scope" =stdout> "user\n"
   $ "raco pkg config default-scope" =stdout> "user\n"
   $ "raco pkg config catalogs" =stdout> "http://localhost:9000\n")))
