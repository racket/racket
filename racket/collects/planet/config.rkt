#lang racket/base
(require "private/define-config.rkt")
(define-parameters
  (PLANET-SERVER-NAME       "planet.racket-lang.org")
  (PLANET-SERVER-PORT       270)
  (PLANET-CODE-VERSION      "300")
  (PLANET-BASE-DIR         (let ([plt-planet-dir-env-var (getenv "PLTPLANETDIR")])
                             (if plt-planet-dir-env-var
                                 (string->path plt-planet-dir-env-var)
                                 (build-path (find-system-path 'addon-dir)
                                             "planet"
                                             (PLANET-CODE-VERSION)))))
  (PLANET-DIR               (build-path (PLANET-BASE-DIR) (version)))
  (CACHE-DIR                (build-path (PLANET-DIR) "cache"))
  (UNINSTALLED-PACKAGE-CACHE (build-path (PLANET-BASE-DIR) "packages"))
  (LINKAGE-FILE             (build-path (PLANET-DIR) "LINKAGE"))
  (HARD-LINK-FILE           (build-path (PLANET-BASE-DIR) (version) "HARD-LINKS"))
  (LOG-FILE                 (build-path (PLANET-DIR) "INSTALL-LOG"))
  (DEFAULT-PACKAGE-LANGUAGE (version))
  
  (USE-HTTP-DOWNLOADS?       #t)
  (HTTP-DOWNLOAD-SERVLET-URL (let ([plt-planet-url-env-var (getenv "PLTPLANETURL")])
                               (or plt-planet-url-env-var
                                   "http://planet.racket-lang.org/servlets/planet-servlet.ss")))
  (PLANET-ARCHIVE-FILTER     #f))
