(module config mzscheme
  (require "private/define-config.ss") 
  (define-parameters
    (PLANET-SERVER-NAME       "planet.plt-scheme.org")
    (PLANET-SERVER-PORT       270)
    (PLANET-CODE-VERSION      "300")
    (PLANET-BASE-DIR         (if (getenv "PLTPLANETDIR")
                                 (string->path (getenv "PLTPLANETDIR"))
                                 (build-path (find-system-path 'addon-dir)
                                             "planet"
                                             (PLANET-CODE-VERSION))))
    (PLANET-DIR               (build-path (PLANET-BASE-DIR) (version)))
    (CACHE-DIR                (build-path (PLANET-DIR) "cache"))
    (UNINSTALLED-PACKAGE-CACHE (build-path (PLANET-BASE-DIR) "packages"))
    (LINKAGE-FILE             (build-path (PLANET-DIR) "LINKAGE"))
    (HARD-LINK-FILE           (build-path (PLANET-BASE-DIR) "HARD-LINKS"))
    (LOGGING-ENABLED?         #t)
    (LOG-FILE                 (build-path (PLANET-DIR) "INSTALL-LOG"))
    (DEFAULT-PACKAGE-LANGUAGE (version))
    
    (USE-HTTP-DOWNLOADS?       #t)
    (HTTP-DOWNLOAD-SERVLET-URL "http://planet.plt-scheme.org/servlets/planet-servlet.ss")
    (PLANET-ARCHIVE-FILTER     #f)))
    
