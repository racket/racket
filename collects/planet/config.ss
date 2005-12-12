(module config mzscheme
  
  (require "private/planet-shared.ss")
  
  (define-parameters
    (PLANET-SERVER-NAME       "planet.plt-scheme.org")
    (PLANET-SERVER-PORT       270)
    (PLANET-CODE-VERSION      "300")
    (PLANET-DIR               (if (getenv "PLTPLANETDIR")
                                  (string->path (getenv "PLTPLANETDIR"))
                                  (build-path (find-system-path 'addon-dir) "planet" (PLANET-CODE-VERSION) (version))))
    (CACHE-DIR                (build-path (PLANET-DIR) "cache"))
    (LINKAGE-FILE             (build-path (PLANET-DIR) "LINKAGE"))
    (LOGGING-ENABLED?         #t)
    (LOG-FILE                 (build-path (PLANET-DIR) "INSTALL-LOG"))
    (DEFAULT-PACKAGE-LANGUAGE (version))
    
    (USE-HTTP-DOWNLOADS?       #t)
    (HTTP-DOWNLOAD-SERVLET-URL "http://planet.plt-scheme.org/servlets/planet-servlet.ss")))
    
