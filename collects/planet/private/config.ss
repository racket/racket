(module config mzscheme
  
  (require "planet-shared.ss")
  
  (define-parameters (PLANET-SERVER (make-tcp-resource "localhost" 10000))
                     (PLANET-DIR    (this-expression-source-directory))
                     (CACHE-DIR     (build-path (PLANET-DIR) "planet-cache"))
                     (LINKAGE-FILE  (build-path (PLANET-DIR) "LINKAGE"))                  
                     (LOGGING-ENABLED? #t)
                     (LOG-FILE      (build-path (PLANET-DIR) "INSTALL-LOG"))))