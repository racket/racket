(module cachepath mzscheme

  (require "config.ss")
  (provide get-planet-cache-path)

  ;; get-planet-cache-path : -> path[absolute, file]
  ;; the path to the cache.ss file for the planet installation
  ;; SIDE EFFECT: creates the directory if it doesn't already exist
  (define (get-planet-cache-path)
    (let ((path (build-path (PLANET-DIR) "cache.ss")))
      path)))