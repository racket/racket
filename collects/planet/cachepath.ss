(module cachepath mzscheme

  (require "config.ss")
  (provide get-planet-cache-path)

  ;; get-planet-cache-path : -> path[absolute, file]
  ;; the path to the cache.ss file for the planet installation
  ;; (n.b. this used to have the side effect of creating the path
  ;; if it didn't exist, but since this function may be run at
  ;; setup time and setup-time programs must not create this sort 
  ;; of directory, it doesn't do that anymore)
  (define (get-planet-cache-path)
    (let ((path (build-path (PLANET-DIR) "cache.ss")))
      path)))