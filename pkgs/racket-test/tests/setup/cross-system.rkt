#lang racket/base
(require setup/cross-system
         rackunit)

(check-true (symbol? (cross-system-type)))
(check-equal? (cross-system-type) (cross-system-type 'os))
(check-true (string? (cross-system-type 'machine)))
(check-true (symbol? (cross-system-type 'link)))
(check-true (symbol? (cross-system-type 'os*)))
(check-true (symbol? (cross-system-type 'arch)))
(check-true (symbol? (cross-system-type 'so-find)))
(check-true (string? (cross-system-type 'platform)))
(check-true (relative-path? (system-library-subpath)))
(check-true (relative-path? (system-library-subpath #f)))
(check-equal? (cross-system-type 'platform) (path->string (system-library-subpath #f)))
(check-true (pair? (memv (cross-system-type 'word) '(32 64))))
(check-equal? (fixnum? (expt 2 32)) (= (cross-system-type 'word) 64))
