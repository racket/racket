#lang scheme
(require plai/datatype
         plai/test-harness
         web-server/servlet
         (prefix-in insta: web-server/insta/insta))

(provide (all-from-out plai/datatype)
         (all-from-out web-server/servlet)
         (except-out (all-from-out scheme) error #%module-begin)
         (except-out (all-from-out plai/test-harness) plai-error)
         (rename-out [plai-error error]
                     [insta:no-web-browser no-web-browser]
                     [insta:static-files-path static-files-path] 
                     [insta:#%module-begin #%module-begin]))

