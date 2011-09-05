#lang typed/racket/base

(require typed/private/utils)

(require/opaque-type Cookie cookie? net/cookie)

(require/typed/provide net/cookie
  [set-cookie (String String -> Cookie)]
  [cookie:add-comment (Cookie String -> Cookie)]
  [cookie:add-domain (Cookie String -> Cookie)]
  [cookie:add-max-age (Cookie Number -> Cookie)]
  [cookie:add-path (Cookie String -> Cookie)]
  [cookie:secure (Cookie Boolean -> Cookie)]
  [cookie:version (Cookie Number -> Cookie)]

  [print-cookie (Cookie -> String)]

  [get-cookie (String String -> (Listof String))]
  [get-cookie/single (String String -> (Option String))])

(require-typed-struct (cookie-error exn:fail) () #:extra-constructor-name make-cookie-error net/cookie)

(provide Cookie cookie? (struct-out cookie-error))
