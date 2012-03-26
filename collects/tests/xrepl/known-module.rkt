#lang racket/base

(require xrepl/xrepl tests/eli-tester setup/dirs)

(define known-module
  (parameterize ([current-namespace (module->namespace 'xrepl/xrepl)])
    (namespace-variable-value 'known-module)))

(define-namespace-anchor a)
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (for-each eval '[(module top-module racket/base)
                   (module mzlib/etc racket/base)]))

(define etc-path     (collection-file-path "etc.rkt" "mzlib"))
(define etc-pathstr  (path->string etc-path))
(define base-path    (collection-file-path "base.rkt" "racket"))
(define base-pathstr (path->string base-path))

(provide test-known-module)
(module+ main (test-known-module))
(define (test-known-module)
  (parameterize ([current-directory (find-collects-dir)])
    (test
     ;; normal mode
     (known-module 'does-not-exist) => #f
     (known-module ''top-module)  => ''top-module
     (known-module 'top-module)   => ''top-module ; guesses the quoted name
     (known-module ''#%kernel)    => ''#%kernel
     (known-module '#%kernel)     => ''#%kernel   ; same here
     (known-module 'racket/base)  => 'racket/base
     (known-module ''racket/base) => #f ; the quoted name doesn't work
     (known-module 'mzlib/etc)    => 'mzlib/etc ; symbol known, no guessing
     (known-module ''mzlib/etc)   => ''mzlib/etc
     (known-module "mzlib/etc.rkt") => "mzlib/etc.rkt"
     (known-module `(file ,etc-pathstr)) => `(file ,etc-pathstr)
     ;; path mode
     (known-module 'does-not-exist 'path) => #f
     (known-module ''top-module    'path) => #f
     (known-module 'top-module     'path) => #f
     (known-module ''#%kernel      'path) => #f
     (known-module '#%kernel       'path) => #f
     (known-module 'racket/base    'path) => base-path
     (known-module ''racket/base   'path) => #f
     (known-module 'mzlib/etc      'path) => etc-path
     (known-module ''mzlib/etc     'path) => #f
     (known-module "mzlib/etc.rkt" 'path) => etc-path
     (known-module `(file ,etc-pathstr) 'path) => etc-path
     ;; path/sym mode
     (known-module 'does-not-exist 'path/sym) => #f
     (known-module ''top-module    'path/sym) => 'top-module
     (known-module 'top-module     'path/sym) => 'top-module
     (known-module ''#%kernel      'path/sym) => '#%kernel
     (known-module '#%kernel       'path/sym) => '#%kernel
     (known-module 'racket/base    'path/sym) => base-path
     (known-module ''racket/base   'path/sym) => #f
     (known-module 'mzlib/etc      'path/sym) => etc-path
     (known-module ''mzlib/etc     'path/sym) => 'mzlib/etc ; we have it too
     (known-module "mzlib/etc.rkt" 'path/sym) => etc-path
     (known-module `(file ,etc-pathstr) 'path/sym) => etc-path
     )))
