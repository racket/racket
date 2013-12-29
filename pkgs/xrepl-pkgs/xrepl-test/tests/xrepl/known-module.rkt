#lang racket/base

(require xrepl/xrepl tests/eli-tester setup/dirs)

(define known-module
  (parameterize ([current-namespace (module->namespace 'xrepl/xrepl)])
    (namespace-variable-value 'known-module)))

(define-namespace-anchor a)
(parameterize ([current-namespace (namespace-anchor->namespace a)])
  (for-each eval '[(module top-module racket/base)
                   (module version/utils racket/base)]))

(define utils-path     (collection-file-path "utils.rkt" "version"))
(define utils-pathstr  (path->string utils-path))
(define base-path    (collection-file-path "base.rkt" "racket"))
(define base-pathstr (path->string base-path))

(provide test-known-module)
(module+ main (test-known-module))
(module+ test (test-known-module))
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
     (known-module 'version/utils)    => 'version/utils ; symbol known, no guessing
     (known-module ''version/utils)   => ''version/utils
     (known-module "version/utils.rkt") => "version/utils.rkt"
     (known-module `(file ,utils-pathstr)) => `(file ,utils-pathstr)
     ;; path mode
     (known-module 'does-not-exist 'path) => #f
     (known-module ''top-module    'path) => #f
     (known-module 'top-module     'path) => #f
     (known-module ''#%kernel      'path) => #f
     (known-module '#%kernel       'path) => #f
     (known-module 'racket/base    'path) => base-path
     (known-module ''racket/base   'path) => #f
     (known-module 'version/utils      'path) => utils-path
     (known-module ''version/utils     'path) => #f
     (known-module "version/utils.rkt" 'path) => utils-path
     (known-module `(file ,utils-pathstr) 'path) => utils-path
     ;; path/sym mode
     (known-module 'does-not-exist 'path/sym) => #f
     (known-module ''top-module    'path/sym) => 'top-module
     (known-module 'top-module     'path/sym) => 'top-module
     (known-module ''#%kernel      'path/sym) => '#%kernel
     (known-module '#%kernel       'path/sym) => '#%kernel
     (known-module 'racket/base    'path/sym) => base-path
     (known-module ''racket/base   'path/sym) => #f
     (known-module 'version/utils      'path/sym) => utils-path
     (known-module ''version/utils     'path/sym) => 'version/utils ; we have it too
     (known-module "version/utils.rkt" 'path/sym) => utils-path
     (known-module `(file ,utils-pathstr) 'path/sym) => utils-path
     )))
