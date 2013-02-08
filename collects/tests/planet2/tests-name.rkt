#lang racket/base
(require rackunit
         planet2/name
         "util.rkt")

(define-syntax check-equal-values?
  (syntax-rules (values)
    [(_ expr (values a ...))
     (check-equal? (call-with-values (lambda () expr) list) (list a ...))])) 

(define (run-pkg-tests)
  (check-equal-values? (package-source->name+type "" #f) (values #f #f))

  (check-equal-values? (package-source->name+type "fish" #f) (values "fish" 'name))
  (check-equal-values? (package-source->name+type "fish" 'name) (values "fish" 'name))
  (check-equal-values? (package-source->name+type "fish!" 'name) (values #f 'name))
  (check-equal-values? (package-source->name+type "fish/" 'name) (values #f 'name))
  (check-equal-values? (package-source->name+type "fish123A_B-C" #f) (values "fish123A_B-C" 'name))
  (check-equal-values? (package-source->name+type "fish123A_B-C!" 'name) (values #f 'name))

  (check-equal-values? (package-source->name+type "fish.plt" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.zip" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.tar" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.tgz" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.tar.gz" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "ocean/fish.tar.gz" #f) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.plt" 'file) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.tar.gz" 'file) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish.other" 'file) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish" 'file) (values "fish" 'file))
  (check-equal-values? (package-source->name+type "fish!" 'file) (values #f 'file))

  (check-equal-values? (package-source->name+type "fish/" #f) (values "fish" 'dir))
  (check-equal-values? (package-source->name+type "./fish" #f) (values "fish" 'dir))
  (check-equal-values? (package-source->name+type "sub/fish" #f) (values "fish" 'dir))
  (check-equal-values? (package-source->name+type "fish/" 'dir) (values "fish" 'dir))
  (check-equal-values? (package-source->name+type "fish/" 'link) (values "fish" 'link))
  (check-equal-values? (package-source->name+type "fish" 'dir) (values "fish" 'dir))
  (check-equal-values? (package-source->name+type "fish!/" 'dir) (values #f 'dir))

  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish.plt" #f) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "https://racket-lang.org/fish.plt" #f) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish.tar.gz" #f) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish" 'file-url) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "fish" 'file-url) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "dir/fish" 'file-url) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "fish/" 'file-url) (values "fish" 'file-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish!" 'file-url) (values #f 'file-url))

  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish/" #f) (values "fish" 'dir-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish/" 'dir-url) (values "fish" 'dir-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish" 'dir-url) (values "fish" 'dir-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish.plt" 'dir-url) (values #f 'dir-url))
  (check-equal-values? (package-source->name+type "http://racket-lang.org/fish" #f) (values "fish" 'dir-url))

  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/master" #f) (values "fish" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish.rkt/master" #f) (values #f 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/release" #f) (values "fish" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/release/catfish" #f) (values "catfish" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/release/catfish/" #f) (values "catfish" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/release/catfish/bill" #f) (values "bill" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish/master" 'github) (values "fish" 'github))
  (check-equal-values? (package-source->name+type "racket/fish/master" 'github) (values "fish" 'github))
  (check-equal-values? (package-source->name+type "racket/fish/master/" 'github) (values "fish" 'github))
  (check-equal-values? (package-source->name+type "github://github.com/fish/master" 'github) (values #f 'github))
  (check-equal-values? (package-source->name+type "fish/master" 'github) (values #f 'github))
  (check-equal-values? (package-source->name+type "github://github.com/racket/fish.more/release" 'github) (values #f 'github))

  (check-equal-values? (package-source->name+type "random://racket-lang.org/fish.plt" #f) (values #f #f))

  (void))

(provide run-pkg-tests)

(module+ main
  (run-pkg-tests* run-pkg-tests))
