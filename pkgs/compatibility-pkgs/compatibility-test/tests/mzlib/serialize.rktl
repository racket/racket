
(load-relative "loadtest.rktl")

(Section 'serialization)

(module ser-mod mzscheme
   (require mzlib/serialize)
   (provide ser-mod-test)

   (define-serializable-struct foo (a b))

   (define (ser-mod-test)
     (foo-a (deserialize (serialize (make-foo 1 2))))))

(require 'ser-mod)
(test 1 ser-mod-test)
