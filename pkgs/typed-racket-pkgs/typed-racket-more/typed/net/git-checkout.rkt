#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a typed version of net/git-checkout

(require net/git-checkout)

(type-environment
 [git-checkout
  (->key -String -String
         #:dest-dir (-opt -Pathlike) #t
         #:ref -String #f
         #:transport (one-of/c 'git 'http 'https) #f
         #:depth (-opt -Integer) #f
         #:status-printf (->* (list -String) Univ -Void) #f
         #:tmp-dir (-opt -Pathlike) #f
         #:clean-tmp-dir? Univ #f
         #:verify-server? Univ #f
         #:port -Integer #f
         -String)])
