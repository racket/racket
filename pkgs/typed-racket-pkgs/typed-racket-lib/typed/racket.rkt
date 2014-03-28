#lang typed-racket/minimal

(require typed/racket/base racket/require
         (subtract-in racket typed/racket/base racket/contract
                      typed/racket/class)
         typed/racket/class
	 (for-syntax racket/base))
(provide (all-from-out racket
                       typed/racket/base
                       typed/racket/class)
	 (for-syntax (all-from-out racket/base))
         class)
