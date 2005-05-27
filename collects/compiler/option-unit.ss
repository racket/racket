
(module option-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "sig.ss")

  (provide compiler:option@)

  (define compiler:option@
    (unit/sig compiler:option^
      (import)
      
      (define propagate-constants (make-parameter #t))
      (define assume-primitives (make-parameter #f))
      (define stupid (make-parameter #f))
      
      (define vehicles (make-parameter 'vehicles:automatic))
      (define vehicles:monoliths (make-parameter 1))
      (define seed (make-parameter 2001))
      (define max-monoliths 32)
      
      (define max-inline-size (make-parameter 50))

      (define unsafe (make-parameter #f))
      (define disable-interrupts (make-parameter #f))
      (define fixnum-arithmetic (make-parameter #f))

      (define verbose (make-parameter #f))
      (define debug (make-parameter #f))
      (define test (make-parameter #f))
      (define clean-intermediate-files (make-parameter #t))

      (define max-exprs-per-top-level-set (make-parameter 25))
      
      (define setup-prefix (make-parameter ""))

      (define compile-subcollections (make-parameter #t))
      (define compile-for-embedded (make-parameter #f))
      
      ;; Maybe #f helps for register-poor architectures?
      (define unpack-environments (make-parameter #f)))))
