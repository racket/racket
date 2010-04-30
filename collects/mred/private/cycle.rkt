(module cycle mzscheme

  ;; There are a few cycles that we can't eliminate without
  ;;  using state. This module implements that state

  (provide (protect get-get-file
		    set-get-file!
		    get-get-font-from-user
		    set-get-font-from-user!))
  
  (define get-file #f)
  (define (get-get-file) get-file)
  (define (set-get-file! f) (set! get-file f))

  (define get-font-from-user #f)
  (define (get-get-font-from-user) get-font-from-user)
  (define (set-get-font-from-user! f) (set! get-font-from-user f)))


  
