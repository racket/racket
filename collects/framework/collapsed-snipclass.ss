(module collapsed-snipclass mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           "private/collapsed-snipclass-helpers.ss")
  
  (provide snip-class)

  (define simple-sexp-snip%
    (class* snip% (readable-snip<%>)
      (init-field left-bracket right-bracket saved-snips)
      (define/public (read-special file line col pos)
        (let ([text (make-object text%)])
          (for-each
           (λ (s) (send text insert (send s copy)
                             (send text last-position)
                             (send text last-position)))
           saved-snips)
	  (datum->syntax-object
	   #f
	   (read (open-input-text-editor text))
	   (list file line col pos 1))))
      (super-instantiate ())))
  
  (define sexp-snipclass% (make-sexp-snipclass% simple-sexp-snip%))
  
  (define snip-class (make-object sexp-snipclass%))
  (send snip-class set-classname (format "~s" '(lib "collapsed-snipclass.ss" "framework")))
  (send snip-class set-version 0)
  (send (get-the-snip-class-list) add snip-class))
