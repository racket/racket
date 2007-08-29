
(module drscheme-ext mzscheme
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "../model/trace.ss"
           "../model/deriv-c.ss"
           "../model/deriv-util.ss"
           "prefs.ss"
           "frame.ss")

  (provide macro-stepper-frame%
           macro-stepper-config/prefs%
           trace/result
           show-deriv/orig-parts)

  (define macro-stepper-frame%
    (macro-stepper-frame-mixin
     (frame:standard-menus-mixin
      frame:basic%)))
  
  ;; show-deriv/orig-parts
  ;; Strip off mzscheme's #%top-interaction
  ;; Careful: the #%top-interaction node may be inside of a lift-deriv
  (define (show-deriv/orig-parts deriv stepper-promise)
    ;; adjust-deriv/lift : Derivation -> (list-of Derivation)
    (define (adjust-deriv/lift deriv)
      (match deriv
        [(IntQ lift-deriv (e1 e2 first lifted-stx second))
         (let ([first (adjust-deriv/top first)])
           (and first
                (let ([e1 (lift/deriv-e1 first)])
                  (rewrap deriv 
                          (make-lift-deriv e1 e2 first lifted-stx second)))))]
        [else (adjust-deriv/top deriv)]))
    ;; adjust-deriv/top : Derivation -> Derivation
    (define (adjust-deriv/top deriv)
      (if (syntax-source (lift/deriv-e1 deriv))
          deriv
          ;; It's not original...
          ;; Strip out mzscheme's top-interactions
          ;; Keep anything that is a non-mzscheme top-interaction
          ;; Drop everything else (not original program)
          (match deriv
            [(IntQ mrule (e1 e2 tx next))
             (match tx
               [(AnyQ transformation (e1 e2 rs me1 me2 locals seq))
                (cond [(ormap (lambda (x)
                                (module-identifier=? x #'#%top-interaction))
                              rs)
                       ;; Just mzscheme's top-interaction; strip it out
                       (adjust-deriv/top next)]
                      [(equal? (map syntax-e rs) '(#%top-interaction))
                       ;; A *different* top interaction; keep it
                       deriv]
                      [else
                       ;; Not original and not tagged with top-interaction
                       #f])])]
            [else #f])))
    (let ([deriv* (adjust-deriv/lift deriv)])
      (when deriv* (send (force stepper-promise) add-deriv deriv*))))
  
  )
