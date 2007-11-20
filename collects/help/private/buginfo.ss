(module buginfo mzscheme

  (provide set-bug-report-info!
           get-bug-report-infos
           bri-label
           bri-value)

  (define-struct bri (label get-value))
  (define (bri-value bri) ((bri-get-value bri)))
  
  ; update with symbol/string assoc list
  (define bug-report-infos null)

  (define (set-bug-report-info! str thunk)
    (set! bug-report-infos (cons (make-bri str thunk) bug-report-infos)))

  (define (get-bug-report-infos) bug-report-infos))




