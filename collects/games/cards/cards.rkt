(module cards mzscheme
  (require "base.rkt"
           "utils.rkt"
           "region.rkt")

  (provide table<%> card<%>
           region struct:region
	   make-region
	   region? region-x region-y region-w region-h 
	   region-label region-callback region-interactive-callback
	   set-region-callback!
	   set-region-interactive-callback!
	   make-button-region
	   make-background-region

	   make-deck make-card
	   make-table

	   shuffle-list))
