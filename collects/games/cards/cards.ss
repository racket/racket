
(module cards mzscheme
  (require "main.ss"
	   "utils.ss"
	   "region.ss")

  (provide region
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

