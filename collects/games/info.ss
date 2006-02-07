(module info (lib "infotab.ss" "setup")
  (define name "Games")
  (define doc.txt "doc.txt")
  (define mred-launcher-libraries (list "games.ss"))
  (define mred-launcher-names (list "PLT Games"))
  (define doc-sub-collections
    (list "cards" "paint-by-numbers" "same" "lights-out" "aces" "spider"
	  "memory" "pousse" "crazy8s"
          "gcalc" "parcheesi" "gl-board-game"
	  "jewel"))
  (define blurb 
    (list "Demos a few small "
	  '(a ((MZSCHEME "
(begin
  (require (lib |mred.ss| |mred|))
  (parameterize ([current-custodian (make-custodian)]) 
    (parameterize ([current-eventspace (make-eventspace)])
      (queue-callback 
       (lambda ()
         (exit-handler void) 
         (dynamic-require (quote (lib |games.ss| |games|)) #f))))))")) "games")
	  " implemented in Scheme.")))

