(require (planet "ssax.ss" ("lizorkin" "ssax.plt" 2)))

(collect-garbage)
(time (void (ssax:xml->sxml
	     (open-input-file (build-path
                               (current-load-relative-directory)
                               "input.xml"))
	     null)))
