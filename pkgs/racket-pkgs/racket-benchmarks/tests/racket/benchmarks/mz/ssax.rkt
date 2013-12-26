#lang racket/base
(require (planet "ssax.ss" ("lizorkin" "ssax.plt" 2))
         racket/runtime-path)

(define-runtime-path input-file "input.xml")

(collect-garbage)
(time (void (ssax:xml->sxml
	     (open-input-file input-file)
	     null)))
