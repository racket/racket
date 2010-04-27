#lang typed-scheme

(require scheme/match)

(match "abc"
  [(regexp "^abc") 1])

(match (list 1 1)
  [(list x x) 1])
