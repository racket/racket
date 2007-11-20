#lang scheme/base

(require "private/search.ss")
(define argv (current-command-line-arguments))
(when (equal? argv #())
  (error 'help-desk "expected a search term on the command line"))
(generate-search-results (vector->list argv))
