#lang setup/infotab

(define game "paint-by-numbers.ss")
(define game-set "Puzzle Games")
(define compile-omit-paths
  '(;; Skipped because it's huge - lots of data-encoding units
    "all-problems.ss"
    ;; Skipped because it requires all-problems.ss
    "paint-by-numbers.ss"
    ;; Skipped because these are used only to build the huge units.
    "build-hattori.ss"
    "build-kajitani.ss"
    "build-problems.ss"
    "raw-hattori.ss"
    "raw-kajitani.ss"
    "raw-problems.ss"
    "raw-misc.ss"
    "build-rows-cols.ss"
    "count-missing.ss"
    "main.ss"
    ;; directories too
    "hattori"
    "problems"
    "raw-problems"
    "solution-sets"))
