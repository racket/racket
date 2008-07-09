#lang typed-scheme

(parameterize ([current-directory ".."])
  (current-directory)
  (current-directory ".."))
