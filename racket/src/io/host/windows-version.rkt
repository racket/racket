#lang racket/base
(require (only-in '#%linklet primitive-table))

(provide get-windows-version)

(define windows-version-table
  (or (primitive-table '#%windows-version)
      (error '#%windows-version "windows-version not supported by host")))

(define get-windows-version
  (hash-ref windows-version-table 'get-windows-version))
