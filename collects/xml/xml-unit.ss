#lang scheme
(require "xml-sig.ss" 
         "private/sig.ss"
         "private/structures.ss"
         "private/reader.ss"
         "private/writer.ss"
         "private/xexpr.ss"
         "private/space.ss"
         "private/syntax.ss")

(provide xml@)

(define-compound-unit/infer xml@
  (import)
  (export xml-structs^ reader^ xml-syntax^ writer^ xexpr^ space^)
  (link   xml-structs@ reader@ native-xml-syntax@ writer@ xexpr@ space@))
