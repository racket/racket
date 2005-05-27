
(module xml-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "xml-sig.ss" "private/sig.ss"
	  "private/structures.ss"
	  "private/reader.ss"
	  "private/writer.ss"
	  "private/xexpr.ss"
	  "private/space.ss"
          "private/syntax.ss")

  (provide xml@)

  (define xml@
    (compound-unit/sig
     (import)
     (link
      [S : xml-structs^ (xml-structs@)]
      [SS : xml-structs^ (syntax-structs@)]
      [R : reader^ (reader@ S)]
      [R2 : reader^ (reader@ SS)]
      (U : writer^ (writer@ S))
      (T : xexpr^ (xexpr@ S U))
      (W : space^ (space@ S)))
     (export (open S) (open R) (var (R2 read-xml) syntax:read-xml)
             (var (R2 read-xml/element) syntax:read-xml/element)
             (open U) (open T) (open W)))))
