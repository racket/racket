#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/head
  [empty-header String]
  [validate-header (String -> Void)]
  [extract-field (Bytes (U Bytes String) -> (Option Bytes))]
  [remove-field (String String -> String)]
  [insert-field (String String String -> String)]
  [replace-field (String String String -> String)]
  [extract-all-fields ((U String Bytes) -> (Listof (cons (U String Bytes) (U Bytes String))))]
  [append-headers (String String -> String)]
  [standard-message-header (String (Listof String) (Listof String) (Listof String) String -> String)]
  [data-lines->data ((Listof String) -> String)]
  [extract-addresses (String Symbol -> (U (Listof String) (Listof (Listof String))))]
  [assemble-address-field ((Listof String) -> String)])

(provide
 empty-header
 validate-header
 extract-field
 remove-field
 insert-field
 replace-field
 extract-all-fields
 append-headers
 standard-message-header
 data-lines->data
 extract-addresses
 assemble-address-field)
