
(module head-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:head^)
  (define-signature net:head^
    (empty-header
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
     assemble-address-field)))

