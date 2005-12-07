(module sig mzscheme
  (require (lib "unitsig.ss"))
  (provide gui^)
  
  (define-signature gui^
    (help-desk-frame<%>
     add-help-desk-mixin
     new-help-desk
     find-help-desk-frame
     show-help-desk
     goto-hd-location
     goto-manual-link
     search-for-docs
     search-for-docs/in-frame)))
