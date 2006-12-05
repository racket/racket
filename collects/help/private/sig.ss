(module sig mzscheme
  (require (lib "unit.ss"))
  (provide gui^
           main^)
  
  (define-signature main^
    (add-help-desk-font-prefs))
  
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
