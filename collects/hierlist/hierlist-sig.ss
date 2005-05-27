
(module hierlist-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide hierlist^)
  (define-signature hierlist^
    (hierarchical-list%
     hierarchical-list-item<%>
     hierarchical-list-item%
     hierarchical-list-compound-item<%>
     hierarchical-list-compound-item%
     
     hierarchical-item-snip%
     hierarchical-list-snip%)))


