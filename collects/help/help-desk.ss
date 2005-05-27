(module help-desk mzscheme
  (require 
   "bug-report.ss" ;; this is require'd here to get the prefs defaults setup done early.
   
   "private/manuals.ss"
   "private/buginfo.ss"
   "private/standard-urls.ss"
   
   "private/link.ss"
   (lib "contract.ss"))
  
  (provide help-desk-frame<%>)
  
  (define (goto-hd-location x) (error 'goto-hd-location "no"))
  
  (provide/contract 
   (set-bug-report-info! any/c)
   (find-doc-names (-> (listof (cons/c path? string?))))
   (goto-manual-link (any/c string? string? . -> . any))
   
   (goto-hd-location (symbol? . -> . any))
   (new-help-desk (-> (is-a?/c help-desk-frame<%>)))
   (show-help-desk (-> any))
   (add-help-desk-mixin (-> mixin-contract void?))
   (search-for-docs (string?
                     search-type?
                     search-how?
                     any/c
                     (listof path?) ;; manual names
                     . -> .
                     any))
   (find-help-desk-frame (-> (union false/c (is-a?/c help-desk-frame<%>))))
   (search-for-docs/in-frame ((is-a?/c help-desk-frame<%>)
                              string?
                              search-type?
                              search-how?
                              any/c
                              (listof path?) ;; manual names
                              . -> .
                              any))))
