(module help-desk mzscheme
  (require 
   "bug-report.ss" ;; this is require'd here to get the prefs defaults setup done early.
   
   "private/manuals.ss"
   "private/buginfo.ss"
   "private/standard-urls.ss"
   
   "private/link.ss"
   (lib "contract.ss"))
  
  (provide help-desk-frame<%>)
  
  (provide/contract 
   (add-help-desk-font-prefs (boolean? . -> . any))
   (set-bug-report-info! any/c)
   (find-doc-names (-> (listof (cons/c path? string?))))
   (goto-manual-link (string? string? . -> . any))
   
   (goto-hd-location ((symbols 'hd-tour 'release-notes 'plt-license) . -> . any))
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
