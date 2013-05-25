;; Written in #%kernel to avoid adding any module-attachment
;; dependencies. Initialized by the DrRacket integration tool.

(module drracket-link '#%kernel
  (#%provide link)

  #|

  If initialized (has non-#f value), the box should contain a vector
  of the following procedures:
  
  (vector get-errortrace-backtrace
          show-backtrace
          show-source)
  |#

  (define-values (link) (box #f)))
