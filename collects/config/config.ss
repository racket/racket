
;; The config module doesn't have to use "configtab.ss";
;; it just has to have the right exports. But using
;; "configtab.ss" makes it easier to generate the
;; code at install time.
(module config (lib "configtab.ss" "setup")
  ;; An empty table means that all defaults apply
  )
