;; The config module doesn't have to use `setup/configtab';
;; it just has to have the right exports. But using
;; `setup/configtab' makes it easier to generate the
;; code at install time.
(module config setup/configtab
  ;; An empty table means that all defaults apply
  )
