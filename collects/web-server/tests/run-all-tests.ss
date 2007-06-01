(module run-all-tests mzscheme
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           "all-web-server-tests.ss")
  
  (test/graphical-ui all-web-server-tests))