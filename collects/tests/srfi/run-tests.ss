(require (planet "test.ss" ("schematics" "schemeunit.plt" 1 1)))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 1 1)))
(require "all-srfi-tests.ss")

(test/text-ui all-srfi-tests)
