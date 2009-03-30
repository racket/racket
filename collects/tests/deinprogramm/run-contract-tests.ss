#lang scheme/base

(require (planet schematics/schemeunit:3/text-ui))
(require tests/deinprogramm/contract)

(run-tests all-contract-tests)