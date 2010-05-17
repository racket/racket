#lang typed/scheme
(require typed/rackunit
         typed/private/utils)

(require/typed/provide
 rackunit/gui
 [test/gui
  (Test * -> Any)]
 [make-gui-runner
  (-> (Test * -> Any))])
