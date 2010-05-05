#lang typed/scheme
(require typed/racunit
         typed/private/utils)

(require/typed/provide
 racunit/gui
 [test/gui
  (Test * -> Any)]
 [make-gui-runner
  (-> (Test * -> Any))])
