#lang typed-scheme
(require/typed
 scheme/base
 [opaque WeakBox weak-box?]
 [make-weak-box (Any -> WeakBox)])
