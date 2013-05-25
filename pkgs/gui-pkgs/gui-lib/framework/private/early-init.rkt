#lang racket/unit
(require "sig.rkt"
         "../preferences.rkt")
(import [prefix preferences: framework:preferences^])
(export framework:early-init^)
(init-depend framework:preferences^)

(preferences:low-level-get-preference preferences:get-preference/gui)
(preferences:low-level-put-preferences preferences:put-preferences/gui)
