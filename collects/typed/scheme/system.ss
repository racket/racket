#lang typed-scheme

(require typed/private/utils)

(require/typed/provide
 scheme/system
 [system (String -> Boolean)]
 [system* (Path-String String * -> Boolean)]
 [system/exit-code (String -> Integer)]
 [system*/exit-code (Path-String String * -> Integer)])