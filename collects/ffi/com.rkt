#lang racket/base
(require ffi/unsafe/com)

(provide guid? iid? clsid?
         string->guid string->iid string->clsid
         guid->string
         guid=?

         progid->clsid clsid->progid

         com-create-instance com-get-active-object
         com-object? com-object-eq?
         com-object-clsid com-object-set-clsid!
         com-release
         com-object-type com-type? com-type=?

         com-methods com-method-type com-invoke com-omit
         com-get-properties com-get-property-type com-get-property
         com-get-property*
         com-set-properties com-set-property-type com-set-property!

         com-events com-event-type
         com-register-event-callback
         com-unregister-event-callback
         com-make-event-executor com-event-executor?

         com-object-get-iunknown com-iunknown?
         com-object-get-idispatch com-idispatch?

         type-description? 
         type-describe type-described?
         type-described-value type-described-description)
