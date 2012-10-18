
Allocation rules:

 * Use `as-objc-allocation' when creating a Cocoa object. When the
   resulting reference becomes unreachable, the Cocoa object will be
   released.

 * Use `with-autorelease' in atomic mode around calls that autorelease
   and where the release should take effect immediate. Do not create
   an autorelease pool except in atomic mode.

 * Other autoreleased objects may end up in the root pool installed by
   "pool.rkt". The root pool is periodically destroyed and replaced;
   call `queue-autorelease-flush' if you need to encurage replacement
   of the pool. If you need to use an object that might be autoflushed,
   be sure that you're in atomic mode.
