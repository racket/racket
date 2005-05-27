/* wx_macevents.h : mac-specific declarations to handle 'leave' events
 */

extern void QueueMrEdEvent(EventRecord *e);
extern void DequeueMrEdEvents(int type, long message);

