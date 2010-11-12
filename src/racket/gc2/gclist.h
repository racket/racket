#ifndef GCLIST_H
#define GCLIST_H

/* design taken from the linux double linked list implementation in include/linux/list.h  */

typedef struct GCList {
  struct GCList *next;
  struct GCList *prev;
} GCList;

#define GCLIST_HEAD(name) GCList name = { &(name), &(name) }

static inline void gclist_init(GCList *list) {
  list->next = list;
  list->prev = list;
}

static inline void __gclist_add(GCList *item, GCList *prev, GCList *next) {
  next->prev = item;
  item->next = next;
  item->prev = prev;
  prev->next = item;
}

static inline void gclist_add(GCList *head, GCList *item) {
  __gclist_add(item, head, head->next);
}

static inline void gclist_add_tail(GCList *head, GCList *item) {
  __gclist_add(item, head->prev, head);
}

static inline void __gclist_del(GCList *prev, GCList *next) {
  next->prev = prev;
  prev->next = next;
}

#define GCLIST_POISON1 ((void *)(0x00100100))
#define GCLIST_POISON2 ((void *)(0x00200200))
static inline void gclist_del(GCList *item) {
  __gclist_del(item->prev, item->next);
  item->next = GCLIST_POISON1;
  item->prev = GCLIST_POISON2;
}

static inline int gclist_is_last(GCList *head, GCList *list) {
  return list->next == head;
}

static inline int gclist_is_empty(GCList *head) {
  return head->next == head;
}

static inline void gclist_move(GCList *list, GCList *head) {
  __gclist_del(list->prev, list->next);
  gclist_add(head, list);
}

static inline void gclist_move_tail(GCList *list, GCList *head) {
  __gclist_del(list->prev, list->next);
  gclist_add(head, list);
}

static inline void __gclist_splice(GCList *item, GCList *prev, GCList *next) {
  abort();
}

static inline void gclist_splice(GCList *head, GCList *list) {
  if(!gclist_is_empty(list)) { __gclist_splice(list, head, head->next); }
}

#define gclist_item(ptr, type, member) \
  ((type) (((void*)(ptr)) - ((void *) (&(((type) 0x0)->member)))))

#define gclist_first_item(head, type, member) \
  gclist_item((head)->next, type, member)

#define gclist_each_item(pos, head, type, member)         \
  for (pos = gclist_item((head)->next, type, member);     \
       &pos->member != (head);                            \
       pos = gclist_item(pos->member.next, type, member))

#define gclist_each_item_safe(pos, n, head, type, member)   \
  for (pos = gclist_item((head)->next, type, member),       \
       n   = gclist_item(pos->member.next, type, member);   \
       &pos->member != (head);                              \
       pos = n,                                             \
       n = gclist_item(n->member.next, type, member))

#endif


/* merge sort */
typedef int (*GCListCmp)(void *priv, GCList *a, GCList *b);
#define MAX_LIST_LENGTH_BITS 20

static GCList *merge(void *priv, GCListCmp cmp, GCList *a, GCList *b) {
  GCList head;
  GCList *tail = &head;

  while(a && b) {
    if ((*cmp)(priv, a, b) <= 0) {
      tail->next = a;
      a = a->next;
    }
    else {
      tail->next = b;
      b = b->next;
    }
    tail = tail->next;
  };
  
  tail->next = a?:b;
  return head.next;
}

static void merge_and_restore_back_links(void *priv, GCListCmp cmp, GCList *head, GCList *a, GCList *b) {
  GCList *tail = head;
  while(a && b) {
    if ((*cmp)(priv, a, b) <= 0) {
      tail->next = a;
      a->prev = tail;
      a = a->next;
    }
    else {
      tail->next = b;
      b->prev = tail;
      b = b->next;
    }
    tail = tail->next;
  }
  
  tail->next = a?:b;

  do {
    tail->next->prev = tail;
    tail = tail->next;
  } while(tail->next);

  tail->next = head;
  head->prev = tail;
}

static void gclist_sort(void *priv, GCList *head, GCListCmp cmp) {
  GCList *part[MAX_LIST_LENGTH_BITS+1];  
  int level; /* index into part[] */
  int max_level = 0;
  GCList *list;

  if (gclist_is_empty(head)) return;

  memset(part, 0, sizeof(part));

  head->prev->next = NULL; /* set end of list NULL */
  list = head->next; /* set list to first item in list */

  while(list) {
    GCList *cur = list;
    list = list->next;
    cur->next = NULL;

    for (level = 0; part[level]; level++) {
      cur = merge(priv, cmp, part[level], cur);
      part[level] = NULL;
    }
    if (level > max_level) {
      if (level > MAX_LIST_LENGTH_BITS) {
        printf("GCList is too long to sort");
        abort();
      }
      max_level = level;
    }
    part[level] = cur;
  }

  for (level = 0; level < max_level; level ++) {
    if (part[level]) {
      list = merge(priv, cmp, part[level], list);
    }
  }
  
  merge_and_restore_back_links(priv, cmp, head, part[max_level], list);
}  
