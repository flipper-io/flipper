#ifndef __lf_ll_h__
#define __lf_ll_h__

/* Include all types exposed by libflipper. */
#include <flipper/types.h>

/* Generic linked list data structure used throughout libflipper */
struct _lf_ll {
	/* The item stored at this node in the list. */
	void *item;
	/* A deconstructor for the item, if any. */
	int (* deconstructor)(void *item);
	/* The next node in the list. */
	struct _lf_ll *next;
};

/* The type signature of a fast enumeration function. */
typedef int (* lf_ll_applier_func)(const void *_item, void *_ctx);

/* Returns the number of items in the list. */
size_t lf_ll_count(struct _lf_ll *ll);
/* Appends the item to the list. Creates if necessary. */
int lf_ll_append(struct _lf_ll **_ll, void *item, void *deconstructor);
/* Retrieves an item from the list at the given index. */
void *lf_ll_item(struct _lf_ll *ll, uint32_t index);
/* Removes matching items from the list. */
void lf_ll_remove(struct _lf_ll **_ll, void *item);
/* Applys a fast enumeration function to each item in the list. */
int lf_ll_apply_func(struct _lf_ll *ll, lf_ll_applier_func func, void *_ctx);
/* Releases the list and all of its items. */
int lf_ll_release(struct _lf_ll **_ll);

#endif
