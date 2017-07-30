#ifndef __lf_ll_h__
#define __lf_ll_h__

/* Include all types exposed by libflipper. */
#include <flipper/types.h>

struct _lf_ll {
	/* The item data. */
	void *item;
	/* A deconstructor for the item, if any. */
	int (* deconstructor)(void *item);
	/* The next item in the list. */
	struct _lf_ll *next;
};

int lf_ll_append(struct _lf_ll **_ll, void *item, void *deconstructor);
void *lf_ll_apply_func(struct _lf_ll *ll, void *_other, void *(* func)(void *_item, void *_other));
int lf_ll_release(struct _lf_ll **_ll);
void *lf_ll_pop(struct _lf_ll **_ll);
size_t lf_ll_count(struct _lf_ll *ll);

#endif
