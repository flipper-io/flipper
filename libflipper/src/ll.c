/* Primitive implementation of a linked-list data type for use by libflipper and its components. */

/* NOTE: This implementation is unfinished. */

#include <flipper/core.h>

/* Allocates and returns a linked-list. */

struct _lf_ll *lf_ll_init(void) {
	/* Allocate memory for the head of the list. */
	struct _lf_ll *_ll = (struct _lf_ll *)calloc(1, sizeof(struct _lf_ll));
	/* Check if the linked-list was allocated successfully. */
	if (!_ll) {
		return NULL;
	}
	return _ll;
}

/* Appends an element to an existing linked-list. */

int lf_ll_append(struct _lf_ll *_ll, void *_item) {
	/* Check that the linked-list is non-null. */
	if (!_ll) {
		return lf_error;
	}
	/* Walk the linked-list until the tail is found. */
	while (_ll -> _item) {
		_ll = _ll -> _next;
	}
	/* Insert the item into the linked-list. */
	_ll -> _item = _item;
	return lf_success;
}

/* Use a higher-order function to find a given item in the list. */

void *lf_ll_match(struct _lf_ll *_ll, _ll_match_routine routine) {
	/* Check that the linked-list is non-null. */
	if (!_ll) {
		return NULL;
	}
	/* Walk the linked-list until the match routine returns. */
	while (_ll -> _next) {
		if (routine(_ll -> _item)) {
			return _ll -> _item;
		}
		_ll = _ll -> _next;
	}
	return NULL;
}

/* Deallocates an existing linked-list. */

int lf_ll_destroy(struct _lf_ll *_ll) {
	/* Check that the linked-list is non-null. */
	if (!_ll) {
		return lf_error;
	}
	return lf_success;
}
