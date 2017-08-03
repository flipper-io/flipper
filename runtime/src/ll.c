#include <flipper/libflipper.h>

/* Appends an item to the linked list. */
int lf_ll_append(struct _lf_ll **_ll, void *item, void *deconstructor) {
	lf_assert(_ll, failure, E_NULL, "NULL");
	struct _lf_ll *new = malloc(sizeof(struct _lf_ll));
	lf_assert(new, failure, E_NULL, "NULL");
	memset(new, 0, sizeof(struct _lf_ll));
	new -> item = item;
	new -> deconstructor = deconstructor;
	struct _lf_ll *head = *_ll;
	if (head) {
		while (head -> next) {
			head = head -> next;
		}
		head -> next = new;
	} else {
		*_ll = new;
	}
	return lf_success;
failure:
	return lf_error;
}

void *lf_ll_apply_func(struct _lf_ll *ll, void *(* func)(void *_item, void *_other),  void *_other) {
	lf_assert(ll, failure, E_NULL, "NULL");
	do {
		void *result = NULL;
		/* Apply the function to the current item in the list. */
		result = func(ll -> item, _other);
		/* If the function returns something, return it here. */
		if (result) {
			return result;
		}
	} while ((ll = ll -> next));
failure:
	return NULL;
}

/* Releases the entire linked list. */
int lf_ll_release(struct _lf_ll **_ll) {
	lf_assert(_ll, failure, E_NULL, "NULL");
	struct _lf_ll *ll = *_ll;
	if (!ll) return lf_success;
	do {
		/* Call the item's deconstructor. */
		if (ll -> deconstructor) {
			ll -> deconstructor(ll -> item);
		}
		struct _lf_ll *old = ll;
		ll = ll -> next;
		free(old);
	} while (ll);
	*_ll = NULL;
	return lf_success;
failure:
	return lf_error;
}

void *lf_ll_pop(struct _lf_ll **_ll) {
	lf_assert(_ll, failure, E_NULL, "NULL");
	struct _lf_ll *ll = *_ll;
	lf_assert(ll, failure, E_NULL, "NULL");
	*_ll = ll -> next;
	void *item = ll -> item;
	free(ll);
	return item;
failure:
	return NULL;
}

size_t lf_ll_count(struct _lf_ll *ll) {
	if (!ll) return 0;
	size_t count = 0;
	while (ll) {
		ll = ll->next;
		count ++;
	}
	return count;
}
