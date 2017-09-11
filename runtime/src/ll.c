#include <flipper/libflipper.h>

size_t lf_ll_count(struct _lf_ll *ll) {
	size_t count = 0;
	while (ll) {
		ll = ll->next;
		count ++;
	}
	return count;
}

int lf_ll_append(struct _lf_ll **_ll, void *item, void *deconstructor) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s'.", __PRETTY_FUNCTION__);
	struct _lf_ll *new = malloc(sizeof(struct _lf_ll));
	memset(new, 0, sizeof(struct _lf_ll));
	new->item = item;
	new->deconstructor = deconstructor;
	struct _lf_ll *head = *_ll;
	if (head) {
		while (head->next) head = head->next;
		head->next = new;
	} else {
		*_ll = new;
	}
	return lf_success;
failure:
	return lf_error;
}

void *lf_ll_pop(struct _lf_ll **_ll) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s.'", __PRETTY_FUNCTION__);
	void *item = NULL;
	if (*_ll) {
		item = (*_ll)->item;
		struct _lf_ll *old = *_ll;
		*_ll = (*_ll)->next;
		free(old);
	}
	return item;
failure:
	return NULL;
}

void lf_ll_remove(struct _lf_ll **_ll, void *item) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s.'", __PRETTY_FUNCTION__);
	while (*_ll) {
		if ((*_ll)->item == item) *_ll = (*_ll)->next;
	}
failure:
	return;
}

void lf_ll_apply_func(struct _lf_ll *ll, lf_ll_applier_func func,  void *_ctx) {
	lf_assert(ll && func, failure, E_NULL, "Invalid parameter provided to '%s'.", __PRETTY_FUNCTION__);
	do {
		func(ll->item, _ctx);
	} while ((ll = ll->next));
failure:
	return;
}

/* Releases the entire linked list. */
int lf_ll_release(struct _lf_ll **_ll) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s'.", __PRETTY_FUNCTION__);
	struct _lf_ll *ll = *_ll;
	while (ll) {
		/* Call the item's deconstructor. */
		if (ll->deconstructor) ll->deconstructor(ll->item);
		struct _lf_ll *old = ll;
		ll = ll->next;
		free(old);
	};
	*_ll = NULL;
	return lf_success;
failure:
	return lf_error;
}
