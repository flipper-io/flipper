#include <flipper.h>

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

void *lf_ll_item(struct _lf_ll *ll, uint32_t index) {
	while (index-- && ll) ll = ll->next;
	lf_assert(ll, failure, E_NULL, "End of list reached before item.");
	return ll->item;
failure:
	return NULL;
}

void lf_ll_free(struct _lf_ll **_ll) {
	struct _lf_ll *ll = *_ll;
	if (ll->deconstructor) ll->deconstructor(ll->item);
    *_ll = (*_ll)->next;
	free(ll);
}

void lf_ll_remove(struct _lf_ll **_ll, void *item) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s.'", __PRETTY_FUNCTION__);
    while (*_ll) {
        if ((*_ll)->item == item) {
            lf_ll_free(_ll);
        } else {
            _ll = &(*_ll)->next;
        }
	}
failure:
	return;
}

int lf_ll_apply_func(struct _lf_ll *ll, lf_ll_applier_func func,  void *_ctx) {
	lf_assert(ll, failure, E_NULL, "Invalid list provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(func, failure, E_NULL, "Invalid applier function provided to '%s'.", __PRETTY_FUNCTION__);
	int e = lf_success;
	do {
		e = func(ll->item, _ctx);
		if (e != lf_success) break;
	} while ((ll = ll->next));
	return e;
failure:
	return lf_error;
}

/* Releases the entire linked list. */
int lf_ll_release(struct _lf_ll **_ll) {
	lf_assert(_ll, failure, E_NULL, "Invalid list reference provided to '%s'.", __PRETTY_FUNCTION__);
	while (*_ll) lf_ll_free(_ll);
	return lf_success;
failure:
	return lf_error;
}
