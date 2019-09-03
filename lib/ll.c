#include "libflipper.h"

size_t lf_ll_count(struct _lf_ll *ll) {
    size_t count = 0;
    while (ll) {
        ll = ll->next;
        count++;
    }
    return count;
}

int lf_ll_append(struct _lf_ll **_ll, void *item, void (*deconstructor)(void *)) {
    lf_assert(_ll, E_NULL, "invalid list reference");

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
fail:
    return lf_error;
}

int lf_ll_concat(struct _lf_ll **_lla, struct _lf_ll *_llb) {
    lf_assert(_lla, E_NULL, "invalid list reference");

    struct _lf_ll *head = *_lla;
    if (head) {
        while (head->next) head = head->next;
        head->next = _llb;
    } else {
        *_lla = _llb;
    }

    return lf_success;
fail:
    return lf_error;
}

void *lf_ll_item(struct _lf_ll *ll, size_t index) {
    while (index-- && ll) ll = ll->next;
    lf_assert(ll, E_NULL, "End of list reached before item.");

    return ll->item;
fail:
    return NULL;
}

void lf_ll_free(struct _lf_ll **_ll) {
    struct _lf_ll *ll = *_ll;
    if (ll->deconstructor) ll->deconstructor(ll->item);
    *_ll = (*_ll)->next;
    free(ll);
}

void lf_ll_remove(struct _lf_ll **_ll, void *item) {
    lf_assert(_ll, E_NULL, "invalid list reference");

    while (*_ll) {
        if ((*_ll)->item == item) {
            lf_ll_free(_ll);
        } else {
            _ll = &(*_ll)->next;
        }
    }

fail:
    return;
}

int lf_ll_apply_func(struct _lf_ll *ll, lf_ll_applier_func func, void *_ctx) {
    lf_assert(ll, E_NULL, "invalid list");
    lf_assert(func, E_NULL, "invalid applier function");

    int e = lf_success;
    do {
        e = func(ll->item, _ctx);
        if (e != lf_success) break;
    } while ((ll = ll->next));

    return e;
fail:
    return lf_error;
}

/* Releases the entire linked list. */
int lf_ll_release(struct _lf_ll **_ll) {
    lf_assert(_ll, E_NULL, "invalid list reference");
    while (*_ll) lf_ll_free(_ll);

    return lf_success;
fail:
    return lf_error;
}
