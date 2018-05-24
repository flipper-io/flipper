/* libflipper linked-list test */

#include <flipper.h>
#include <tests.h>

#define ITEM (void *)0xabcd

int ll_test(void) {

    /* Try to create a valid list. */
    struct _lf_ll *ll = NULL;
    lf_try(lf_ll_append(&ll, ITEM, NULL));
    lf_expect_success();
    lf_assert(ll, fail, E_NULL, "Linked list was not created.");

    /* Expect 1 item. */
    int count = 0;
    lf_try(count = lf_ll_count(ll));
    lf_expect_success();
    lf_assert(count == 1, fail, E_UNIMPLEMENTED, "Expected 1 item in list.");

    /* Try to access a valid item. */
    void *item = NULL;
    lf_try(item = lf_ll_item(ll, 0));
    lf_expect_success();
    lf_assert(item == ITEM, fail, E_UNIMPLEMENTED, "Item did not match.");

    /* Try to access an invalid item. */
    lf_try(item = lf_ll_item(ll, 1));
    lf_expect_error();
    lf_assert(item == NULL, fail, E_UNIMPLEMENTED, "Invalid item was not NULL.");

    /* Free that item. */
    lf_try(lf_ll_remove(&ll, ITEM));
    lf_expect_success();
    lf_assert(ll == NULL, fail, E_UNIMPLEMENTED, "Expected ll to be NULL.");

    /* Count should be 0. */
    lf_try(count = lf_ll_count(ll));
    lf_expect_success();
    lf_assert(count == 0, fail, E_UNIMPLEMENTED, "Expected 0 items in list.");

    return lf_success;
fail:
    return lf_error;
}
