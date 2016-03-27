#define __private_include__
#include <flipper/fdl/fdl.h>
#include <flipper/fmr/fmr.h>

void fdl_configure(void) {

}

void *fdl_load(uint16_t key) {

	return (void *)(uint64_t)(host.invoke(_fdl, _fdl_load, 2, little(key), 0));

}

void fdl_launch(uint16_t key) {

	host.invoke(_fdl, _fdl_launch, 2, little(key), 0);

}

void fdl_resolve(uint16_t key, const void *address) {

}
