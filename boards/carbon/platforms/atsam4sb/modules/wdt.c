#define __private_include__
#include <flipper/wdt.h>
#include <platforms/atsam4s16b.h>

int wdt_configure(void) {
	return lf_success;
}

void wdt_fire(void) {

}
