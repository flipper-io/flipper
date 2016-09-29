#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>

#define v(i) atoi(argv[i])

int main(int argc, char *argv[]) {

	//flipper_attach_endpoint("fvm", &lf_fvm_ep);
	flipper_attach();
	led_set_rgb(0, 0, 0);
	uart_put('#');
	char ack[3];
	uart_pull(ack, sizeof(ack));
	if (!memcmp(ack, (char []){ 0x0a, 0x0d, 0x3e }, sizeof(ack))) {
		fprintf(stderr, "Successfully entered update mode.\n");
	} else {
		fprintf(stderr, KRED "Failed to enter update mode.\n");
	}

    return EXIT_SUCCESS;
}
