#define __private_include__
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <strings.h>

#include <flipper/error.h>
#include <flipper/platform/network.h>

const struct _bus network = {
	network_configure,
	network_enable,
	network_disable,
	network_ready,
	network_put,
	network_get,
	network_push,
	network_pull
};

int network_socket;

struct sockaddr_in network_address;

void network_configure(void *ip) {

	/* ~ Open a socket on the current network. ~ */
	network_socket = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

	if (network_socket < 0) {
		error_raise(E_OPEN_SOCK, "");
	}

	/* ~ Clear the network address. ~ */
	bzero(&network_address, sizeof(network_address));
	network_address.sin_family = AF_INET;
	network_address.sin_addr.s_addr = inet_addr(ip);
	network_address.sin_port = htons(FLIPPER_NETWORK_PORT);

	if (connect(network_socket, (struct sockaddr *) &network_address, sizeof(network_address)) < 0)
		error_raise(E_CONN_SOCK, "");

	// Don't think this should be here...
	//verbose("Successfully connected to 'Elroy' at '%s'.\n\n", ip);

}

void network_enable(void) {

}

void network_disable(void) {

}

uint8_t network_ready(void) {
	return 0;
}

void network_put(uint8_t byte) {

}

uint8_t network_get(void) {
	return 0;
}

void network_push(void *source, size_t length) {

	if (network_socket < 0) {
		error_raise(E_FLIPPER_UNBOUND, "");
	}
	send(network_socket, source, length, 0);

}

void network_pull(void *destination, size_t length) {

	recv(network_socket, destination, length, 0);

}
