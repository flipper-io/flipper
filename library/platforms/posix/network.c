#define __private_include__
#include <flipper/platforms/posix/network.h>
#include <flipper/error.h>
#include <flipper/libflipper.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <netdb.h>

struct _lf_endpoint lf_network_ep = {
	network_configure,
	network_ready,
	network_put,
	network_get,
	network_push,
	network_pull,
	network_destroy
};

struct _network_record {
	int fd;
	struct sockaddr_in device;
};

int network_configure(struct _lf_endpoint *this, char *hostname) {
	/* Allocate memory for the network record if it has not yet been allocated. */
	if (!(this -> record)) {
		this -> record = calloc(1, sizeof(struct _network_record));
	}
	/* Obtain a pointer to and cast to the network record associated with the provided endpoint. */
	struct _network_record *record = this -> record;
	/* Create a new socket. */
	record -> fd = socket(AF_INET, SOCK_DGRAM, 0);
	if (record -> fd < 0) {
		lf_error_raise(E_SOCKET, error_message("Failed to open a new message runtime socket."));
		return lf_error;
	}
	struct hostent *host = gethostbyname(hostname);
	if (!host) {
		lf_error_raise(E_NO_DEVICE, error_message("Failed to resolve hostname '%s' to device IP.", hostname));
		return lf_error;
	}
	struct in_addr **list = (struct in_addr **) host -> h_addr_list;
	/* Create the addressing record. */
	memset(&(record -> device), 0, sizeof(struct sockaddr_in));
	record -> device.sin_family = AF_INET;
	record -> device.sin_addr.s_addr = list[0] -> s_addr;
	record -> device.sin_port = htons(FMR_PORT);
	// /* Bind to the network socket. */
	// if (bind(record -> fd, (struct sockaddr *)&(record -> device), sizeof(struct sockaddr_in)) < 0) {
	// 	/* Close the opened socket. */
	// 	close(record -> fd);
	// 	lf_error_raise(E_SOCKET, error_message("Failed to bind to the message runtime socket."));
	// 	return lf_error;
	// }
	return lf_success;
}

uint8_t network_ready(struct _lf_endpoint *this) {
	return true;
}

void network_put(struct _lf_endpoint *this, uint8_t byte) {
	return;
}

uint8_t network_get(struct _lf_endpoint *this) {
	return 0;
}

int network_push(struct _lf_endpoint *this, void *source, lf_size_t length) {
	/* Obtain a pointer to and cast to the network record associated with the active endpoint. */
	struct _network_record *record = this -> record;
	char derp[length];
	memcpy(derp, source, length);
	printf("'%s' to %s\n", derp, inet_ntoa(record -> device.sin_addr));
	socklen_t len = sizeof(struct sockaddr_in);
	ssize_t _e = sendto(record -> fd, source, length, 0, (struct sockaddr *)&(record -> device), len);
	if (_e < 0) {
		lf_error_raise(E_COMMUNICATION, error_message("Failed to transfer data to networked device."));
		return lf_error;
	}
	return lf_success;
}

int network_pull(struct _lf_endpoint *this, void *destination, lf_size_t length) {
	// /* Obtain a pointer to and cast to the network record associated with the active endpoint. */
	// struct _network_record *record = this -> record;
	// socklen_t _length;
	// ssize_t _e = recvfrom(record -> fd, destination, length, 0, (struct sockaddr *)&(record -> device), &_length);
	// if (_e < 0) {
	// 	lf_error_raise(E_COMMUNICATION, error_message("Failed to transfer data to networked device."));
	// 	return lf_error;
	// }
	return lf_success;
}

int network_destroy(struct _lf_endpoint *this) {
	/* Obtain a pointer to and cast to the network record associated with the provided endpoint. */
	struct _network_record *record = this -> record;
	/* If a file descriptor has been opened for the associated socket, close it. */
	if (record) {
		close(record -> fd);
		free(record);
	}
	return lf_success;
}
