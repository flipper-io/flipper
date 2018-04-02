#include <flipper/posix/network.h>
#include <flipper/error.h>
#include <flipper.h>

int lf_network_configure(struct _lf_device *device, void *_ctx) {
	return lf_success;
}

bool lf_network_ready(struct _lf_device *device) {
	return false;
}

int lf_network_push(struct _lf_device *device, void *source, lf_size_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'", __PRETTY_FUNCTION__);
	lf_assert(device, failure, E_NULL, "No endpoint for to device '%s'.", device->name);

	struct _lf_endpoint *endpoint = device->endpoint;
	struct _lf_network_context *context = (struct _lf_network_context *)endpoint->_ctx;
	ssize_t e = sendto(context->fd, source, length, 0, (struct sockaddr *)&context->device, sizeof(struct sockaddr_in));
	lf_assert(e > 0, failure, E_COMMUNICATION, "Failed to send data to networked device '%s' at '%s'.", context->host, inet_ntoa(context->device.sin_addr));
	return lf_success;

failure:
	return lf_error;
}

int lf_network_pull(struct _lf_device *device, void *destination, lf_size_t length) {
	lf_assert(device, failure, E_NULL, "No device provided to '%s'", __PRETTY_FUNCTION__);
	lf_assert(device, failure, E_NULL, "No endpoint for to device '%s'.", device->name);

	struct _lf_endpoint *endpoint = device->endpoint;
	struct _lf_network_context *context = (struct _lf_network_context *)endpoint->_ctx;
	socklen_t _length = sizeof(context->device);
	ssize_t e = recvfrom(context->fd, destination, length, 0, (struct sockaddr *)&context->device, &_length);
	lf_assert(e > 0, failure, E_COMMUNICATION, "Failed to receive data from networked device '%s' at '%s'.", context->host, inet_ntoa(context->device.sin_addr));
	return lf_success;

failure:
	return lf_error;
}

int lf_network_destroy(struct _lf_endpoint *endpoint) {
	if (endpoint && endpoint->_ctx) {
		struct _lf_network_context *context = endpoint->_ctx;
		close(context->fd);
	}
	return lf_success;
}

struct _lf_endpoint *lf_network_endpoint_for_hostname(char *hostname) {
	struct _lf_network_context *context = NULL;
	struct _lf_endpoint *endpoint = lf_endpoint_create(lf_network_configure,
													   lf_network_ready,
													   lf_network_push,
													   lf_network_pull,
													   lf_network_destroy,
													   sizeof(struct _lf_network_context));
	lf_assert(endpoint, failure, E_ENDPOINT, "Failed to create endpoint for networked device.");
	context = (struct _lf_network_context *)endpoint->_ctx;
	context->fd = socket(AF_INET, SOCK_DGRAM, 0);
	lf_assert(context->fd > 0, failure, E_SOCKET, "Failed to create socket for network device.");
	struct hostent *host = gethostbyname(hostname);
	lf_assert(host, failure, E_COMMUNICATION, "Failed to find device with hostname '%s' on the network.", hostname);
	strncpy(context->host, host->h_name, sizeof(context->host));
	struct in_addr **list = (struct in_addr **) host->h_addr_list;
	memset(&(context->device), 0, sizeof(struct sockaddr_in));
	context->device.sin_family = AF_INET;
	context->device.sin_addr.s_addr = list[0]->s_addr;
	context->device.sin_port = htons(LF_UDP_PORT);
	return endpoint;
failure:
	if (context) close(context->fd);
	return NULL;
}
