#include "libflipper.h"
#include "network.h"
#include <unistd.h>

int lf_network_read(struct _lf_device *device, void *dst, uint32_t length) {
    lf_assert(device, E_NULL, "invalid device");
    lf_assert(device, E_NULL, "No endpoint for to device '%s'.", device->name);

    struct _lf_network_context *context = (struct _lf_network_context *)device->_ep_ctx;
    lf_assert(context, E_NULL, "invalid context");
    socklen_t _length = sizeof(context->device);
    ssize_t e = recvfrom(context->fd, dst, length, 0, (struct sockaddr *)&context->device, &_length);
    lf_assert(e > 0, E_COMMUNICATION, "Failed to receive data from networked device '%s' at '%s'.", context->host, inet_ntoa(context->device.sin_addr));
    return lf_success;

fail:
    return lf_error;
}

int lf_network_write(struct _lf_device *device, void *src, uint32_t length) {
    lf_assert(device, E_NULL, "invalid device");
    lf_assert(device, E_NULL, "No endpoint for to device '%s'.", device->name);

    struct _lf_network_context *context = (struct _lf_network_context *)device->_ep_ctx;
    lf_assert(context, E_NULL, "invalid context");
    ssize_t e = sendto(context->fd, src, length, 0, (struct sockaddr *)&context->device, sizeof(struct sockaddr_in));
    lf_assert(e > 0, E_COMMUNICATION, "Failed to send data to networked device '%s' at '%s'.", context->host, inet_ntoa(context->device.sin_addr));
    return lf_success;

fail:
    return lf_error;
}

int lf_network_release(struct _lf_device *device) {
    lf_assert(device, E_NULL, "invalid device");

    struct _lf_network_context *context = device->_ep_ctx;
    lf_assert(context, E_NULL, "invalid context");
    close(context->fd);
    return lf_success;

fail:
    return lf_error;
}

struct _lf_device *lf_network_device_for_hostname(char *hostname) {
    struct _lf_network_context *context = NULL;
    struct _lf_device *device = lf_device_create(lf_network_read, lf_network_write, lf_network_release);
    lf_assert(device, E_ENDPOINT, "Failed to create device");
    device->_ep_ctx = calloc(1, sizeof(struct _lf_network_context));
    context = (struct _lf_network_context *)device->_ep_ctx;
    lf_assert(context, E_NULL, "Failed to allocate memory for context");
    context->fd = socket(AF_INET, SOCK_DGRAM, 0);
    lf_assert(context->fd > 0, E_SOCKET, "Failed to create socket for network device.");
    struct hostent *host = gethostbyname(hostname);
    lf_assert(host, E_COMMUNICATION, "Failed to find device with hostname '%s' on the network.", hostname);
    strncpy(context->host, host->h_name, sizeof(context->host));
    struct in_addr **list = (struct in_addr **) host->h_addr_list;
    memset(&(context->device), 0, sizeof(struct sockaddr_in));
    context->device.sin_family = AF_INET;
    context->device.sin_addr.s_addr = list[0]->s_addr;
    context->device.sin_port = htons(LF_UDP_PORT);
    return device;
fail:
    if (context) close(context->fd);
    return NULL;
}
