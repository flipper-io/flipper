#include <flipper/flipper.h>
#include <libusb.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <flipper/posix/network.h>
#include <pthread.h>

struct libusb_device_handle *usb_handle = NULL;

#define BUFFER_SIZE 1024

int server_fd, client_fd, err;
struct sockaddr_in server, client;

static volatile int alive = 1;

void sigint(int signal) {
    alive = 0;
}

// Who has stdout
pthread_mutex_t lock;

void *usb_debug_thread(void *vargp)
{

    int e = 0;
    int read;
    uint8_t buf[DEBUG_IN_SIZE];

    int timeout = 0;

    while (1) {

        e = libusb_interrupt_transfer(usb_handle, DEBUG_IN_ENDPOINT, buf, DEBUG_IN_SIZE, &read, timeout);

        if (timeout == 0)
        {
            pthread_mutex_lock(&lock);
            printf("\r\n--- DEVICE ---\r\n\r\n");
        }

        if (e == LIBUSB_ERROR_TIMEOUT || read == 0)
        {
            timeout = 0;
            pthread_mutex_unlock(&lock);
        }
        else
        {
            timeout = 5;
            char *bufp = (char *)buf;
            while (read--) printf("%c", *bufp++);
            fflush(stdout);
        }

    }

    return NULL;
}

void *network_debug_thread(void *vargp) {

    char buf[BUFFER_SIZE];
    int timeout = 0;

    socklen_t client_len = sizeof(client);
    client_fd = accept(server_fd, (struct sockaddr *) &client, &client_len);

    if (client_fd < 0) printf("Could not establish new connection\n");

    while (1) {

        int read = recv(client_fd, buf, BUFFER_SIZE, timeout);

        if (timeout == 0)
        {
            pthread_mutex_lock(&lock);
            printf("\r\n--- HOST ---\r\n\r\n");
        }

        if (read < 0 || read == 0) {
            timeout = 0;
            pthread_mutex_unlock(&lock);
            continue;
        }
        else
        {
            timeout = 5;
            char *bufp = buf;
            while (read--) printf("%c", *bufp++);
            fflush(stdout);
        }

    }

    return NULL;
}

int main(int argc, char *argv[]) {

    struct libusb_context *usb_context = NULL;

    int e;

    /* capture interrupt signal */
    signal(SIGKILL, sigint);

    e = libusb_init(&usb_context);
    lf_assert(e == 0, E_LIBUSB, "Failed to initialize libusb.");

    usb_handle = libusb_open_device_with_vid_pid(usb_context, FLIPPER_USB_VENDOR_ID, USB_PRODUCT_ID);
    lf_assert(usb_handle, E_LIBUSB, "Failed to find a device.");

    /* Configure USB device. */
    e = libusb_claim_interface(usb_handle, DEBUG_INTERFACE);
    lf_assert(e == 0, E_LIBUSB, "Failed to claim the debug interface.");

    int port = 9872;

    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) printf("Could not create socket\n");

    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = htonl(INADDR_ANY);

    int opt_val = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_val, sizeof opt_val);

    e = bind(server_fd, (struct sockaddr *) &server, sizeof(server));
    if (e < 0) printf("Could not bind socket\n");

    e = listen(server_fd, 128);
    if (e < 0) printf("Could not listen on socket\n");

    if (pthread_mutex_init(&lock, NULL) != 0)
    {
        printf("\n mutex init has failed\n");
        return 1;
    }

    pthread_t usb_thread_id;
    pthread_create(&usb_thread_id, NULL, usb_debug_thread, NULL);

    pthread_t network_thread_id;
    pthread_create(&network_thread_id, NULL, network_debug_thread, NULL);

    pthread_join(network_thread_id, NULL);
    pthread_join(usb_thread_id, NULL);

    pthread_mutex_destroy(&lock);

fail:

    libusb_close(usb_handle);
    libusb_exit(usb_context);

    return 0;
}
