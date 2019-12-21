#include "libflipper.h"
#include <stdarg.h>
#include "network.h"
#include <unistd.h>

int server_fd;

void lf_connect_debug_server(void)
{

    int e;

    int port = 9872;

    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) printf("Could not create socket\n");

    struct sockaddr_in server;

    server.sin_family = AF_INET;
    server.sin_port = htons(port);
    server.sin_addr.s_addr = htonl(INADDR_ANY);

    int opt_val = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt_val, sizeof opt_val);

    e = connect(server_fd, (struct sockaddr *) &server, sizeof(server));
    if (e < 0) printf("Could not bind socket\n");

}

void _lf_debug(const char *fmt, ...) {

    int written;
    char buf[1024];
    va_list args;
    va_start(args, fmt);

    written = vsprintf(buf, fmt, args);
    write(server_fd, buf, written);

    va_end(args);
}

void _lf_assert(lf_err_t err, const char *func, int line, const char *fmt, ...) {
    _lf_err = err;

    int written;
    char buf[1024];
    va_list args;
    va_start(args, fmt);

    written = sprintf(buf, KRED "flipper runtime error" KNRM ": " KBLU "%s" KNRM " (0x%02x)\n" KGRN "  %s:%i: " KYEL, lf_error_string(err), err, func, line);
    write(server_fd, buf, written);

    written = vsprintf(buf, fmt, args);
    write(server_fd, buf, written);

    written = sprintf(buf, KNRM "\n\n");
    write(server_fd, buf, written);

    va_end(args);
}
