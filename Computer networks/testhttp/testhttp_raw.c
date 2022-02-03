#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <ctype.h>
#include "err.h"

#define BUFFLEN 16384
#define STATUS_OK "200"
#define HTTP_VER_STR "HTTP/1.1 "
#define MIN(a, b) (((a) < (b)) ? (a) : (b))

int sock;
char *executable, *connection_addr, *connection_port, *host_addr, *host_path, *cookies;
size_t host_addr_len = 0, content_len = 0;
int chunked_field_flag = 0;
char buffer[BUFFLEN];


void raise_err_usage() {
    fatal("Usage: %s <host address>:<port> <cookie file> <http test address>", executable);
}

void parse_connection_address(char *arg) {
    char *str = arg;
    connection_addr = strsep(&str, ":");
    connection_port = str;
    if (!connection_addr || !strlen(connection_addr) || !connection_port || !strlen(connection_port))
        raise_err_usage();
}

void parse_host(char *arg) {
    char *str;
    if ((str = strstr(arg, "http://")) && str == arg)
        str += 7;
    else if ((str = strstr(arg, "https://")) && str == arg)
        str += 8;
    else
        raise_err_usage();

    host_addr = strsep(&str, "#");
    if ((host_path = strchr(host_addr, '/'))) {
        host_addr_len = (size_t) (host_path - host_addr);
        host_path += 1;
    } else if ((host_path = strchr(host_addr, '?')) || (host_path = strchr(host_addr, '&'))) {
        host_addr_len = (size_t) (host_path - host_addr);
    } else {
        host_addr_len = strlen(host_addr);
    }
}

void connect_socket() {
    struct addrinfo addr_hints, *addr_result;

    memset(&addr_hints, 0, sizeof(struct addrinfo));
    addr_hints.ai_family = AF_INET; // IPv4
    addr_hints.ai_socktype = SOCK_STREAM;
    addr_hints.ai_protocol = IPPROTO_TCP;

    int err = getaddrinfo(connection_addr, connection_port, &addr_hints, &addr_result);
    if (err == EAI_SYSTEM) // system error
        syserr("getaddrinfo: %s", gai_strerror(err));
    else if (err != 0) // other error (host not found, etc.)
        fatal("getaddrinfo: %s", gai_strerror(err));

    // initialize socket according to getaddrinfo results
    sock = socket(addr_result->ai_family, addr_result->ai_socktype, addr_result->ai_protocol);
    if (sock < 0)
        syserr("socket");

    // connect socket to the server
    if (connect(sock, addr_result->ai_addr, addr_result->ai_addrlen) < 0)
        syserr("connect");

    freeaddrinfo(addr_result);
}

void write_to_host(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    size_t len = vsnprintf(0, 0, fmt, args) + 1; // count how much memory to allocate
    va_end(args);

    char buff[len];
    va_start(args, fmt);
    vsnprintf(buff, len, fmt, args);
    va_end(args);

    if (write(sock, buff, len - 1) != len - 1)
        syserr("partial / failed write");
}

// replace trailing CRLF with null byte
char *remove_trailing_crlf(char *str) {
    size_t len = strlen(str);
    if (len > 1 && str[len - 2] == '\r')
        str[len - 2] = '\0';
    else if (len && str[len - 1] == '\n')
        str[len - 1] = '\0';

    return str;
}

void append_cookie(char *line, size_t *total_len) {
    line = remove_trailing_crlf(line);
    if (!cookies) {
        cookies = calloc(strlen(line) + 1, sizeof(char));
        memcpy(cookies, line, strlen(line));
    } else {
        cookies = realloc(cookies, *total_len + strlen(line) + 2);
        strcat(cookies, "; ");
        strcat(cookies, line);
    }
    *total_len += strlen(line) + 2;
}

int parse_cookies_from_file(char *path) {
    char *line = NULL;
    size_t len = 0, total_len = 0;

    FILE *in_cookies = fopen(path, "rt");
    if (!in_cookies)
        syserr("fopen");

    cookies = NULL;

    while (getline(&line, &len, in_cookies) > 0) {
        append_cookie(line, &total_len);
    }

    if (line) free(line);
    fclose(in_cookies);

    return (!cookies);
}

int parse_status_line(FILE *stream) {
    char *line = NULL;
    size_t len = 0;

    if (getline(&line, &len, stream) < 0)
        syserr("No status header received.");

    int status_not_ok = !strstr(line, STATUS_OK);
    if (status_not_ok) {
        printf("%s\n", remove_trailing_crlf(line + (strstr(line, HTTP_VER_STR) ? strlen(HTTP_VER_STR) : 0)));
    }

    free(line);
    return status_not_ok;
}

// perform tolower() on all characters in the array
char *str_to_lower(char *str) {
    for (char *c = str; *c; ++c)
        *c = tolower(*c);

    return str;
}

char *remove_leading_whitespace(char *str) {
    while (*str && isspace(*str))
        str++;
    return str;
}

// replace trailing whitespace with a null byte
char *remove_trailing_whitespace(char *str) {
    char *retval = str;

    while (*str && !isspace(*str))
        str++;
    if (isspace(*str))
        *str = '\0';

    return retval;
}

char *remove_whitespace(char *str) {
    if (!str)
        return str;

    return remove_trailing_whitespace(remove_leading_whitespace(str));
}

void print_cookie_report(char *str) {
    printf("%s\n", str);
}

void parse_response_headers(FILE *stream) {
    char *key, *value;
    char *line = NULL;
    size_t len = 0;

    while (getline(&line, &len, stream) > 2) {
        key = str_to_lower(strsep(&line, ":"));
        if (!strcmp(key, "set-cookie")) {
            value = remove_whitespace(strsep(&line, ";"));
            print_cookie_report(value);
        } else if (!strcmp(key, "transfer-encoding") && strstr(str_to_lower(line), "chunked")) {
            chunked_field_flag = 1;
        }
        line = key;
    }
    if (line) free(line);
}

// discard the following n bytes from stream
void skip_bytes(FILE *stream, size_t n) {
    size_t read_len;
    ssize_t bytes_left = n + 2;
    while (bytes_left) {
        read_len = MIN(bytes_left, BUFFLEN);
        if (fread(buffer, 1, read_len, stream) != read_len)
            syserr("fread");

        bytes_left -= read_len;
    }
}

// add the following chunk's length (provided) to content_len and discard the chunk
int parse_chunk(FILE *stream) {
    size_t chunk_len, len = 0;
    char *line = NULL, *endptr;

    if (getline(&line, &len, stream) < 0) {
        if (line) free(line);
        return 1;
    }

    chunk_len = strtoul(remove_whitespace(line), &endptr, 16);
    free(line);
    if (!chunk_len)
        return 1;

    skip_bytes(stream, chunk_len);

    content_len += chunk_len;
    return 0;
}

void calc_chunk_content_length(FILE *stream) {
    content_len = 0;
    while (!parse_chunk(stream));
}

ssize_t read_body(FILE *stream) {
    ssize_t ret, sum = 0;
    while ((ret = fread(buffer, 1, BUFFLEN, stream)) > 0) {
        if (ret < 0)
            syserr("fread");
        sum += ret;
    }
    return sum;
}

void calc_content_length(FILE *stream) {
    content_len = read_body(stream);
}

void print_content_length_report() {
    printf("Dlugosc zasobu: %lu\n", content_len);
}

int parse_read_response(FILE *stream) {
    if (parse_status_line(stream) != 0)
        return 1;

    parse_response_headers(stream);
    if (chunked_field_flag)
        calc_chunk_content_length(stream);
    else
        calc_content_length(stream);

    return 0;
}

int main(int argc, char *argv[]) {
    executable = argv[0];
    if (argc != 4)
        raise_err_usage();

    parse_connection_address(argv[1]);
    parse_host(argv[3]);

    connect_socket();

    write_to_host("GET /%.*s HTTP/1.1\r\n", (host_path ? strlen(host_path) : 0), host_path);
    write_to_host("Host: %.*s\r\n", host_addr_len, host_addr);
    if (!parse_cookies_from_file(argv[2])) {
        write_to_host("Cookie: %s\r\n", cookies);
        free(cookies);
    }
    write_to_host("Connection: close\r\n\r\n");

    FILE *stream = fdopen(sock, "rb");
    if (!stream)
        syserr("fdopen");

    if (!parse_read_response(stream))
        print_content_length_report();
    else
        (void) read_body(stream);

    if (fclose(stream) < 0)
        syserr("fclose");

    return 0;
}
