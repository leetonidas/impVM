#include <netinet/in.h>
#include "utils.h"

uint64_t htonll(uint64_t x){
    return ((1==htonl(1)) ? (x) : ((uint64_t)htonl((x) & 0xFFFFFFFF) << 32) | htonl((x) >> 32));
}

uint64_t ntohll(uint64_t x) {
    return ((1==ntohl(1)) ? (x) : ((uint64_t)ntohl((x) & 0xFFFFFFFF) << 32) | ntohl((x) >> 32));
}