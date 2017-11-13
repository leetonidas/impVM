#!/usr/bin/env python3

# this script takes as arguments the hostname.
# test if your service is exploitable
# return one of the following statuses:
#     'none': no info available (initial state)
#     'offline': couldn't connect to service
#     'good': everything worked, got the flag
#     'bad': something is wrong with the service
#     'timeout': exploit took too long

################################################################################

def main():
    flags, ports = read_meta()

    # returns a list of sockets (most people will only need socks[0])
    socks = connect(ports)

    def recv_until(sock, delim):
        buf = sock.recv(1)
        while delim not in buf:
            buf += sock.recv(1)
        return buf

    pwn = open("xtea_de.cmp", 'rb').read()
    socks[0].send(pwn)
    try:
        recv_until(socks[0], flags[0].encode())
    except:
        status('bad')
        return
    status('good')
    
################################################################################

if __name__ == "__main__":

    import sys, socket, signal, json

    if len(sys.argv) < 2:
        print('arguments: hostname')
        exit(1)

    real_print = print
    def print(*args, **kwargs):
        # make printing to stderr the default
        if 'file' not in kwargs:
            kwargs['file'] = sys.stderr
        return real_print(*args, **kwargs)

    def status(s, r = 0):
        real_print('STATUS', s)
        exit(r)

    def handle_timeout(*args):
        status('timeout')

    def connect(ports):
        socks = []
        for external_port, internal_port in ports:
            sock = socket.socket()
            try:
                sock.connect((sys.argv[1], external_port))
            except socket.error:
                status('offline', 1)
            socks.append(sock)
        return socks

    def read_meta():
        with open('META.json') as f:
            meta = json.load(f)
            return meta['flags'], meta['ports']


    signal.signal(signal.SIGALRM, handle_timeout)
    signal.alarm(10)

    main()

    status('none')


