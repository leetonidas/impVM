#!/usr/bin/env python3

import string, struct, sys

code2 = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

code = b''
with open(sys.argv[1], 'rb') as f:
    for l in f:
        if not l.startswith(b'#'):
            code = code + l.strip()

code += b'\0'
code = code.ljust(((len(code) + 7) // 8) * 8, b'\0')
dat = list(struct.unpack('>' + 'Q' * (len(code) // 8), code))
dat = [0, len(dat) + 2] + dat + [0] * int(sys.argv[2])
for e in dat:
    print(str(e))