#!/usr/bin/env python3
import struct, base64

x = list(struct.unpack('>Q',b'\x01\x02\x03\x04\x05\x06\x07\x08'))
#x = [0,0]
k = list(struct.unpack('>QQ',b'\x01\x23\x45\x67\x12\x34\x56\x78\x23\x45\x67\x89\x34\x56\x78\x9A'))
sum = 0

#for i in range(0,32):
#    x[0] = (x[0] + ((((x[1] << 4) ^ (x[1] >> 5)) + x[1]) ^ (sum + k[sum & 3]))) & 0xFFFFFFFF
#    sum = (sum + 0x9E3779B9) & 0xFFFFFFFF
#   x[1] = (x[1] + ((((x[0] << 4) ^ (x[0] >> 5)) + x[0]) ^ (sum + k[(sum >> 11) & 3]))) & 0xFFFFFFFF

#print(base64.b64encode(struct.pack('>II', x[0], x[1])).decode())
print (x * 4 + k)
#print(hex(x[0]) + ', ' + hex(x[1]))
