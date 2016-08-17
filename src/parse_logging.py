import os


lines = []

with open('trace_hex.txt') as f:
    lines = f.readlines()


#lines = filter(lambda x: not x.startswith("*"), lines[:-1] )

#print len(lines)

# every 16M
chunk = []
c = 0
trace = []


def rebuild (bs):
# 43b2 0400
    a1 = "0x08" + bs[1][0:2] + bs[0][2:4]+bs[0][0:2]
    a2 = "0x08" + bs[3][0:2] + bs[2][2:4]+bs[2][0:2]
    a3 = "0x08" + bs[5][0:2] + bs[4][2:4]+bs[4][0:2]
    a4 = "0x08" + bs[7][0:2] + bs[6][2:4]+bs[6][0:2]
    return [a1,a2,a3,a4]


for l in lines:

    bs = l[9:48].split()

    addrs = rebuild(bs)

    chunk.append(addrs[0]+"\n")
    chunk.append(addrs[1]+"\n")
    chunk.append(addrs[2]+'\n')
    chunk.append(addrs[3]+'\n')

    c += 16

    if c == 0x1000000:
        print "asd"
        # 16 M chunk
        trace += reversed(chunk)

        chunk = []
        c = 0

print len(trace)

with open('trace.txt', 'w') as f:
    f.writelines(trace)
