c = ""

with open("final.s") as f:
    c = f.read()


# 15M
log_size = 0x1000000

index = "index:\n.byte 0x00\n.byte 0x00\n.byte 0x00\n.byte 0x00"

c += index

buf = "\nbuf:"

for i in range(log_size):
    buf += "\n.byte 0x00"


c += buf

c += "\n"

with open("final.s", "w") as f:
    f.write(c)
