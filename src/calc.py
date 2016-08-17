lines = []

with open('temp1') as f:
    lines = f.readlines()

base = float(lines[0])

res = []

for i in range(1,len(lines)):
    t = float(lines[i])
    t1 = (t-base)/base
    res.append(t1*100)


for i in res:
    print i


