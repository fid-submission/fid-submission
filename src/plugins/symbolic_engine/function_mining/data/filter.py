import os

lines = []

with open('datasample.csv') as f:
    lines = f.readlines()

t = []
f = []

for l in lines:
    if l.strip().endswith('1'):
        t.append(l)
    else:
        f.append(l)


with open('datasample_t.csv', 'w') as f:
    f.writelines(t)

with open('datasample_f.csv', 'w') as f:
    f.writelines(f)
