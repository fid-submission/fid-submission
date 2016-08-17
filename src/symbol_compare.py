# symbols from AIL
# finals_c2c_label.txt
# finals_c2d_label.txt
# finals_d2c_label.txt
# finals_d2d_label.txt

# symbols from compiler
# fn.symtbl


import sys, os

fn = sys.argv[1]

c_c2c = []
c_c2d = []
c_d2c = []
c_d2d = []

d_c2c = []
d_c2d = []
d_d2c = []
d_d2d = []

with open('final_c2c_label.txt') as f:
    d_c2c = f.readlines()
with open('final_c2d_label.txt') as f:
    d_c2d = f.readlines()
with open('final_d2c_label.txt') as f:
    d_d2c = f.readlines()
with open('final_d2d_label.txt') as f:
    d_d2d = f.readlines()

ts = []
with open(fn+'.symtbl') as f:
    ts = f.readlines()

for tl in ts:
    if "c2d" in tl:
        c_c2d.append(tl.split()[0])
    if "c2c" in tl:
        c_c2c.append(tl.split()[0])
    if "d2c" in tl:
        c_d2c.append(tl.split()[0])
    if "d2d" in tl:
        c_d2d.append(tl.split()[0])


d_c2c = filter(lambda l: l.strip() != "", d_c2c)
d_c2c = map(lambda l: int(l.strip(),16), d_c2c)
d_c2d = filter(lambda l: l.strip() != "", d_c2d)
d_c2d = map(lambda l: int(l.strip(),16), d_c2d)
d_d2c = filter(lambda l: l.strip() != "", d_d2c)
d_d2c = map(lambda l: int(l.strip(),16), d_d2c)
d_d2d = filter(lambda l: l.strip() != "", d_d2d)
d_d2d = map(lambda l: int(l.strip(),16), d_d2d)

# checking
def check(l1, l2):
    sl1 = set(l1)
    sl2 = set(l2)

    n1 = len(sl1.difference(sl2))
    n2 = len(sl2.difference(sl1))

    nb = len(sl1)

    if nb == 0 and not len(sl2) == 0:
        print "zero base value, false positive number : " + str(len(sl2))
        return
    elif nb == 0:
        print "zero base value"
    # false positive
    else:
        fp = n2/float(nb)
        # false negtive
        fn = n1/float(nb)

        print str(fp) + " " + str(fn)

print "c2c"
check(c_c2c, d_c2c)
print "c2d"
check(c_c2d, d_c2d)
print "d2c"
check(c_d2c, d_d2c)
print "d2d"
check(c_d2d, d_d2d)
