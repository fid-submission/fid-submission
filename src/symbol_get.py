# get all the resolved symbol information from relocation table


# input the name of a binary compiled with -E
# example:
#    gcc -Wl,-q simple.c


import os, sys

fn = sys.argv[1]



os.system('readelf -SW ' + fn + " > " + fn + '.secs')
os.system('objdump -Dr -j .text ' + fn + ' > '+fn + '.dis')
os.system('readelf -r ' + fn + ' > '+fn + '.sym')

fn1 = fn + '.dis'
fn2 = fn + '.sym'
fns = fn + '.secs'

lines1 = []
with open(fn1) as f:
    lines1 = f.readlines()

lines2 = []
with open(fn2) as f:
    lines2 = f.readlines()

lines_sec = []
with open(fns) as f:
    lines_secs = f.readlines()

tb = 0
te = 0
rdb = 0
rde = 0
db = 0
de = 0
bb = 0
be = 0
for l in lines_sec:
    if ".text" in l and "PROGBITS" in l:
        tb = int(l.split()[3], 16)
        te = tb + int(l.split()[5], 16)
    if ".rodata" in l and "PROGBITS" in l:
        rdb = int(l.split()[3], 16)
        rde = tb + int(l.split()[5], 16)
    if ".data" in l and "PROGBITS" in l:
        db = int(l.split()[3], 16)
        de = tb + int(l.split()[5], 16)
    if ".bss" in l and "NOBITS" in l:
        bb = int(l.split()[3], 16)
        be = tb + int(l.split()[5], 16)

def check_text (addrs):
    addr = int(addrs, 16)
    return (tb <= addr) and (addr < te)

def check_data (addrs):
    addr = int(addrs, 16)
    c1 = (rdb <= addr) and (addr < rde)
    c2 = (db <= addr) and (addr < de)
    c3 = (bb <= addr) and (addr < be)
    return c1 or c2 or c3

syms = []
for i in range(len(lines1)):
    l = lines1[i]
    if "R_386_" in l:
        lp = lines1[i-1]
        a = lp.split(':')[0]
        if "call" in lp:
            syms.append((a,'c2c'))
        else:
            if (check_text a):
                syms.append((a,'c2c'))
            elif (check_data a):
                syms.append((a,'c2d'))
            else:
                print a + " error"

is_data = False
is_rodata = False
for i in range(len(lines2)):
    l = lines2[i]
    if is_data == True:
        if "386" in l:
            a = l.split()[3]
            if (check_text a):
                syms.append((a,'d2c'))
            elif (check_data a):
                syms.append((a,'d2d'))
            else:
                print a + " error"

    if is_rodata == True:
        if "386" in l:
            a = l.split()[3]
            if (check_text a):
                syms.append((a,'d2c'))
            elif (check_data a):
                syms.append((a,'d2d'))
            else:
                print a + " error"

    if '.rel.data' in l:
        is_data = True
        is_rodata = False
    if '.rel.rodata' in l:
        is_rodata = True
        is_data = False

syms_l = map(lambda l : l[0] + " " + l[1] + "\n", syms)

# dump symbols
fn3 = fn + '.symtbl'
with open(fn3, 'w') as f:
    f.writelines(syms_l)
