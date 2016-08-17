# this is one of our feature vector construction trial.

# In this work, we try to build feature vector based on syntax features


# We construct a vector like this:

# (N_number_of_reg, N_number_of_arg, N_number_of_plus, N_number_of_minus, N_number_of_and)

# For instance:
#  Y = (x1 * 5) >> 4 + (x2 * 6)
#  (2, 3, 4)

# FIXME: Note that Concat and Extract is considered as a operator...?

import os, hashlib

def seperate(lines):
    r = {
        '%ebp' : '',
        '%eax' : '',
        '%edi' : '',
        '%ebx' : '',
        '%ecx' : '',
        '%eip' : '',
        '%edx' : '',
        '%eiz' : '',
        '%esi' : '',
        '%esp' : '',
    }

    t = ""
    for l in lines:
        if '=' not in l:
            t += l
        else:
            items = l.split(' = ')
            if items[0] in r:
                r[items[0]] = t + items[1]
                t = ''
            elif 'stack+' in items[0]:
                r[items[0]] = t + items[1]
                t = ''
            else:
                raise Exception("undefined register")

    return r

# parse a have a feature vector like this:
# (N_number_of_reg, N_number_of_arg, N_number_of_plus, N_number_of_and)
def parse_a(r):
    t = r['%esp']
    n_r = 0
    n_a = 0
    n_p = 0
    n_m = 0

    n_a = t.count('arg')
    n_r = t.count('reg')

    n_and = t.count('&')

    n_p = t.count('+')
    n_m = t.count('-')


    return (n_r, n_a, n_p, n_m, n_and)


# parse b have a feature vector like this:
# (N_number_of_reg, N_number_of_arg, N_number_of_plus, N_number_of_minus, Check_on_large_minus, N_number_of_and)
# parse b is designed to capture this situation:

#  foo:
#       sub $0x18, %esp
#       ...

# Also avoid to this situation (usually happends right before ret instruction):
#
#       pop %eax
#       pop %ebx
#       pop %esi

def parse_b(r):
    t = r['%esp']
    n_r = 0
    n_a = 0
    n_p = 0
    n_m = 0

    n_a = t.count('arg')
    n_r = t.count('reg')

    n_and = t.count('&')

    n_p = t.count('+')
    n_m = t.count('-')

    if '-' in t:
        # reg - 28
        if t.count('-') == 1:
            items = t.split(' ')
            i = items.index('-')
            n = items[i+1]
            if n > 4:
            #    print t
                n_lm = 1
            else:
                n_lm = 0
        else:
            n_lm = 0
    else:
        n_lm = 0


    return (n_r, n_a, n_p, n_m, n_lm, n_and)


def parse_c(r):
    # feature extraction without simplifying logic formulas.
    t = r['%esp']
    f = 0
    if "reg8 -" in t:
        f = 1

    return (f, 0, 0)


def parse_d(r):
    # feature extraction after logic formula simplification.
    t = r['%esp']
    f = 0
    # note that z3 would translate esp - 4 - 4 to esp + 0xfffffff4
    #
    if "42949" in t:
        f = 1

    return (f, 0, 0)


def check_stack(r):
    rt = [0] * 3

    if 'stack+4' in r:
        if 'mem' in r['stack+4']:
            rt[0] = 1
    if 'stack+8' in r:
        if 'mem' in r['stack+8']:
            rt[1] = 1
    if 'stack+12' in r:
        if 'mem' in r['stack+12']:
            rt[2] = 1

    return rt


def parse_e(r):
    t = r['%esp']
    f = 0
    # note that z3 would translate esp - 4 - 4 to esp + 0xfffffff4
    #
    if "42949" in t:
        f = 1


    rt = check_stack(r)

    #print rt

    return (f, rt[0], rt[1], rt[2])



def parse_f(r):
    #print r
    t = r['%esp']
    t1 = r['%ebp']
    f = 0
    f1 = 0
    # note that z3 would translate esp - 4 - 4 to esp + 0xfffffff4
    #
    if "42949" in t:
        f = 1

    if "42949" in t1:
        f1 = 1

    rt = check_stack(r)

    #print rt
    f2 = 0
    if f1 == 1 and f == 1:
	f2 = 1
    else:
	f2 = 0

    #print f
    return (f, f1, rt[0], rt[1], rt[2])
    #return (f, f1, f * rt[0],  f * rt[1], f * rt[2])

def parse_advanced(fn):
    import sys
    sys.path.insert(0, '../feature/')

    import extract as E

    rr = E.feature_extract_32(fn)
    print rr
    return rr

def parse_advanced_multi(fn):
    import sys
    sys.path.insert(0, '../feature/')

    import extract as E

    E.mul_process(fn)

    res = {}
    for i in range(1, 7):
        fn = "log_" + str(i) + ".log"
        lines = []
        with open(fn) as f:
            lines = f.readlines()

        for l in lines:
            items = l.strip().split(":")
            bb_id = items[0].split('_')[-1].strip()

            res[int(bb_id)] = map(float, items[1].split(","))

    return res


def parse_g(r):
    t = r['%esp']
    t1 = r['%ebp']
    t2 = r['%eax']
    t3 = r['%ebx']
    t4 = r['%ecx']
    t5 = r['%edx']
    f = 0
    f1 = 0
    f2 = 0
    f3 = 0
    f4 = 0
    f5 = 0
    # note that z3 would translate esp - 4 - 4 to esp + 0xfffffff4
    #
    if "42949" in t:
        f = 1
    if "42949" in t1:
        f1 = 1
    if "42949" in t2:
        f2 = 1
    if "42949" in t3:
        f3 = 1
    if "42949" in t4:
        f4 = 1
    if "42949" in t5:
        f5 = 1

    rt = check_stack(r)

    #print rt

    return (f, f1, f2, f3, f4, f5, rt[0], rt[1], rt[2])


def process():
    res = {}
    fl = []  
    bbn = []


    for i in os.listdir('./'):
	#print i
        if "symbolic_formula_BB_" in i:
	    bbn.append(i)

    for bn in bbn:
        lines = []
        bb_id = bn.split('_')[-1]
        #fl.append(bn)
        with open(bn) as f:
            lines = f.readlines()
            r = seperate(lines)
            #res[bb_id] = parse_b(r)
            #res[bb_id] = parse_e(r)
            res[bb_id] = parse_f(r)
            #res[bb_id] = parse_advanced(i)
            #res[bb_id] = parse_g(r)


    #res = parse_advanced_multi(fl)

    #print res
    print_feature_vector(res)
    return res


def process_label(gd):
    res1 = []
    res = process()

    #print res

    # note that we have changed into log_*.log

    gd = map(int, gd)

    for k in sorted(res):
	v = res[k]
	
	#print k
        if k in gd:
            # function starting
            v1 = list(v)
            v1.append(1)
            res1.append(v1)
        else:
            v1 = list(v)
            v1.append(0)
	    res1.append(v1)

    return res1


def process_2():
    res1 = []
    res = process()

    #print res

    # note that we have changed into log_*.log

    #res1 = []
    #for k in sorted(res):
    #	res1.append(res[k])

    return res


def print_feature_vector(r):

    with open("feature_vector.txt", "w") as f:
        for k,v in r.items():
            f.write("bb : " + str(k) + "end\n")
            f.write(str(v) + "\n")

if __name__ == '__main__':
    iterate_data_sample()
