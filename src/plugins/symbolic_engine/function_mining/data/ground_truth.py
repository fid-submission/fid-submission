# this script is used to parse the ground truth

import os, sys

lines = []

with open('fb1') as f:
    lines = f.readlines()


def add_call_identified_func(res):
    fl = []
    with open("../../../../call_function_list.txt") as f:
        fl = f.readlines()

    cl = []
    for l in fl:
        if l.startswith("S_0x"):
            cl.append(l.strip())

    cl_set = set(cl)

    # note that ground truth is represented by bb number
    bl = translate_func_to_bb(cl_set)

    #print set(bl)
    return res.union(set(bl))


def collect_bb_info():
    res = []
    for i in os.listdir("./"):
        if "symbol" in i:
	    #print i
	    i1 = i.split("_")[3]
	    i1 = int(i1)
	    res.append(i1)

    res = sorted(res)

    return res



def dump_call_bb_list():
    fl = []
    with open("../../../../call_function_list.txt") as f:
        fl = f.readlines()

    cl = []
    for l in fl:
        if l.startswith("S_0x"):
            cl.append(l.strip())

    cl_set = set(cl)

    #print cl_set
    # note that ground truth is represented by bb number
    bl = translate_func_to_bb(cl_set)

    bls = set(bl)
    bls = map(int, bls)
 
    return bls



def translate_func_to_bb(cl):
    m = construct_map()

    #for k in sorted(m):
    #    print k

    #print m
    res = []
    for c in cl:
        if c in m:
            #print "found", c
            res.append(m[c])
        else:
            #print "undefined function", c
	    continue

    return res


def construct_map():
    m = {}

    for i in range(len(lines)):
        l = lines[i]
        if "globl" in l and "BB_" in lines[i-1]:
            b = lines[i-1]
            fn = l.split()[1].strip()
            #print b
            bn = b.split('_')[1][:-2]

            m[fn] = bn

    #print m
    return m


def obtain_gd():
    gd = []

    for l in lines:
        if "BB_" in l:
            #print l
            items = l.split('_')
            gd.append(items[1][:-2])

    return gd

def obtain_gd_fb2():
    gd = []

    for l in lines:
        if "<" in l:
            #print l
            items = l.split()
            gd.append(items[0])

    return gd




def obtain_ground_truth(gd):

    os.system('mkdir gd_folder')

    for i in gd:
        fn = "symbolic_formula_BB_" + str(i)
        os.system('cp ' + fn + ' gd_folder')


#gd = obtain_gd()
#
#obtain_ground_truth(gd)


def obtain_gt_2(gd):
    os.system('mkdir gd_folder_2')

    for i in gd:
        fn = "symbolic_formula_BB_" + str(i)

        lines = []
        with open(fn) as f:
            lines = f.readlines()

if __name__ == "__main__":
    dump_call_bb_list()
    #collect_bb_info()
    
