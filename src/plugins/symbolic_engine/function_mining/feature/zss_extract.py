import os
import os.path
import extract as E
from zss import simple_distance, Node


def distance(l1, l2):
    try:
        return simple_distance(l1, l2)
    except Exception:
        return -1


def similar(l1, l2):
    r = distance(l1, l2)
    if r == -1:
        r = distance(l1, Node("d"))
        if r == -1:
            r = distance(l2, Node("d"))
            if r == -1:
                return 0

    return r


def parse(l1):
    import ast_visitor as AV
    import compiler as C
    v = AV.MyVisitor()

    try:
        ast = C.parse(l1)
        #print ast
        C.walk(ast, v)
        #print '-'*20
        #print v.ast
        #print '-'*20

        return v.ast
    except:
	return Node("dummy")


def zss_1(l1, l2):
    l1 = parse(l1)
    l2 = parse(l2)

    #print '----------------------'
    #print l1
    #print '----------------------'
    #print l2
    #print '----------------------'

    return similar(l1, l2)


def random_files():
    from random import randint

    l = os.listdir('../data/')
    n = len(l)

    res = []
    fb = "../data/symbolic_formula_BB_"
    for i in range(50):
        res.append(fb + str(randint(0, n-50)))

    return res


def find_average_32(r1, key):
    res = []
    fnl = random_files()

    for fn in fnl:
        if os.path.isfile(fn):
            r2 = E.parse_file_32(fn)

            res.append(zss_1(r1, r2[key]))

    return float(sum(res))/len(res)


def find_average_64(r1, key):
    res = []
    fnl = random_files()

    for fn in fnl:
        if os.path.isfile(fn):
            r2 = E.parse_file_64(fn)

            res.append(zss_1(r1, r2[key]))

    return float(sum(res))/len(res)


def process(r, t, key):
    if t == 32:
        d = find_average_32(r, key)
    else:
        d = find_average_64(r, key)

    return [d]


if __name__ == '__main__':
    #a = ("reg8 + reg4")
    #c = ("a * 10 + Concat(0, Extract(4, 0, reg3))")
    #b = ("reg8 + 4 + 2 + 1")
    #d = ("Extract(31, 0, bvsdiv_i(64, Concat(0, reg2)))")
    #e = ("Concat(0, Extract(31, 1, 1 + reg3))")
    #f = ("Concat(0, Extract(31, 1, 1 + reg3)) + reg3")

    r = E.parse_file_64("../data/symbolic_formula_BB_1693")
    print process(r['%rax'], 64, "%rax")

    #import os
    #import extract as E

    #fb = "../data/"
    #count = 0
    #for i in os.listdir(fb):
    #    #if count == 30:
    #    #    break
    #    #elif "BB_" in i:
    #    if "BB_" in i:
    #        count += 1
    #        r = E.parse_file_64(fb + i)

    #        print i
    #        for k,v in r.items():
    #    	print v
    #            print parse(v)

    #print zss_1(c, f)
