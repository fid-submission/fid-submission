import os
import ground_truth as GT

import preprocess as P

import sys
import numpy as NP


def data():
    gd = GT.obtain_gd()

    test = P.process()

    return (gd, test)


def data_write_disk():
    test = P.process()

    #return test
    test = map(lambda t: ', '.join(str(x) for x in t), test)
    test = map(lambda t: t+"\n", test)
    #t = ", ".join(test)
    #print "precision: ", test[0]
    #print "recall : ", test[1]
    #print "f1 : ", test[2]

    #print test[0]
    #print test[1]
    #print test[2]

    ##print NP.mean(test[0])
    ##print NP.mean(test[1])
    ##print NP.mean(test[2])
    #print "precision:", NP.mean(test[0])
    #print "recall:", NP.mean(test[1])
    #print "f1:", NP.mean(test[2])
    with open("datasample.csv", "a") as f:
        f.writelines(test)

    GT.dump_call_bb_list()


from itertools import product

def dump_distribution():
    test = P.process()
    #print test

    bl = GT.dump_call_bb_list()
    #print bl
    
    t = []
    for k,v in test.items():
	#if int(k) in bl:
	if True:
 	    #print k, v
	    t.append(v)


    #t1 = map(lambda e: (e[0], e[1], e[2]*e[3]*e[4]), t)
    t1 = map(lambda e: (e[0], e[1], e[2], e[3], e[4]), t)

    #print t1
    k = len(t1[0])
    kk = list(product(range(2), repeat=k))

    res = [0] * len(kk)

    for t11 in t1:
        i = kk.index(t11)
	#print i, t11
	res[i] += 1

    res = map(lambda i: float(i)/len(t1), res)

    print "distribution:", res
    print "length:", len(t1)

    

import itertools

def findsubsets(S, m):
    return set(itertools.combinations(S, m))


def match_fname(gd, test):
    for c in test:
        gd = set(gd)
        test = set(c)

        if gd.issuperset(test):
            print "no false positive"
            mm = gd.difference(test)
            #print gd
            #print test
            #print mm
            print 1- len(mm)/float(len(gd))
        else:
            print "false positive"
            #print m



def aux(gd, test, total):
    gd = set(gd)
    test = set(test)
    #total = set(total)
    #print total

    if gd.issuperset(test):
        print "no false positive"
        mm = gd.difference(test)
        #print mm
        #print "accurary: ", 1- len(mm)/float(len(gd))
    else:
        """
                      Condition: A        Not A

        Test says "A"       True positive   |   False positive
        Test says "Not A"   False negative  |    True negative
        """
        # true positive
        tp = gd.intersection(test)
        #
        ## false positive
        fp = test.difference(tp)
        #print "fp: ", fp

        #print "fp : " , len(fp)

        ## false negative
        fn = gd.difference(tp)
        #print "fn: ", fn
        #print "fn : " , len(fn)
        #print fn

        ## true negative
        #gd_n = total.difference(gd)
        #test_n = total.difference(test)

        #tn = gd_n.intersection(test_n)

        #print "precision : ", len(tp)/float(len(tp) + len(fp))
        #print "recall : ", len(tp)/float(len(tp) + len(fn))

        p = len(tp)/float(len(tp) + len(fp))
        r = len(tp)/float(len(tp) + len(fn))

    	print "precision:" , p
    	print "recall:" , r
    	print "f1:" , 2*p*r/(p+r)
    	print "tp:" , len(tp)
    	print "fp:" , len(fp)
    	print "fn:" , len(fn)
        return (p, r, len(tp), len(fp), len(fn))


       # print "true positive rate : "  , len(tp)/float(len(tp) + len(fp))
       # print "false positive rate : " , len(fp)/float(len(fp) + len(tn))
       # print "true negative rate : "  , len(tn)/float(len(fp) + len(tn))
       # print "false negative rate : "  , 1 - len(tp)/float(len(tp) + len(fp))


def match_fname_mult_bb_size(gd, test, total, n):
    gd = set(gd)
    indexs = findsubsets([0, 1, 2, 3], n-1)
    res = []


    print "bb total: ", total

    func_estimate = int(total) / 10

    dis = 99999999

    for indext in indexs:
        (i1, i2, i3) = indext

        s1 = set(test[i1])
        s2 = set(test[i2])
        s3 = set(test[i3])

        s0 = s1.union(s2)
        s0 = s0.union(s3)

        #print s0

        if abs(len(set(s0)) - func_estimate) < dis:
            dis = abs(len(s0) - func_estimate)

            #print len(set(s0)), dis
            res = set(s0)

    aux(gd, res, total)
    res = GT.add_call_identified_func(res)
    aux(gd, res, total)



def match_fname_mult(gd, test, total, n):
    gd = set(gd)
    # n : how many subsets to intersect.
    #print test
    indexs = findsubsets([0, 1, 2, 3], n-1)
    #indexs = findsubsets([0,1,2,3], n-1)
    #indexs = findsubsets([0,1,2], n-1)
    res = []

    for indext in indexs:
        #print indext
        #(i1, i2, i3, i4) = indext
        (i1, i2, i3) = indext
        #(i1, i2) = indext
        s1 = set(test[i1])
        s2 = set(test[i2])
        s3 = set(test[i3])

        s0 = s1.union(s2)
        s0 = s0.union(s3)

        #print s0

        res.append(aux(gd, s0, total))

    return res


def generate_total_list(t):
    t = range(1, t+1)
    return t


def select(res):
    print res
    s = map(lambda r: 2 * r[0] * r[1] / (r[0] + r[1]), res)
    smax = max(s)

    smax_i = s.index(smax)

    (p, r, tp, fp, fn) = res[smax_i]

    f1 = 2 * p * r / float((p + r))

    print "precision:" + str(p)
    print "recall:" + str(r)
    print "f1:" + str(f1)
    print "tp:" + str(tp)
    print "fp:" + str(fp)
    print "fn:" + str(fn)


#if __name__ == '__main__':
#    total = sys.argv[1]
#
#    (gd, test) = data()
#    #os.system("rm datasample.csv")
#    #data_write_disk()
#    #print len(gd)
#    #print test
#    #match_fname(gd, test)
#    res = match_fname_mult(gd, test, total, 4)
#    #res = match_fname_mult(gd, test, total, 2)
#    #res = match_fname_mult(gd, test, total, 5)
#    select(res)
#    #match_fname_mult(gd, test, total, 3)
#    # note that as we cluster to 3 groups.
#    #print test[0]
#    #print len(test[0])
#    #print len(test[1])
#    #print len(test[2])
#    #print len(test[3])
#    #aux(gd, test[0], total)
#    #aux(gd, test[1], total)

def unsupervised_learning():
    (gd, test) = data()
    #-------------------------------------------
    #res = match_fname_mult(gd, test, total, 4)
    #select(res)
    #-------------------------------------------
    match_fname_mult_bb_size(gd, test, total, 4)
    #res = match_fname_mult(gd, test, total, 2)
    #res = match_fname_mult(gd, test, total, 5)
    #match_fname_mult(gd, test, total, 3)
    # note that as we cluster to 3 groups.
    #print test[0]
    #print len(test[0])
    #print len(test[1])
    #print len(test[2])
    #print len(test[3])
    #aux(gd, test[0], total)
    #aux(gd, test[1], total)

def supervised_learning():
    #(gd, test) = data()
    #os.system("rm datasample.csv")
    #data_write_disk()
    dump_distribution()


if __name__ == '__main__':
    total = sys.argv[1]
    supervised_learning()
    #unsupervised_learning()
