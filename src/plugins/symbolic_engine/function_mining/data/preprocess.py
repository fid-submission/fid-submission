import os, hashlib
# FIXME: uncomment the line below
import feature_syntax as FS
import kmean_cluster as KC
import decision_tree as DT
import linear_svc as LSVC
import ground_truth as GT
import random_forest as RF
import ada_boost as AB
import major_voting as MV
import soft_voting as SV
import tune_svc as TS
import mean_shift as MS


def remove_unsiginificant():
    for i in os.listdir('./'):
        if "symbolic_formula_BB_" in i:
            md5 = hashlib.md5(open(i, 'rb').read()).hexdigest()
            if md5 == 'a0d8c5ef78c3af8bdbddba3b3fdf4953':
                os.system('rm ' + i)


def extract_feature_vector():
    # this method is one of the key study of our research
    gd = GT.obtain_gd()
    #return FS.process()
    return FS.process_label(gd)

def extract_distribution_vector():
    return FS.process_2()


def clustering(r):
    # k-mean methods require us to provide the number of clusters.
    #print r
    return KC.process(r)


def decision_tree(r):
    return DT.process(r)


def linear_svc(r):
    return LSVC.process(r)


def random_forest(r):
    return RF.process(r)


def ada_boost(r):
    return AB.process(r)


def major_voting(r):
    return MV.process(r)


def soft_voting(r):
    return SV.process(r)


def tune_svc(r):
    return TS.process(r)

def mean_shift(r):
    return MS.process(r)

def process():
    #r = extract_feature_vector()
    #return r
    r = extract_distribution_vector()
    return r
    #return clustering(r)
    #return mean_shift(r)
    #return decision_tree(r)
    #return linear_svc(r)
    #return random_forest(r)
    #return ada_boost(r)
    #return major_voting(r)
    #return soft_voting(r)
    #return tune_svc(r)
