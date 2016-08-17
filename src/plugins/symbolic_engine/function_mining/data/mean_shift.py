print(__doc__)

import numpy as np
from sklearn.cluster import MeanShift, estimate_bandwidth
from sklearn.datasets.samples_generator import make_blobs
from sklearn import cluster


def mining(r_list, d_list, k):
    res = [None] * k

    k_means = cluster.KMeans(n_clusters=k)

    k_means.fit(d_list)

    for i in range(len(r_list)):
        bn = r_list[i][0]
        label = k_means.labels_[i]

        #if label == 2:
        #    res.append(bn)
        if res[label] == None:
            res[label] = []

        res[label].append(bn)


    #index = 0
    #minn = len(res[0])

    #for i in range(len(res)):
    #    if len(res[i]) < minn:
    #        minn = len(res[i])
    #        index = i


    #print index
    #return res[index]
    return res


def process(r):
    r_list = []
    d_list = []
    for k,v in r.items():
        r_list.append((k,v))
        d_list.append(v)

    #return mining(r_list, d_list, 4)
    #return mining(r_list, d_list, 4)
    return num_cluster(d_list)


def num_cluster(X):
    ms = MeanShift(bin_seeding=True)
    ms.fit(X)
    labels = ms.labels_
    #cluster_centers = ms.cluster_centers_

    labels_unique = np.unique(labels)
    n_clusters_ = len(labels_unique)

    print("number of estimated clusters : %d" % n_clusters_)
