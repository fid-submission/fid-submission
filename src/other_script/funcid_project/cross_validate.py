import os
from sklearn import tree
from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score
from sklearn import cross_validation, datasets, linear_model


import numpy as np


import operator


def mean(l):
    return sum(l) / len(l)

data = []
target = []


def load_data():
    global data
    global target

    lines = []
    with open('datasample.csv') as f:
        lines = f.readlines()

    lines = map(lambda l : l.strip(), lines)

    for d in lines:
        target.append(d.split(',')[-1])

        data.append(d.split(',')[:-1])


from sklearn import preprocessing
lb = preprocessing.LabelBinarizer()

def prune_dt(maxn, data, target):
    r = {}

    for i in range(2, maxn+1):
        clf = tree.DecisionTreeClassifier(max_leaf_nodes=i)
	clf.fit(data, target)
        r[i] = clf.score(data, target)

    i = max(r.iteritems(), key=operator.itemgetter(1))[0]
    #print i
    #print r[i]

    return i



def kf_dt():
    #  we have totally 186794 data

    # 20% validate, 80% training model.
    # 186794 * 0.2 = 37359
    # 186794 - 37359 = 149435

    global data
    global target
    p = []
    r = []
    f = []

    data = np.array(data)
    target = map(lambda t : int(t), target)
    target = np.array(target)

    from sklearn.cross_validation import KFold
	
    k_fold = cross_validation.KFold(len(data), 5)
	
    from sklearn import preprocessing
    from sklearn import svm

    lb = preprocessing.LabelBinarizer()

    for k, (train, test) in enumerate(k_fold):

	#m = np.linspace(1, 60, 1)
	print train
	print test
	print data[train]
	m = prune_dt(60, data[train], target[train])

    #    clf = tree.DecisionTreeClassifier(max_leaf_nodes=m)

	C = 2^4
    	clf = svm.LinearSVC(C=C).fit(data[train], target[train])

	#clf.fit(data[train], target[train])

#	lb.fit(target[test])
		
	from sklearn.metrics import precision_score
	from sklearn.metrics import recall_score 
	#pred = clf.predict(data[test])
	#print list(pred)

	#print '-' * 100


	#print list(target[test])
	#print recall_score(target[test], pred)
	#print precision_score(target[test], pred)
	p = mean(cross_val_score(clf, data[test], target[test], cv=10, scoring='precision'))
	r = mean(cross_val_score(clf, data[test], target[test], cv=10, scoring='recall'))
	#f1 = mean(cross_val_score(clf, data[test], target[test], cv=10, scoring='f1'))
    	#print("[fold {0}] m {1} precison: {2:.5f} recall {2:.5f} f1 {2:.5f}".
          #format(k, m, clf.score(data[test], target[test])))
        #  format(k, m, p, r, f1))

        #p.append(mean(cross_val_score(clf, test_data, test_target, cv=10, scoring='precision')))
        #r.append(mean(cross_val_score(clf, test_data, test_target, cv=10, scoring='recall')))
        #f.append(mean(cross_val_score(clf, test_data, test_target, cv=10, scoring='f1')))

        #print str(m) + " , precision : " + str(p[-1])
        #print str(m) + " , recall : " + str(r[-1])
        #print str(m) + " , f1 score : " + str(f[-1])

    print mean(p)
    print mean(r)
    print mean(f)


load_data()
kf_dt()
