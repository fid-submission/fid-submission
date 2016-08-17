import os
from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score
from sklearn import svm
import numpy as np
from sklearn import cross_validation, datasets, svm

def tune(X, y):
    #digits = datasets.load_digits()
    #X = digits.data
    #y = digits.target

    svc = svm.SVC(kernel='linear')
    C_s = np.logspace(-10, 0, 10)

    scores = list()
    scores_std = list()
    for C in C_s:
        svc.C = C
        this_scores = cross_validation.cross_val_score(svc, X, y, n_jobs=1)
        scores.append(np.mean(this_scores))
        scores_std.append(np.std(this_scores))

    # Do the plotting
    import matplotlib.pyplot as plt
    plt.figure(1, figsize=(4, 3))
    plt.clf()
    plt.semilogx(C_s, scores)
    plt.semilogx(C_s, np.array(scores) + np.array(scores_std), 'b--')
    plt.semilogx(C_s, np.array(scores) - np.array(scores_std), 'b--')
    locs, labels = plt.yticks()
    plt.yticks(locs, list(map(lambda x: "%g" % x, locs)))
    plt.ylabel('CV score')
    plt.xlabel('Parameter C')
    plt.ylim(0, 1.1)
    plt.show()


def process(r):
    #print r
    data = []
    label = []
    for i in r:
        #print i
        data.append(i[:-1])
        label.append(i[-1])

    tune(data, label)
    #return train(data, label)


def train(x_train, y_train):
    C = 2^4
    #lin_svc = svm.LinearSVC(C=C).fit(x_train, y_train)
    lin_svc = svm.LinearSVC(C=C)


    from sklearn import preprocessing
    lb = preprocessing.LabelBinarizer()
    lb.fit(y_train)
    #print y_train

    #a = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='accuracy')
    p = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='precision')
    r = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='recall')
    f1 = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='f1')
    pa = float(sum(p))/len(p)
    ra = float(sum(r))/len(r)
    fa = float(sum(f1))/len(f1)

    #print pa, ra, fa
    return [pa, ra, fa]


if __name__ == '__main__':
    lines = []
    with open("datasample.csv") as f:
        lines = f.readlines()

    r = []
    for l in lines:
        items = l.split(",")
        items = map(lambda i: int(i), items)
        r.append(items)

    process(r)
