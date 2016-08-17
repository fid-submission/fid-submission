import os
from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score
from sklearn.ensemble import RandomForestClassifier


def process(r):
    #print r
    data = []
    label = []
    for i in r:
        #print i
        data.append(i[:-1])
        label.append(i[-1])

    return train(data, label)


def train(x_train, y_train):

    clf = RandomForestClassifier(n_estimators=500, max_features=5,
                                 max_depth=None, min_samples_split=1)
    #clf.fit(x_train, y_train)

    from sklearn import preprocessing
    lb = preprocessing.LabelBinarizer()
    lb.fit(y_train)
    #print y_train

    #a = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='accuracy')
    p = cross_val_score(clf, x_train, y_train, cv=10, scoring='precision')
    r = cross_val_score(clf, x_train, y_train, cv=10, scoring='recall')
    f1 = cross_val_score(clf, x_train, y_train, cv=10, scoring='f1')
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
