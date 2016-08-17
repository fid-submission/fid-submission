import os
from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score
from sklearn import svm


def process(r):
    #print r
    data = []
    label = []
    for i in r:
        #print i
        data.append(i[:-1])
        label.append(i[-1])

    print "begin train"
    return train(data, label)

def test(m, r):
    data = []
    label = []
    for i in r:
        #print i
        data.append(i[:-1])
        label.append(i[-1])

    print "begin test"
    return test_with_train(m, data, label)


def train(x_train, y_train):
    C = 2^4
    lin_svc = svm.LinearSVC(C=C).fit(x_train, y_train)
    #lin_svc = svm.LinearSVC(C=C)

    return lin_svc

    #from sklearn import preprocessing
    #lb = preprocessing.LabelBinarizer()
    #lb.fit(y_train)
    #print y_train

    #a = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='accuracy')
    #print cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='precision')
    #print cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='recall')
    #print cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='f1')
    #return (p, r, f1)
    #p = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='precision')
    #r = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='recall')
    #f1 = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='f1')
    #pa = float(sum(p))/len(p)
    #ra = float(sum(r))/len(r)
    #fa = float(sum(f1))/len(f1)

    #print pa, ra, fa
    #return [pa, ra, fa]


def predict(m, x, y):
    from sklearn import preprocessing
    lb = preprocessing.LabelBinarizer()
    lb.fit(y)

    from sklearn.metrics import precision_score
    from sklearn.metrics import recall_score
    from sklearn.metrics import f1_score

    yp = m.predict(x)
    p = precision_score(y, yp, average="macro")
    r = recall_score(y, yp, average="macro")
    f1 = f1_score(y, yp, average="macro")
    #p = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='precision')
    #r = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='recall')
    #f1 = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='f1')
    #pa = float(sum(p))/len(p)
    #ra = float(sum(r))/len(r)
    #fa = float(sum(f1))/len(f1)

    print p, r, f1
    #return [pa, ra, fa]


def test_with_train():
    lines = []
    with open("llvm_O0_orig.csv") as f:
        lines = f.readlines()

    r = []
    for l in lines:
        items = l.split(",")
        items = map(lambda i: int(i), items)
        r.append(items)

    m = process(r)

    lines = []
    with open("llvm_O0_cfg_flatten.csv") as f:
        lines = f.readlines()

    r = []
    for l in lines:
        items = l.split(",")
        items = map(lambda i: int(i), items)
        r.append(items)

    test(m, r)



if __name__ == '__main__':
    test_with_train()

    #lines = []
    #with open("datasample.csv") as f:
    #    lines = f.readlines()

    #r = []
    #for l in lines:
    #    items = l.split(",")
    #    items = map(lambda i: int(i), items)
    #    r.append(items)

    #process(r)
