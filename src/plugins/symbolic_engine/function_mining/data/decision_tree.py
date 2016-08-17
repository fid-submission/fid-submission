import os
from sklearn import tree
from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score


def process(r):
    data = []
    label = []
    for i in r:
        #print i
        data.append(i[:-1])
        label.append(i[-1])

    print data
    print label
    return dt_basic(data, label)



def dt_basic(x_train, y_train):
    clf = tree.DecisionTreeClassifier(criterion="gini")
    clf.fit(x_train, y_train)
    
    from sklearn import preprocessing
    lb = preprocessing.LabelBinarizer()
    lb.fit(y_train)
    #print y_train
    
    p = cross_val_score(clf, x_train, y_train, cv=10, scoring='precision')
    r = cross_val_score(clf, x_train, y_train, cv=10, scoring='recall')
    f1 = cross_val_score(clf, x_train, y_train, cv=10, scoring='f1')
    return (p, r, f1)


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
