from sklearn import ensemble
from sklearn.externals.six import StringIO
from sklearn.cross_validation import cross_val_score
from sklearn.ensemble import AdaBoostClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn import svm


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

    clf1 = svm.LinearSVC(C=2^4)
    clf2 = AdaBoostClassifier(n_estimators=100)
    clf3 = GradientBoostingClassifier(n_estimators=100, learning_rate=1.0,
                                      max_depth=1, random_state=0)
    #clf.fit(x_train, y_train)

    from sklearn import preprocessing
    lb = preprocessing.LabelBinarizer()
    lb.fit(y_train)
    #print y_train

    eclf = VotingClassifier(estimators=[('ls', clf1), ('ad', clf2), ('gbc',
                                                                     clf3)], voting="hard")

    #a = cross_val_score(lin_svc, x_train, y_train, cv=10, scoring='accuracy')
    p = cross_val_score(eclf, x_train, y_train, cv=10, scoring='precision')
    r = cross_val_score(eclf, x_train, y_train, cv=10, scoring='recall')
    f1 = cross_val_score(eclf, x_train, y_train, cv=10, scoring='f1')
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
