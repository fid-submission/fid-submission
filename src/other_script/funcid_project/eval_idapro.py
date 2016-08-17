import os


class cd:
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)


def preprocess(fn):
    tl0 = []
    tl1 = []

    b = []
    g = []

    with open(fn+".txt") as f:
        tl0 = f.readlines()

    with open("t11") as f:
        tl1 = f.readlines()


    for l in tl0:
        a = l.split()[0]
        if "sub" in a:
            # ignore term_proc and init_proc
            a = a.split("_")[1]
            b.append(int(a, 16))


    # 080808080 <func_name>
    for l in tl1:
        a = l.split()[0]
        g.append(int(a, 16))

    return (g, b)


def analysis(g, b, f_name):
    fp = 0
    fn = 0
    tp = 0
    tn = 0


    for i in b:
        if i in g:
            tp += 1
        else:
            fp += 1

    p = float(tp) / (tp + fp)

    for i in g:
        if i in b:
            continue
        else:
            fn += 1

    r = float(tp) / (tp + fn)

    return (p, r, tp, fp, fn, f_name)


data = "/home/work/coreutils_clang_obfuscated/cfg_flatten/src/"


def process(fn):
    fn1 = data + fn
    os.system('objdump -Dr -j .text ' + fn1 + ' > t1')
    os.system('grep ">:" t1 > t11')

    # ground truth

    (g, b) = preprocess(fn)

    return analysis(g, b, fn)


def report (r):
    tp_t = 0
    fp_t = 0
    fn_t = 0

    for (p, r, tp, fp, fn, i) in r:
        print "| ", i, " | ", p, " | ", r, "|"
        tp_t += tp
        fp_t += fp
        fn_t += fn

    p = float(tp_t) / (tp_t + fp_t)
    r = float(tp_t) / (tp_t + fn_t)

    print "| average | ", p, " | ", r, "|"


ida_data = "/home/work/ida_data/cfg_flatten/"


def iter_bin():
    results = []

    counter = 1
    with cd(ida_data):
        os.system("rm t1 t11")
        for i in os.listdir('./'):
            #if counter == 2:
            #    break
            #counter += 1
            i = i.split(".")[0]
            print '##############', i, '################'
            results.append(process(i))


    report(results)

iter_bin()
