import os


class cd:
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)


def preprocess():
    tl0 = []
    tl1 = []

    b = []
    g = []

    with open("t0") as f:
        tl0 = f.readlines()

    with open("t11") as f:
        tl1 = f.readlines()

    # 0x80808080:32
    for l in tl0:
        a = l.split(":")[0][2:]
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




def process(i):
    os.system('bap-byteweight find ./' + i + ' > t0')

    # ground truth
    os.system('objdump -Dr -j .text ./' + i + ' > t1')
    os.system('grep ">:" t1 > t11')

    (g, b) = preprocess()

    return analysis(g, b, i)


def report (r):
    tp_t = 0
    fp_t = 0
    fn_t = 0

    for (p, r, tp, fp, fn, i) in r:
        print "| ", i, " | precision | ", p, " | recall ", r, "|"
        tp_t += tp
        fp_t += fp
        fn_t += fn

    p = float(tp_t) / (tp_t + fp_t)
    r = float(tp_t) / (tp_t + fn_t)

    print "| average | precision | ", p, " | recall ", r, "|"


data = "/home/x86-binaries/elf/coreutils/"


def iter_bin():
    results = []

    counter = 1
    with cd(data):
        os.system("rm t0 t1 t11")
        for i in os.listdir('./'):
            if "." not in i and "gcc_coreutils_32_O2" in i:
                if counter == 30:
                    break
                counter += 1
                print '##############', i, '################'
                results.append(process(i))
            else:
                continue

    report(results)

iter_bin()
