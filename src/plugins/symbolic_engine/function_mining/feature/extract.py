import zss_extract as ZE
import lexical_extract as LE
import ast_extract as AE


def parse_file_32(fn):

    lines = []

    with open(fn) as f:
        lines = f.readlines()

    r = {
        '%ebp': '',
        '%eax': '',
        '%edi': '',
        '%ebx': '',
        '%ecx': '',
        '%eip': '',
        '%edx': '',
        '%eiz': '',
        '%esi': '',
        '%esp': '',
    }

    t = ""
    k = ""
    for l in lines:
        l = l.strip()
        if '=' not in l:
            r[k] += l
        else:
            items = l.split(' = ')
            k = items[0]
            if items[0] in r:
                r[k] = t + items[1]
                t = ''
            elif 'stack+' in items[0]:
                r[k] = t + items[1]
                t = ''
            else:
                raise Exception("undefined register")

    return r


def parse_file_64(fn):

    lines = []

    with open(fn) as f:
        lines = f.readlines()

    r = {
        '%rbp': '',
        '%rax': '',
        '%rdi': '',
        '%rbx': '',
        '%rcx': '',
        '%rip': '',
        '%rdx': '',
        '%riz': '',
        '%rsi': '',
        '%rsp': '',
    }

    t = ""
    k = ""
    for l in lines:
        l = l.strip()
	l = l.replace("k!", "")
        if '=' not in l and k in r:
            r[k] += l
        else:
            items = l.split(' = ')
            k = items[0]
            if items[0] in r:
                r[k] = t + items[1]
                t = ''
            elif 'stack+' in items[0]:
                r[k] = t + items[1]
                t = ''
            else:
                continue

    return r


def feature_extract_32(fn):

    d = parse_file_32(fn)

    esp = d['%esp']
    ebp = d['%ebp']

    r = []
    r += reg_process_32(esp, '%esp')
    r += reg_process_32(ebp, '%ebp')

    r += mem_process_32(d)

    return r


def feature_extract_64(fn):

    d = parse_file_64(fn)

    esp = d['%rsp']
    ebp = d['%rbp']

    r = []
    r += reg_process_64(esp, '%rsp')
    r += reg_process_64(ebp, '%rbp')

    r += mem_process_64(d)

    return r


def reg_process_64(r, key):
    res = []

    res += ZE.process(r, 64, key)
    res += LE.process(r)
    res += AE.process(r)

    return res


def reg_process_32(r, key):
    res = []

    # 2016-01-01 un-comment two lines
    res += ZE.process(r, 32, key)
    res += LE.process(r)
    res += AE.process(r)

    return res


def mem_process_64(r):
    rt = [0] * 9

    if 'stack+1' in r:
        rt[0] = 1
    if 'stack+2' in r:
        rt[1] = 1
    if 'stack+3' in r:
        rt[2] = 1
    if 'stack+4' in r:
        rt[3] = 1
    if 'stack+5' in r:
        rt[4] = 1
    if 'stack+6' in r:
        rt[5] = 1
    if 'stack+7' in r:
        rt[6] = 1
    if 'stack+8' in r:
        rt[7] = 1
    if 'stack+9' in r:
        rt[8] = 1

    return rt


def mem_process_32(r):
    rt = [0] * 3

    if 'stack+4' in r:
        if 'mem' in r['stack+4']:
            rt[0] = 1
    if 'stack+8' in r:
        if 'mem' in r['stack+8']:
            rt[1] = 1
    if 'stack+12' in r:
        if 'mem' in r['stack+12']:
            rt[2] = 1

    return rt


import threading, os, sys, os.path

class bb_worker(threading.Thread):

    def __init__(self, thread_id, bblist):
        threading.Thread.__init__(self)
        self.ti = thread_id
        self.bl = bblist


    def run(self):
        # for each thread, we iterate from begin to the end

        print 'thread ' + str(self.ti) + " with range " + str(self.bl[0]) + " to " + str(self.bl[-1])

        log_fn = "log_" + str(self.ti) + ".log"

        os.system('rm ' + log_fn)

        res = []
        for i in self.bl:
            #print i
            res.append(str(i) + " : " + ", ".join(format(x, "10.3f") for x in feature_extract_32(i)) + "\n")
            #res.append(str(i) + " : " + ", ".join(format(x, "10.3f") for x in feature_extract_64(i)) + "\n")

        with open(log_fn, 'w') as f:
            f.writelines(res)


# input should be the list of all the candidates

def mul_process(rl):
    threads = []

    bnum = int(len(rl))
    # create ten threads as follows
    #b_range = bnum / 10
    b_range = bnum / 6
    try:
        for i in range(1, 6):
            s = (i-1) * b_range
            e = (i) * b_range

            t = bb_worker(i, rl[s: e])
            t.start()

            threads.append(t)


        #b = 9 * b_range
        b = 5 * b_range
        #t = bb_engine(10, b, e+1)
        t = bb_worker(6, rl[b:])
        t.start()
        threads.append(t)

        for t in threads:
            t.join()

    except:
        print "Error: unable to start thread"


if __name__ == '__main__':
    print feature_extract_32("../data/symbolic_formula_BB_1040")
