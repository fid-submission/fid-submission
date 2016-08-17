import os


# iterate

class cd:
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)


def parse_name(fn):
    items = fn.split('_')
    compiler = items[0]
    version = items[2]
    opt = items[3]
    fn = items[4]

    return (compiler, version, opt, fn)


def report(res, i0, j0):
    print res
    tp_t = 0
    fp_t = 0
    fn_t = 0

    p = 0
    rr = 0

    fl = []

    for (p_1, r_1, f1_1, tp_1, fp_1, fn_1, p_2, r_2, f1_2, tp_2, fp_2, fn_2, i) in res:
    	try:
            #print "| ", i, " |  ", p_1.strip(), " | ", r_1.strip(), "|", f1_1.strip(), "|"
            fl.append("| " + str(i) +  " |  " +  p_1.strip() + " | " + r_1.strip() + "|" + f1_1.strip() + "|" + "\n" )
            tp_t += int(tp_1)
            fp_t += int(fp_1)
            fn_t += int(fn_1)
    	except :
            print " exception on ", i

    p = float(tp_t) / (tp_t + fp_t)
    rr = float(tp_t) / (tp_t + fn_t)
    #print "| average |  ", p, " | ", rr, "|", (2*p*rr/(p+rr)), "|"
    fl.append("| average |  " + str(p) + " | " + str(rr) + "|" + str(2*p*rr/(p+rr)) + "|" + "\n")

    with open("llvm_O" + i0 + j0 + "-test_out.org", "w") as f:
	f.writelines(fl)

    tp_t = 0
    fp_t = 0
    fn_t = 0

    for (p_1, r_1, f1_1, tp_1, fp_1, fn_1, p_2, r_2, f1_2, tp_2, fp_2, fn_2, i) in res:
    	try:
            #print "| ", i, " |  ", p_2.strip(), " | ", r_2.strip(), "|", f1_2.strip(), "|"
            fl.append("| " + str(i) +  " |  " +  p_2.strip() + " | " + r_2.strip() + "|" + f1_2.strip() + "|" + "\n" )
            tp_t += int(tp_2)
            fp_t += int(fp_2)
            fn_t += int(fn_2)
    	except :
            print " exception on ", i

    p = float(tp_t) / (tp_t + fp_t)
    rr = float(tp_t) / (tp_t + fn_t)
    #print "| average |  ", p, " | ", rr, "|", (2*p*rr/(p+rr)), "|"
    fl.append("| average |  " + str(p) + " | " + str(rr) + "|" + str(2*p*rr/(p+rr)) + "|" + "\n")

    with open("llvm_O" + i0 + j0 + "-test_out-combi.org", "w") as f:
	f.writelines(fl)


def record(p, r, f1, i):
#    (c, v, o, f) = parse_name(i)

    if p == -1:
        r = i + ', ' + ' failed\n'
    else:
        #r = i + ', ' + str(p).strip() + ', ' + str(r).strip() + ', ' + str(f1).strip() + '\n'
        r = "| " + i + '| ' + str(p).strip() + '| ' + str(r).strip() + '| ' + str(f1).strip() + '|\n'

    return r


def iter_bins2():
    b = "/data/x86_binaries_uroboros/llvm_O"
    #sub_f = ['coreutils']
    res = []


    for i0 in ["1"]:
        #for j0 in ['instr_sub', 'cfg_flatten', 'opaque', 'mix1',
        #           'mix2', 'mix3']:
        for j0 in ['cfg_flatten']:

	    counter = 0
            res = []
            d = b + i0 + "/" + j0
            with cd(d):
                jounter = 1
                #for i in ["icc_coreutils_32_O0_od"]:
                for i in os.listdir('./'):
                    #try:
                    if counter == 2:
                        break
                    #elif "O0" in i and "gcc" in i and not (i.endswith("ar")):
                    #elif "O2" in i and "gcc" in i:
                    #	print i
                    else:
                        counter += 1
                        print counter
                        print '############## ', i, ' ################'
                        #(p, r, f1, tp, fp, fn) = analysis(d, i)
                        t = analysis(d, i)

                	#t1.append(i)
                	#res.append(t1)
                        res.append(i + " : " + t + "\n")

            #report(res, i0, j0)
            with open(i0 + j0 + 'distribution.res', 'w') as f:
                f.writelines(res)



def iter_bins():
    d = "/home/x86-binaries/elf/"
    sub_f = ['binutils', 'findutils']
    #sub_f = ['coreutils']
    res = []

    for sf in sub_f:
        p = d + sf

        with cd(p):
            for i in os.listdir('./'):
                if "gcc_" in i or "icc_" in i:

                    try:
                        print '##############', i, '################'
                        (precision, recall, f1) = analysis(p, i)
                    except:
                        (precision, recall, f1) = (-1, -1, -1)

                    res.append(record(precision, recall, f1, i))


    with open('test.res', 'w') as f:
        f.writelines(res)



# analysis

def cp_large():
    for i in os.listdir('./'):
        if "symbolic_formula_BB_" in i:
            os.system('cp ' + i + ' plugins/symbolic_engine/function_mining/data/')

def produce_black_list():
	blacklist = []
	lines = []
	with open("fb2") as f:
	    lines = f.readlines()


	res = []
	for l in lines:
	    items = l.split()
	    fn = items[1]
	    if fn in blacklist:
	    # found one!
	        res.append(items[0])

	#with open("fb2", "w") as f:
	#     f.writelines(res)
	return res



def adjust_gd_by_bl (res):
        res = map(lambda r : ".globl S_0x" + r[1:].upper(), res)
	print res
	lines = []
	with open("fb1") as f:
	    lines = f.readlines()

	bbl = []
	for i in range(len(lines)):
	    l = lines[i]
	    if l.strip() in res:
		print l
		lines[i] = ""
		bbl.append(lines[i-1])
		lines[i-1] = ""


	with open("fb1", 'w') as f:
	    f.writelines(lines)

	return bbl


def adjust_test_by_bl (bbl):
        bbl = map(lambda r : "symbolic_formula_" + r[:-2], bbl)

	for b in bbl:
	    print b
	    os.system("rm " + b)



def analysis(p, i):
    full_path = p + '/' + i
    print full_path
    uroboros = '/data/src/'

    mining = uroboros + '/plugins/symbolic_engine/function_mining/data'

    with cd(uroboros):
        os.system('find . -name "symbolic_formula_BB_*" -print0 | xargs -0 rm -f')
        os.system('find . -name "instr_list_BB_*" -print0 | xargs -0 rm -f')



        n = -1
        os.system("python uroboros.py " + full_path + " > t.output")

        lines = []
        with open('t.output') as f:
            lines = f.readlines()

        for l in lines:
            if "bb_number" in l:
                n = int(l.split(':')[1])

        os.system('python plugins/engine_starter.py ' + str(n-1))

        os.system('grep -B 2 "globl" final.s > fb1')
	os.system("cp " + full_path + " .")
        os.system('objdump -Dr -j .text ' + i + ' > t.dis')
        os.system('grep ">:" t.dis > fb2')
	res = produce_black_list()
	bbl = adjust_gd_by_bl (res)
        cp_large()
        os.system('cp fb1 plugins/symbolic_engine/function_mining/data')

        p_1 = -1
        r_1 = -1
        f1_1 = -1
        tp_1 = -1
        fp_1 = -1
        fn_1 = -1

        p_2 = -1
        r_2 = -1
        f1_2 = -1
        tp_2 = -1
        fp_2 = -1
        fn_2 = -1

        with cd(mining):
	    print n
            os.system('python analysis.py ' + str(n-1) + ' > ' + ' t.output')

            lines = []

            with open('t.output') as f:
                lines = f.readlines()
	        print lines
		for l in lines:
                    if "distribution" in l: 
                        s = l.split(':')[1].strip()

        return s


iter_bins2()
