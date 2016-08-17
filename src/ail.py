import sys, os

def process(f, times):
    for i in xrange(0,times):
        print ("########## iteration round "+str(i+1) + " begin ! ###########")
        os.system("cp "+f+" func_discover/")
        os.system("python func_discover/func_addr.py func_discover/"+f)
        #os.system('rm trampoline')
        #os.system('rm trampoline.o')
        #os.system('rm fadd1.txt')
        #os.system('rm union_*.txt')
        os.system('rm useless_func.info')
        os.system("rm final_data.s")
        #os.system("rm final_trampoline.s")
        if i > 0:
   	    os.system("python useless_func_discover.py " + f)
        os.system("strip " + f)
        os.system("python main_discover.py " + f)
        os.system("./init.native " + f)
        os.system("cp final_data.s final_data.s.bak")
	if not os.path.isfile("final.s"):
	    print "error"
	    break
        os.system("python post_process_data.py")
        os.system('cat final_data.s >> final.s')
        os.system("cp final.s final.s." + str(i))
        #os.system('cat final_trampoline.s >> final.s')
        os.system("python compile_process.py")
        #if i > 0:
        #    os.system("python instr_prune.py " + f + " " + str(i+1))
        #os.system("gcc -Tld_gobmk.sty final.s -lm -m32")
        os.system("gcc final.s -m32")
        os.system("cp a.out " + f)
        print ("########## iteration round "+str(i+1) + " finish ! ##########")
        print ""

if __name__ == '__main__':
    if len(sys.argv) == 3:
        f = sys.argv[1]
        times = int(sys.argv[2])
        os.system("cp test/" + f + " .")
        #os.system("cp test/spec_bin/" + f + " .")
        process(f, times)
    else:
        print "usage: python ail.py binary times"
