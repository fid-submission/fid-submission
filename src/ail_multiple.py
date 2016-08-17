import sys, os


def check_exe():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "LSB shared object" in lines[0]:
        return False
    else:
        return True

def check_32():
    lines = []
    with open("elf.info") as f:
        lines = f.readlines()
    if "32-bit" in lines[0]:
        return True
    else:
        return False



def reassemble():
    if check_32() == True:
        # 32-bit binary
        os.system('gcc final.s -lm -lrt -lpthread -lcrypt -m32')
    else:
        # 64-bit binary
        os.system('gcc final.s -lm -lrt -lpthread -lcrypt')

def check_md5():
    return True
    os.system('./a.out 5m.img')
    os.system('md5sum 5m.img.bz2 > md5_tmp')
    os.system('bzip2 -d 5m.img.bz2')
    with open('md5_tmp') as f:
        lines = f.readlines()
        v = lines[0].split()[0]
        return v == 'bb6402506d88740b49d9cfd1a454d188'



def process(f, times):
    for i in xrange(0,times):
        print ("########## iteration round "+str(i+1) + " " + f + " begin ! ###########")
	os.system("rm final_*.txt")
        os.system("cp "+f+" func_discover/")
        os.system("python func_discover/func_addr.py func_discover/"+f + " " + str(i))
        #os.system('rm trampoline')
        #os.system('rm trampoline.o')
        #os.system('rm fadd1.txt')
        #os.system('rm union_*.txt')
        os.system("rm final_data.s")
        os.system('rm useless_func.info')
        if i > 0:
   	    os.system("python useless_func_discover.py " + f)
        #os.system("rm final_trampoline.s")
	os.system('echo \"' + str(i) + '\" > count.txt')
        #os.system("objdump -Dr -j .text " + f + " > dump." + str(i))
        os.system("strip " + f)
        os.system("python main_discover.py " + f)
        os.system("./init.native " + f)
        #os.system("cp final_data.s final_data.s.bak")
	if not os.path.isfile("final.s"):
	    print "error"
	    break
        os.system("python post_process_data.py")
        #os.system('echo ".section .eh_frame" >> final_data.s')
        #os.system('cat eh_frame_split.info >> final_data.s')
        #os.system('echo ".section .eh_frame_hdr" >> final_data.s')
        #os.system('cat eh_frame_hdr_split.info >> final_data.s')
        os.system('cat final_data.s >> final.s')
        #os.system("cp final.s final.s." + str(i))
        #os.system('cat final_trampoline.s >> final.s')
        #os.system("python gobmk_sub.py")
        os.system("python compile_process.py")
        #if i > 0:
        #    os.system("python instr_prune.py " + f + " " + str(i+1))
        os.system("python label_adjust.py")
        #os.system("gcc -Tld_gobmk.sty final.s -lm -m32")
        #os.system("gcc final.s -lm -m32")
        #os.system('python post_process_logger.py')
        reassemble()
        #os.system("gcc final.s -m32")
        if check_md5() == False:
            print "check sum error"
            break
        #os.system("cp a.out " + f)
        #os.system("cp a.out test_folder_" + f+"/"+f + "." + str(i+1))

	#with open('final_d2c_label.txt') as f1:
	#    l = len(f1.readlines())
        #    if i == 0:
	#        assert(l  == 2747)
        #    else:
	#        assert(l  == 2742)


        print ("########## iteration round "+str(i+1) + " finish ! ##########")
        print ""


if __name__ == '__main__':
    if len(sys.argv) == 3:
        f = sys.argv[1]
        times = int(sys.argv[2])
        #os.system("cp test/" + f + " .")
        #os.system("cp test/" + f + " .")
        os.system("cp test/spec_32_normal/" + f + " .")
        #os.system("cp test/spec_64_normal/" + f + " .")
        #os.system("cp test/coreutils_32_normal/" + f + " .")
        #os.system("cp test/spec_32_llvm_normal/" + f + " .")
        #os.system("cp test/real_32_normal/" + f + " .")
        #os.system("rm -rf test_folder_" + f)
        os.system('rm useless_func.info')
        os.system('rm faddr_old.txt')
        #os.system("mkdir test_folder_" + f)
        #os.system("cp test/spec_bin/" + f + " .")
        process(f, times)
    else:
        print "usage: python ail.py binary times"
