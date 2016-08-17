import os

class cd:
    def __init__(self, newPath):
        self.newPath = os.path.expanduser(newPath)

    def __enter__(self):
        self.savedPath = os.getcwd()
        os.chdir(self.newPath)

    def __exit__(self, etype, value, traceback):
        os.chdir(self.savedPath)


def process(fn):
    data = '"F:\\binaries\\coreutils_clang\\orig\\'
    data1 = data + fn + '"'
    os.system('idaq -A -S"F:\\funcEnum.idc" ' + data1)

    data2 = 'F:\\binaries\\coreutils_clang\\orig\\'
    os.system("mv " + data2 + "idaout.txt " + data2 + fn + ".txt")


def iter_bin():
    results = []
    data = "F:\\binaries\\coreutils_clang\\orig\\"

    counter = 1
    for i in os.listdir(data):
        if "." not in i:
            if counter == 500:
                break
            counter += 1
            print '##############', i, '################'
            process(i)
        else:
            continue

iter_bin()
