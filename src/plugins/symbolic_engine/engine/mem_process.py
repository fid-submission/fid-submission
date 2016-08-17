"""
this module process memory load and store operation.
"""
from z3 import *


def load_process(o, dst, src, src2):
    if dst in o.ctx and src in o.ctx and src2.isdigit():
        # load %esp 4 %t0

        s = o.get_reg_equation(src) + int(src2)

        # esp - 4 + 8 = esp + 4
        ss = simplify(s)

        if str(ss) not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), 32)
            o.sym_variables.append(sym)
            print 'Trying to load a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[str(ss)] = ('r', o._push_equation(sym))
        else:
            sym = o.equations[o.mem[str(ss)][1]]
            o.mem[str(ss)][0] = 'r'

        o.set_reg_with_equation(dst, sym)
    elif dst in o.ctx and src in o.ctx and src2[0] == '-' and src2[1:].isdigit():
        # load %esp 4 %t0

        s = o.get_reg_equation(src) + int(src2)

        # esp - 4 + 8 = esp + 4
        ss = simplify(s)

        if str(ss) not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), 32)
            o.sym_variables.append(sym)
            print 'Trying to load a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[str(ss)] = ('r', o._push_equation(sym))
        else:
            sym = o.equations[o.mem[str(ss)][1]]
            o.mem[str(ss)] = 'r'

        o.set_reg_with_equation(dst, sym)
    else:
        raise Exception('This encoding of "load" is not handled.')


def store_process(o, dst2, src, dst):
    if dst in o.ctx and (dst2.isdigit() or dst2[1:].isdigit()) and src in o.ctx:
        # store %t0 %ebp -4

        s = o.get_reg_equation(dst) + int(dst2)

        # esp - 4 + 8 = esp + 4
        ss = simplify(s)

        src_s = o.get_reg_equation(src)

        o.mem[str(ss)] = ('w', o._push_equation(src_s))
    else:
        raise Exception('This encoding of "store" is not handled.')


##############################################################################

# this method can be called after processing.
# it summarizes stack access behavoir

# we need to check esp + 4; esp + 8; esp + 12; esp + 16
#
# In other words, we need to look at reg8 + 4, reg8 + 8 and reg8 + 12

# note that after the execution, the value of register esp could be changed,
# but that doesn't matter

def inspect_stack(o):

    #print o.mem

    res = []

    if "4 + reg8" in o.mem and o.mem['4 + reg8'][0] == 'r':
        res.append('stack+4 = ' + str(o.equations[o.mem["4 + reg8"][1]]) + '\n')
    if "8 + reg8" in o.mem and o.mem['8 + reg8'][0] == 'r':
        res.append('stack+8 = ' + str(o.equations[o.mem["8 + reg8"][1]]) + '\n')
    if "12 + reg8" in o.mem and o.mem['12 + reg8'][0] == 'r':
        res.append('stack+12 = ' + str(o.equations[o.mem["12 + reg8"][1]]) + '\n')

    return res
