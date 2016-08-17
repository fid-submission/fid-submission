from z3 import *

import assign_process as AP
import arith_process as ARP
import logic_process as LP
import mem_process as MP


def is_hex(s):
    try:
        int(s, 16)
    except :
        return False
    else:
        return True


def preprocess(s):
    if '(' in s:
        return s
    else:
        if s.startswith('S_0x'):
            d = int(s[2:], 16)
            return "(" + str(d) + ")"
        elif s.startswith('$S_0x'):
            d = int(s[3:], 16)
            return str(d)
        elif '%gs:0x' in s:
            s = '(' + s + ')'
            return s
        else:
            return s


def preprocess_op(p):
    # long in 32-bit is 32-bit
    if len(p) == 4 and p.endswith('l') and 'mul' not in p:
        t = p[:-1]
        print 'change ' + p + ' to ' + t
        return t
    elif len(p) == 4 and p.startswith('i'):
        t = p[1:]
        print 'change ' + p + ' to ' + t
        return t
    else:
        return p


class EquationId(object):
    def __init__(self, id_):
        self.id = id_

    def __repr__(self):
        return 'EID:%d' % self.id



def prove(f):
    s = Solver()
    s.add(Not(f))
    if s.check() == unsat:
        return True
    return False


class Disassembler(object):


    # instructions we don't handle

    def __init__(self, bn):
        self.instrlist = []
        self.blist = ['nop', 'int', 'hlt', 'repz', 'bsr']
        self.set_list = ['seta', 'setae', 'setbe', 'setc', 'sete', 'setg',
                         'setge', 'setl', 'setle', 'setna', 'setnae', 'setnb',
                         'setnbe', 'setnc', 'setne', 'setng', 'setnl',
                         'setnle', 'setno', 'setnp', 'setns', 'setnz', 'setb']

        self.clist = ['shrl', 'shr', 'shl', 'ror', 'rol', 'sar', 'shll']

        self.dlist = ['faddp', 'fadd', 'fiadd', 'fdivrp', 'fdivp', 'fmul', 'fmulp']

        self.alist = ['div', 'mul']

        self.wlist = ['not']

        self.small_regs = [
            '%ah',
            '%al',
            '%bh',
            '%bl',
            '%ch',
            '%cl',
            '%dh',
            '%dl',
        ]

        self.cond_list = {
            'cmove'  : 'mov',
            'cmovne' : 'mov',
            'cmovae' : 'mov',
            'cmovbe' : 'mov',
            'cmovb'  : 'mov',
            'cmova'  : 'mov',
            'movw'   : 'mov',
            'movzwl' : 'mov',
            'movswl' : 'mov',
            'sbb'    :  'sub',
        }
        # pass in the basic block name
        # function name instr_list_BB_XX.txt
        fn = 'instr_list_' + bn + '.txt'
        with open(fn) as f:
            self.instrlist = f.readlines()
            self.instrlist = filter(lambda l : "#" not in l, self.instrlist)


    def change(self, items):
        items = [items[0], '1', items[1]]
        return items


    def get_next_instruction(self):
        for l in self.instrlist:
            print '-'*20, l.strip(), '-'*20
            items = l.strip().split()

            if items[0] in self.set_list:
                print "don't handle : " + l
            elif items[0] in self.blist:
                print "don't handle : " + l
            elif len(items) == 3 and items[0] in self.cond_list:
                items[0] = self.cond_list[items[0]]
                items[1] = preprocess(items[1])
                items[2] = preprocess(items[2])
                yield items
            elif items[0] in self.dlist:
                print "cannot handle float instructions"
            elif len(items) == 2 and items[0] in self.clist:
                items = self.change(items)
                items[0] = preprocess(items[0])
                items[1] = preprocess(items[1])
                yield items
            elif len(items) == 2 and items[0] in self.wlist:
                yield items
            elif len(items) == 3 and items[0] in self.wlist:
                yield items
            elif len(items) == 2 and items[0] in self.alist:
                yield items
            elif len(items) == 3:
                items[0] = preprocess_op(items[0])
                items[1] = preprocess(items[1])
                items[2] = preprocess(items[2])
                yield items
            elif items[0] in ('loadl', 'storel') and len(items) == 4:
            #elif items[0] in ('loadl',) and len(items) == 4:
                items[0] = items[0][:-1]
                items[1] = preprocess(items[1])
                items[2] = preprocess(items[2])
                items[3] = preprocess(items[3])
                yield items
            elif items[0] in ('mov', 'sub', 'mul', 'add', 'load', 'store') and len(items) == 4:
            #elif items[0] in ('mov', 'sub', 'mul', 'add', 'load') and len(items) == 4:
                items[1] = preprocess(items[1])
                items[2] = preprocess(items[2])
                items[3] = preprocess(items[3])
                yield items
            elif items[0] in ('imul',) and len(items) == 4:
                items[0] = preprocess_op(items[0])
                items[1] = preprocess(items[1])
                items[2] = preprocess(items[2])
                items[3] = preprocess(items[3])
                yield items
            else:
                # for now for a program like basename; the following opcode
                # cannot be handled
                # setne; rep; not
                print "cannot handle instruction : " + l



class SymbolicExecutionEngine(object):
    def __init__(self, bn):
        # This is the CPU context at each time
        # The value of the registers are index in the equations dictionnary


        self.ctx = {
            '%eax' : None,
            '%ebx' : None,
            '%ecx' : None,
            '%edx' : None,
            '%esi' : None,
            '%edi' : None,
            '%ebp' : None,
            '%esp' : None,
            '%eip' : None,
            '%eiz' : None,
            '%t0'  : None,
            '%t1'  : None,
        }

        self.reg_small_map = {
            #'%ah' : ((15,8), '%eax'),
            '%al' : ((7,0),  '%eax'),
            '%ax' : ((15,0), '%eax'),
            #'%bh' : ((15,8), '%ebx'),
            '%bl' : ((7,0),  '%ebx'),
            '%bx' : ((15,0), '%ebx'),
            #'%ch' : ((15,8), '%ecx'),
            '%cl' : ((7,0),  '%ecx'),
            '%cx' : ((15,0), '%ecx'),
            #'%dh' : ((15,8), '%edx'),
            '%dl' : ((7,0),  '%edx'),
            '%dx' : ((15,0), '%edx'),
            '%di' : ((15,0), '%edi'),
            '%si' : ((15,0), '%esi'),
            '%bp' : ((15,0), '%ebp'),
            '%sp' : ((15,0), '%esp'),
        }

        self.reg_special_map = {
            '%ah': ((15, 8), '%eax'),
            '%bh': ((15, 8), '%ebx'),
            '%ch': ((15, 8), '%ecx'),
            '%dh': ((15, 8), '%edx'),
        }

        self.bn = bn

        # Our disassembler
        self.disass = Disassembler(bn)

        # This is the memory that can be used by the instructions to save temporary values/results
        self.mem = {}


        # Each equation must have a unique id
        self.idx = 0

        # The symbolic variables will be stored there
        self.sym_variables = []

        # Each equation will be stored here
        self.equations = {}

        # Number of instructions emulated
        self.ninstrs = 0

        self.set_reg_with_equation('%eax', BitVec('reg%d' % 1, 32))
        self.set_reg_with_equation('%ebx', BitVec('reg%d' % 2, 32))
        self.set_reg_with_equation('%ecx', BitVec('reg%d' % 3, 32))
        self.set_reg_with_equation('%edx', BitVec('reg%d' % 4, 32))
        self.set_reg_with_equation('%esi', BitVec('reg%d' % 5, 32))
        self.set_reg_with_equation('%edi', BitVec('reg%d' % 6, 32))
        self.set_reg_with_equation('%ebp', BitVec('reg%d' % 7, 32))
        self.set_reg_with_equation('%esp', BitVec('reg%d' % 8, 32))
        self.set_reg_with_equation('%eip', BitVec('reg%d' % 9, 32))
        self.set_reg_with_equation('%eiz', BitVec('reg%d' % 10, 32))
        self.set_reg_with_equation('%t0', BitVec('t%d' % 0, 32))
        self.set_reg_with_equation('%t1', BitVec('t%d' % 1, 32))
        self.sym_variables.append(BitVec('reg%d' % 1, 32))
        self.sym_variables.append(BitVec('reg%d' % 2, 32))
        self.sym_variables.append(BitVec('reg%d' % 3, 32))
        self.sym_variables.append(BitVec('reg%d' % 4, 32))
        self.sym_variables.append(BitVec('reg%d' % 5, 32))
        self.sym_variables.append(BitVec('reg%d' % 6, 32))
        self.sym_variables.append(BitVec('reg%d' % 7, 32))
        self.sym_variables.append(BitVec('reg%d' % 8, 32))
        self.sym_variables.append(BitVec('reg%d' % 9, 32))
        self.sym_variables.append(BitVec('reg%d' % 10, 32))
        self.sym_variables.append(BitVec('t%d' % 0, 32))
        self.sym_variables.append(BitVec('t%d' % 1, 32))


    def _check_if_reg32(self, r):
        '''XXX: make a decorator?'''
        return r.lower() in self.ctx

    def _push_equation(self, e):
        idx = EquationId(self.idx)
        self.equations[idx] = e
        self.idx += 1
        return idx

    def set_reg_with_equation(self, r, e):
        if self._check_if_reg32(r) == False:
            return

        self.ctx[r] = self._push_equation(e)

    def get_reg_equation(self, r):
        if self._check_if_reg32(r) == False:
            return

        if isinstance(self.ctx[r], EquationId):
            return self.equations[self.ctx[r]]
        else:
            return self.ctx[r]


    def execute(self):
        # we use at&t syntax
        for items in self.disass.get_next_instruction():
            mnemonic = ""
            src = ""
            dst = ""
            src_2 = ""
            if len(items) == 2:
                mnemonic, dst = items
            elif len(items) == 3:
                mnemonic, src, dst = items
            elif len(items) == 4:
                mnemonic, src, src_2, dst = items

            print dst

            if (self.ninstrs % 5000) == 0 and self.ninstrs > 0:
                print '%d instructions, %d equations so far...' % (self.ninstrs, len(self.equations))

            if mnemonic == 'mov' or mnemonic == 'movl':
                AP.mov_process(self, dst, src)
            elif mnemonic == 'movb':
                AP.movb_process(self, dst, src)
            elif mnemonic == 'movzbl':
                AP.movzbl_process(self, dst, src)
            elif mnemonic == 'movsbl':
                AP.movsbl_process(self, dst, src)
            elif mnemonic == 'xchg':
                AP.xchg_process(self, dst, src)
            elif mnemonic == 'not':
                LP.not_process(self, dst)
            elif mnemonic == 'and':
                LP.and_process(self, dst, src)
            elif mnemonic == 'xor':
                LP.xor_process(self, dst, src)
            elif mnemonic == 'orl':
                LP.or_process(self, dst, src)
            elif mnemonic == 'or':
                LP.or_process(self, dst, src)
            elif mnemonic == 'shr':
                ARP.shr_process(self, dst, src)
            elif mnemonic == 'shl':
                ARP.shl_process(self, dst, src)
            elif mnemonic == 'ror':
                ARP.ror_process(self, dst, src)
            elif mnemonic == 'rol':
                ARP.rol_process(self, dst, src)
            elif mnemonic == 'mul' and len(items) == 4:
                ARP.mul_4_process(self, dst, src, src_2)
            elif mnemonic == 'mul' and len(items) == 3:
                ARP.mul_3_process(self, dst, src)
            elif mnemonic == 'mul' and len(items) == 2:
                ARP.mul_2_process(self, dst)
            elif mnemonic == 'add' and len(items) == 4:
                ARP.add_4_process(self, dst, src, src_2)
            elif mnemonic == 'add' and len(items) == 3:
                ARP.add_process(self, dst, src)
            elif mnemonic == 'sub' and len(items) == 4:
                ARP.sub_4_process(self, dst, src, src_2)
            elif mnemonic == 'sub' and len(items) == 3:
                ARP.sub_process(self, dst, src)
            elif mnemonic == 'div' and len(items) == 2:
                ARP.div_2_process(self, dst)
            elif mnemonic == 'sar':
                ARP.sar_process(self, dst, src)
            elif mnemonic == 'load':
                MP.load_process(self, dst, src, src_2)
            elif mnemonic == 'store':
                MP.store_process(self, dst, src, src_2)
            else:
                print mnemonic, src, dst
                raise Exception('This instruction is not handled.')

            self.ninstrs += 1

    def run(self):
        try:
            self.execute()
        except Exception as s:
            print s
            print "symbolic execution of " + self.bn + " failed!"


    def _simplify_additions(self, eq):
        # The two expressions are equivalent ; we got a simplification!
        if prove(Sum(self.sym_variables) == eq):
            return Sum(self.sym_variables)

        return eq


    def get_reg_equation_simplified(self, reg):
        eq = self.get_reg_equation(reg)
        if isinstance(self.ctx[reg], EquationId):
            #eq = simplify(self._simplify_additions(eq))
            eq = simplify(eq)
            #eq = eq
        return eq



def retrieving (sym, bn):
    res = []

    for r in sym.ctx:
        if sym.ctx[r]:
            if r != '%t1' and r != '%t0':
                state = sym.get_reg_equation_simplified(r)
                res.append(r + " = " + str(state) + "\n")
        else:
            res.append(r + ' = undefined\n')


    res1 = MP.inspect_stack(sym)
    res = res + res1

    fn = 'symbolic_formula_' + bn
    with open (fn, 'w') as f:
        f.writelines(res)



def main(bn):
    sym = SymbolicExecutionEngine(bn)
    sym.run()
    retrieving (sym, bn)
    return 1

if __name__ == '__main__':
    bn = sys.argv[1]
    main(bn)
