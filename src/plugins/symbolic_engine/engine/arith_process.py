from z3 import *
import utility as U

def shl_process(o, dst, src):
    U.process_32(o, dst, src, "<<")

def shr_process(o, dst, src):
    U.process_32(o, dst, src, "shr")

def sar_process(o, dst, src):
    U.process_32(o, dst, src, ">>")

def mul_4_process(o, dst, src, src_2):
    U.process_4_process(o, dst, src, src_2, "*")

def mul_3_process(o, dst, src):
    U.process_32(o, dst, src, "*")

def mul_2_process(o, dst):
    # mul %edx
    # EDX:EAX = EAX * EDX

    if dst in o.ctx:
        e_eax = o.get_reg_equation('%eax')
        e_edx = o.get_reg_equation('%edx')
        e_r = o.get_reg_equation(dst)

        v = e_eax * e_r

        #o.set_reg_with_equation('%edx', Extract(63, 32, v))
        o.set_reg_with_equation('%eax', v)

    else:
        raise Exception('This encoding of "mul" is not handled.')



def div_2_process(o, dst):
    # div %edx

    if dst in o.ctx:
        e_eax = o.get_reg_equation('%eax')
        e_edx = o.get_reg_equation('%edx')
        e_r   = o.get_reg_equation(dst)

        v = Concat(e_edx, e_eax)

        v1 = Concat(BitVecVal(0, 32), e_r)

        o.set_reg_with_equation('%eax', Extract(31, 0, v / v1))
        o.set_reg_with_equation('%edx', Extract(31, 0, URem(v, v1)))
    else:
        raise Exception('This encoding of "div" is not handled.')


def add_4_process(o, dst, src, src_2):
    U.process_4_process(o, dst, src, src_2, "+")

def add_process(o, dst, src):
    U.process_32(o, dst, src, "+")

def addb_process(o, dst, src):
    U.process_8(o, dst, src, "+")

def sub_4_process(o, dst, src, src_2):
    U.process_4_process(o, dst, src, src_2, "-")

def sub_process(o, dst, src):
    U.process_32(o, dst, src, "-")

def subb_process(o, dst, src):
    U.process_8(o, dst, src, "-")

def ror_process(o, dst, src):
    U.process_32(o, dst, src, "ror")

def rol_process(o, dst, src):
    U.process_32(o, dst, src, "rol")
