from z3 import *
import pypatt

@pypatt.transform
def translate_op(x, y, ops):
    with match(ops):
        # and
        with "&":
            return x & y
        # or
        with "^":
            return x ^ y
        # xor
        with "|":
            return x | y
        with "~":
            return ~y
        with "<<":
            return y << x
        with ">>":
            return y >> x
        with "shr":
            return LShR(y, x)
        with "*":
            return x * y
        with "div":
            return y / x
        with "+":
            return x + y
        with "-":
            return y - x
        with "ror":
            return RotateRight(y, x)
        with "rol":
            return RotateLeft(y, x)
        with "mov":
            return x
        with "movsbl":
            if x.size() == 8:
                return Concat(BitVecVal(0xffffff, 24), x)
            else:
                return x
        with "movzbl":
            if x.size() == 8:
                return Concat(BitVecVal(0, 24), x)
            else:
                return x


# w : 32; 16; 8
def process(o, dst, src, ops, w):
    if dst in o.reg_small_map and src in ['%t0', '%t1']:
        ((s1, e1), r1) = o.reg_small_map[dst]
        src_s = Extract(s1, e1, o.get_reg_equation(src))
        dst_s = Extract(s1, e1, o.get_reg_equation(r1))

        dst_s = translate_op(src_s, dst_s, ops)

        dst_s1 = Extract(31, s1+1, o.get_reg_equation(r1))
        dst_s2 = Concat(dst_s1, dst_s)

        o.set_reg_with_equation(r1, dst_s2)
    # and 23, reg32
    elif dst in o.ctx and src.isdigit():
        x = BitVecVal(int(src), w)
        dst_s = o.get_reg_equation(dst)
        dst_s_new = translate_op(x, dst_s, ops)

        o.set_reg_with_equation(dst, dst_s_new)

    # and -23, reg32
    elif dst in o.ctx and src[0] == '-' and src[1:].isdigit():
        x = BitVecVal(int(src), w)
        dst_s = o.get_reg_equation(dst)
        dst_s_new = translate_op(x, dst_s, ops)

        o.set_reg_with_equation(dst, dst_s_new)

    # and reg32, reg32
    elif src in o.ctx and dst in o.ctx:
        src_s = o.get_reg_equation(src)
        dst_s = o.get_reg_equation(dst)

        dst_s_new = translate_op(src_s, dst_s, ops)

        o.set_reg_with_equation(dst, dst_s_new)

    # and mem32, reg32
    elif dst in o.ctx and src.find('(') != -1:
        if src not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to read a non-initialized area, we got a new '+str(w)+ '-bit symbolic variable: %s' % sym
            o.mem[src] = ['r', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[src][1]]
            o.mem[src][0] = 'r'

        dst_s = o.get_reg_equation(dst)
        dst_s_new = translate_op(sym, dst_s, ops)

        o.set_reg_with_equation(dst, dst_s_new)

    # and reg32, mem32
    elif src in o.ctx and dst.find('(') != -1:
        if dst not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to write a non-initialized area, we got a new '+str(w)+'-bit symbolic variable: %s' % sym
            o.mem[dst] = ['w', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[dst][1]]
            o.mem[dst][0] = 'w'

        src_s = o.get_reg_equation(src)
        dst_s_new = translate_op(src_s, sym, ops)

        o.mem[dst] = ['w', o._push_equation(dst_s_new)]

    # and cst, mem
    elif src.isdigit() and dst.find('(') != -1:
        if dst not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to write a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[dst] = ['w', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[dst][1]]
            o.mem[dst][0] = 'w'

        x = int(src)
        x = BitVecVal(x, w)

        sym1 = translate_op(x, sym, ops)

        o.mem[dst] = ['w', o._push_equation(sym1)]

    # xxxl $_GLOBAL, %reg32
    elif dst in o.ctx and src.startswith('$'):
        dst_s = o.get_reg_equation(dst)

        dst_s_new = translate_op(BitVecVal(999, w), dst_s, ops)
        o.set_reg_with_equation(dst, dst_s_new)

    # and reg8/reg16, reg32
    elif dst in o.ctx and src in o.reg_small_map:
        ((s1,e1), r1) = o.reg_small_map[src]
        src_s = Extract(s1, e1, o.get_reg_equation(r1))

        dst_s = o.get_reg_equation(dst)

        src_s1 = Extract(31, s1+1, o.get_reg_equation(r1))

        if src_s.size() == 8:
            src_s_new = Concat(src_s1, src_s)
        if src_s.size() == 16:
            src_s_new = Concat(src_s1, src_s)

        dst_s_new = translate_op(src_s_new, dst_s, ops)

        if ops == "<<":
            dst_s_new = Extract(31, 0, dst_s_new)

        o.set_reg_with_equation(dst, dst_s_new)

    # xxxl $_GLOBAL, %reg8
    elif dst in o.reg_small_map and src.startswith('$'):
        ((s1,e1), r1) = o.reg_small_map[dst]
        dst_s = Extract(s1, e1, o.get_reg_equation(r1))
        dst_s1 = Extract(31, s1+1, o.get_reg_equation(r1))

        dst_s_new = Concat(dst_s1, translate_op(BitVecVal(999, s1 - e1 + 1), dst_s, ops))
        o.set_reg_with_equation(r1, dst_s_new)

    # and xxx, ah
    # special short registers
    elif src.isdigit() and dst in o.reg_special_map:
        ((s1,e1), r1) = o.reg_special_map[dst]

        dst_s = Extract(s1, e1, o.get_reg_equation(r1))
        dst_s0 = Extract(31, s1+1, o.get_reg_equation(r1))
        dst_s1 = Extract(e1-1, 0, o.get_reg_equation(r1))

        x = BitVecVal(int(src), s1-e1+1)

        dst_s_new = Concat(dst_s1, translate_op(x, dst_s, ops), dst_s0)

        o.set_reg_with_equation(r1, dst_s_new)
    # and xxx, reg16
    elif src.isdigit() and dst in o.reg_small_map:
        ((s1,e1), r1) = o.reg_small_map[dst]
        dst_s = Extract(s1, e1, o.get_reg_equation(r1))
        dst_s1 = Extract(31, s1+1, o.get_reg_equation(r1))

        x = BitVecVal(int(src), s1-e1+1)

        dst_s_new = Concat(dst_s1, translate_op(x, dst_s, ops))

        o.set_reg_with_equation(r1, dst_s_new)

    # and -xxx, reg16
    elif src[0] == '-' and src[1:].isdigit() and dst in o.reg_small_map:
        ((s1,e1), r1) = o.reg_small_map[dst]
        dst_s = Extract(s1, e1, o.get_reg_equation(r1))
        dst_s1 = Extract(31, s1+1, o.get_reg_equation(r1))

        x = BitVecVal(int(src), s1-e1+1)

        dst_s_new = Concat(dst_s1, translate_op(x, dst_s, ops))

        o.set_reg_with_equation(r1, dst_s_new)

    # and reg16, reg16
    elif src in o.reg_small_map and dst in o.reg_small_map:
        ((s1,e1), r1) = o.reg_small_map[src]
        src_s = Extract(s1, e1, o.get_reg_equation(r1))
        ((s2,e2), r2) = o.reg_small_map[dst]
        dst_s = Extract(s2, e2, o.get_reg_equation(r2))

        if s1 - e1 > s2 - e2:
            # mov reg16, reg8
            raise Exception('undefined behavior.')

        dst_s_new = translate_op(src_s, dst_s, ops)

        dst_s = Extract(31, s2+1, o.get_reg_equation(r2))

        dst_s_new1 = Concat(dst_s, dst_s_new)

        o.set_reg_with_equation(r2, dst_s_new1)

    # and reg16, mem16
    elif src in o.reg_small_map and dst.find('(') != -1:
        ((s1,e1), r1) = o.reg_small_map[src]
        src_s = Extract(s1, e1, o.get_reg_equation(r1))

        if dst not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to write a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[dst] = ['w', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[dst][1]]
            o.mem[dst][0] = 'w'

        if w == 32:
            dst_s_new = Concat(Extract(31, s1+1, sym), translate_op(src_s, sym, ops))
        else:
            dst_s_new = translate_op(src_s, sym, ops)

        o.mem[dst] = ['w', o._push_equation(dst_s_new)]

    # and mem16, reg16
    elif dst in o.reg_small_map and src.find('(') != -1:
        ((s1,e1), r1) = o.reg_small_map[dst]
        dst_s = Extract(s1, e1, o.get_reg_equation(r1))

        if src not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to read a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[src] = ['r', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[src]]
            o.mem[src][0] = 'r'

        dst_s_new = Concat(Extract(31, s1+1, o.get_reg_equation(r1)), translate_op(sym, dst_s, ops))

        o.set_reg_with_equation(r1, dst_s_new)

    # and -cst, mem
    elif src[0] == '-' and src[1:].isdigit() and dst.find('(') != -1:
        if dst not in o.mem:
            sym = BitVec('mem%d' % len(o.sym_variables), w)
            o.sym_variables.append(sym)
            print 'Trying to write a non-initialized area, we got a new symbolic variable: %s' % sym
            o.mem[dst] = ['w', o._push_equation(sym)]
        else:
            sym = o.equations[o.mem[dst]]
            o.mem[dst][0] = 'w'

        x = int(src)
        x = BitVecVal(x, w)

        sym1 = translate_op(x, sym, ops)

        o.mem[dst] = ['w', o._push_equation(sym1)]
    else:
        print o, src, dst
        raise Exception('This instruction is not handled.')


# mul r, src_2, t0
def process_4_process(o, dst, src, src_2, ops):
    if dst in o.ctx and src.isdigit() and src_2 in o.ctx:
        o.set_reg_with_equation(dst, translate_op(int(src), o.get_reg_equation(src_2), ops))
    elif dst in o.ctx and src in o.ctx and src_2.isdigit():
        o.set_reg_with_equation(dst, translate_op(int(src_2), o.get_reg_equation(src), ops))

    elif dst in o.ctx and src in o.ctx and src_2 in o.ctx:
        t = translate_op(o.get_reg_equation(src), o.get_reg_equation(src_2), ops)
        o.set_reg_with_equation(dst, t)
    else:
        raise Exception('This instruction is not handled.')


def process_32(o, dst, src, ops):
    process(o, dst, src, ops, 32)

def process_16(o, dst, src, ops):
    process(o, dst, src, ops, 16)

def process_8(o, dst, src, ops):
    process(o, dst, src, ops, 8)
