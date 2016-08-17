import compiler as C
import ast_visitor as AV


def depth(l):
    v = AV.MyVisitor()

    try:
        ast = C.parse(l)
        C.walk(ast, v)

        return v.m
    except:
        return 0


def process(l):
    return [depth(l)]


if __name__ == '__main__':
    print process('reg8 + 4 + 2 + 1')
    print process("Extract(31, 0, bvsdiv_i(64, Concat(0, reg2)))")
    print process('reg8 + ((4))')
