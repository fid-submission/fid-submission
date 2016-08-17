def keyword(l):
    # keyword
    # Concat
    # Extract
    #
    #c = l.count('Concat')
    #e = l.count('Extract')
    #print l
    c = float(l.count('+'))/len(l)
    e = float(l.count('42949'))/len(l)

    return [c, e]


def operator(l):
    res = []
    res.append(l.count('+'))
    res.append(l.count('-'))
    res.append(l.count('*'))
    res.append(l.count('/'))
    res.append(l.count('<<'))
    res.append(l.count('>>'))
    res.append(l.count('Concat'))
    res.append(l.count('Extract'))
    res.append(l.count('|'))
    res.append(l.count('^'))
    res.append(l.count('&'))

    r = float(sum(res))/len(l)
    return r


def tokens(l):
    import tokenize
    import StringIO

    try:
        t = tokenize.generate_tokens(StringIO.StringIO(l).readline)
        res = []
        for i in t:
            res.append(i)

        return float(len(res) - 1)/len(l)
    except:
	return 0.0


def constants(l):
    import compiler as C
    try:
        ast = C.parse(l)
        s = str(ast).count('Const')

        return float(s)/len(l)
    except:
	return 0.0


def nesting(l):
    def aux(l):
        c = 0
        m = 0
        n = len(l)
        for i in range(n):
            if l[i] == '(':
                c += 1

                if c > m:
                    m = c
            elif l[i] == ')':
                if c > 0:
                    c -= 1
                else:
                    #raise Exception("undefined")
	            return 0

        if c != 0:
            #raise Exception("undefined")
	    return 0

        return m

    return aux(l)


def process(l):
    return keyword(l) + [operator(l), tokens(l), constants(l), nesting(l)]


if __name__ == '__main__':
    print process('reg8 + 4')
    print process('reg8 + ((4))')
