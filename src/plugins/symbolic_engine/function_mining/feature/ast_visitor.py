from zss import simple_distance, Node
import compiler


class MyVisitor:
    def __init__(self):
        self.ast = None
        self.depth = 0
        self.m = 0

    def visitName(self, t):
        self.depth += 1
        if self.m < self.depth:
            self.m = self.depth

        self.depth -= 1
        return Node(str(t.name))

    def visitConst(self, t):
        self.depth += 1
        if self.m < self.depth:
            self.m = self.depth

        self.depth -= 1
        return Node(str(t.value))

    def visitSub(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node("-")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node("-")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node("-")
                            .addkid(r))

        #return self.ast

    def visitAdd(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node("+")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node("+")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node("+")
                            .addkid(r))


    def visitMul(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node("*")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node("*")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node("*")
                            .addkid(r))


    def visitDiv(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node("/")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node("/")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node("/")
                            .addkid(r))


    def visitLeftShift(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node("<<")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node("<<")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node("<<")
                            .addkid(r))


    def visitRightShift(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        l = self.visit(t.left)
        r = self.visit(t.right)

        self.depth -= 1

        if l and r:
            self.ast.addkid(Node(">>")
                            .addkid(l)
                            .addkid(r))
        elif l:
            self.ast.addkid(Node(">>")
                            .addkid(l))
        elif r:
            self.ast.addkid(Node(">>")
                            .addkid(r))


    def visitNot(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        res = self.visit(t.expr)

        self.depth -= 1

        if res:
            self.ast.addkid(Node("not").addkid(res))
        else:
            self.ast.addkid(Node("not").addkid(str(t.expr)))


    def visitInvert(self, t):
        if self.m < self.depth:
            self.m = self.depth
        self.depth += 1

        res = self.visit(t.expr)

        self.depth -= 1

        if not (res is None):
            self.ast.addkid(Node("invert").addkid(res))
        elif not (t.expr is None):
            self.ast.addkid(Node("invert").addkid(str(t.expr)))
        else:
            self.ast.addkid(Node("invert"))


            #return self.ast
    def visitCallFunc(self, t):
        if t.node and t.args:
            temp = Node("callFunc").addkid(Node(str(t.node)))

            self.depth += 1

            for a in t.args:
                i = self.visit(a)
                if i:
                    temp.addkid(Node((str(i))))

            self.depth -= 1

            return temp

        #elif t.node:
        #    self.ast.addkid(Node("callFunc")
        #                    .addkid(t.node))

        #elif t.args:
        #    self.ast.addkid(Node("callFunc"))
        #    for a in t.args:
        #        self.ast.addkid(a)


    def visitAnd(self, t):
        if self.m < self.depth:
            self.m = self.depth

        temp = None
        self.depth += 1

        for i in t:

            i1 = self.visit(i)

            if i1 and temp is None:
                temp = Node("and").addkid(i1)
            elif i1 and temp:
                temp.addkid(i1)

        self.ast.addkid(temp)

        self.depth -= 1


    def visitBitor(self, t):
        if self.m < self.depth:
            self.m = self.depth

        temp = None
        self.depth += 1

        for i in t:
            i1 = self.visit(i)

            if i1 and temp is None:
                temp = Node("|").addkid(i1)
            elif i1 and temp:
                temp.addkid(i1)

        self.ast.addkid(temp)

        self.depth -= 1


    def visitBitxor(self, t):
        if self.m < self.depth:
            self.m = self.depth

        temp = None
        self.depth += 1

        for i in t:
            i1 = self.visit(i)

            if i1 and temp is None:
                temp = Node("^").addkid(i1)
            elif i1 and temp:
                temp.addkid(i1)

        self.ast.addkid(temp)

        self.depth -= 1

    def visitBitand(self, t):
        if self.m < self.depth:
            self.m = self.depth

        temp = None
        self.depth += 1

        for i in t:
            i1 = self.visit(i)

            if i1 and temp is None:
                temp = Node("&").addkid(i1)
            elif i1 and temp:
                temp.addkid(i1)

        self.ast.addkid(temp)
        self.depth -= 1


    def visitDiscard(self, t):
        self.depth += 1

        return self.visit(t.expr)

    def visitStmt(self, t):
        temp = None
        self.ast = Node("Stmt")
        self.depth += 1

        for i in t:
            i1 = self.visit(i)
            if i1 and temp is None:
                temp = Node("Discard").addkid(i1)
            elif i1 and temp:
                temp.addkid(i1)

        if temp:
            self.ast.addkid(temp)
