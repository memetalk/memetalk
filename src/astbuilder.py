class ASTBuilder:
    def ast(self, begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[begin:end]
        line = full[:begin].count("\n") + self.line_offset
        node = ASTNode(ast, self.filename, text, line+1)
        return node

    def sint_ast(self, last_begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[last_begin:end]
        line = full[:last_begin].count("\n") + self.line_offset+1
        node = ASTNode(ast, self.filename, text, line+1)
        return node

class ASTNode(list):
    def __init__(self, lst, filename, text, line):
        list.__init__(self,lst)
        self.lst = lst
        self.filename = filename
        self.text = text
        self.line = line
    def __repr__(self):
        #return str(self.lst) + ":" + str(self.line)
        return "ASTNode(" + str(self.lst) + "," +self.filename.__repr__() + "," + self.text.__repr__() + "," + str(self.line) + ")"
