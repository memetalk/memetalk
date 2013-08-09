class ASTBuilder:
    def ast(self, begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[begin:end]
        start_line = full[:begin].count("\n") + self.line_offset+1
        start_col = begin - full.rfind("\n", 0, begin)-1
        end_line = start_line + text.count("\n")
        inside_nl = text.rfind("\n", 0)
        if inside_nl == -1:
            end_col = start_col + len(text)
        else:
            end_col = len(text[inside_nl:])
        node = ASTNode(ast, self.filename, text, start_line, start_col, end_line, end_col)
        return node

    def sint_ast(self, last_begin, ast):
        end = self.parser.input.position
        full = ''.join(self.parser.input.data)
        text = full[last_begin:end]
        line = full[:last_begin].count("\n") + self.line_offset+2
        node = ASTNode(ast, self.filename, text, line, 0, line, 0)
        return node

class ASTNode(list):
    def __init__(self, lst, filename, text, start_line, start_col, end_line, end_col):
        list.__init__(self,lst)
        self.lst = lst
        self.filename = filename
        self.text = text
        self.start_line = start_line
        self.start_col = start_col
        self.end_line = end_line
        self.end_col = end_col

    def __repr__(self):
        #return str(self.lst) + ":" + str(self.line)
        return "ASTNode(" + str(self.lst) + "," +self.filename.__repr__() + "," + self.text.__repr__() + "," + \
            str(self.start_line) + "," + str(self.start_col) + "," + str(self.end_line) + ","+ str(self.end_col) + ")"
