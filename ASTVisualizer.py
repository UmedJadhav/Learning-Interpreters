# python3 ASTVisualizer.py "7 + 3 * (10 / (12 / (3 + 1) - 1))" > 
# \  ast.dot && dot -Tpng -o ast.png ast.dot

import argparse
import textwrap

from BasicInterpreter import Lexer, Parser, NodeVisitor

class ASTVisualizer(NodeVisitor):
    def __init__(self, parser):
        self.parser = parser
        self.ncount = 1
        self.dot_header = [textwrap.dedent("""\
        digraph astgraph {
          node [shape=circle, fontsize=12, fontname="Courier", height=.1];
          ranksep=.3;
          edge [arrowsize=.5]
        """)]
        self.dot_body = []
        self.dot_footer = ['}']

    def visit_Program(self, node):
        s = f'  node{self.ncount} [label="Program"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.block)

        s = f'  node{node._num} -> node{node.block._num}\n'
        self.dot_body.append(s)

    def visit_Block(self, node):
        s = f'  node{self.ncount} [label="Block"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

        for decl_node in node.declarations:
            s = f'  node{node._num} -> node{decl_node._num}\n'
            self.dot_body.append(s)

        s = f'  node{node._num} -> node{node.compound_statement._num}\n'
        self.dot_body.append(s)

    def visit_VarDecl(self, node):
        s = f'  node{self.ncount} [label="VarDecl"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.var_node)
        s = f'  node{node._num} -> node{node.var_node._num}\n'
        self.dot_body.append(s)

        self.visit(node.type_node)
        s = f'  node{node._num} -> node{node.type_node._num}\n'
        self.dot_body.append(s)

    def visit_ProcedureDecl(self, node):
        s = f' node{self.ncount} [label="ProcDecl:{node.proc_name}"]'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.block_node)
        s = f' node{node._num} -> node{node.block_node._num}\n'
        self.dot_body.append(s)

    def visit_Type(self, node):
        s = f'  node{self.ncount} [label="{node.token.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1


    def visit_Num(self, node):
        s = f'  node{self.ncount} [label="{node.token.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

    def visit_BinOp(self, node):
        s = f'  node{self.ncount} [label="{node.op.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.left)
        self.visit(node.right)

        for child_node in (node.left, node.right):
            s = f'  node{node._num} -> node{child_node._num}\n'
            self.dot_body.append(s)

    def visit_UnaryOp(self, node):
        s = f'  node{self.ncount} [label="unary {node.op.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.expr)
        s = f'  node{node._num} -> node{ node.expr._num}\n'
        self.dot_body.append(s)

    def visit_Compound(self, node):
        s = f'  node{self.ncount} [label="Compound"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        for child in node.children:
            self.visit(child)
            s = f'  node{node._num} -> node{child._num}\n'
            self.dot_body.append(s)

    def visit_Assign(self, node):
        s = f'  node{self.ncount} [label="{ node.op.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

        self.visit(node.left)
        self.visit(node.right)

        for child_node in (node.left, node.right):
            s = f'  node{node._num} -> node{child_node._num}\n'
            self.dot_body.append(s)

    def visit_Var(self, node):
        s = f'  node{self.ncount} [label="{ node.value}"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

    def visit_NoOp(self, node):
        s = f'  node{self.ncount} [label="NoOp"]\n'
        self.dot_body.append(s)
        node._num = self.ncount
        self.ncount += 1

    def gendot(self):
        tree = self.parser.parse()
        self.visit(tree)
        return ''.join(self.dot_header + self.dot_body + self.dot_footer)


def main():
    argparser = argparse.ArgumentParser(
        description='Generate an AST DOT file.'
    )
    argparser.add_argument(
        'fname',
        help='Pascal source file"'
    )
    args = argparser.parse_args()
    fname = args.fname
    text = open(fname, 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    viz = ASTVisualizer(parser)
    content = viz.gendot()
    print(content)


if __name__ == '__main__':
    main()