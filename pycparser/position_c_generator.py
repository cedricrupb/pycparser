#------------------------------------------------------------------------------
# pycparser: c_generator.py
#
# C code generator from pycparser AST nodes.
#
# Eli Bendersky [https://eli.thegreenplace.net/]
# License: BSD
#------------------------------------------------------------------------------
from pycparser import c_ast


class PositionCGenerator(object):
    """ Uses the same visitor pattern as c_ast.NodeVisitor, but modified to
        return a value from each visit method, using string accumulation in
        generic_visit.
    """
    def __init__(self, reduce_parentheses=False):
        """ Constructs C-code generator

            reduce_parentheses:
                if True, eliminates needless parentheses on binary operators
        """
        # Statements start with indentation of self.indent_level spaces, using
        # the _make_indent method.
        self.indent_level = 0
        self.reduce_parentheses = reduce_parentheses

    def generate(self, node):
        code = []
        map = {}
        self.visit(node, code, map)
        return ''.join(code), map

    def _make_indent(self, c):
        c += [' '] * self.indent_level

    def visit(self, node, code, map):
        method = 'visit_' + node.__class__.__name__
        start = len(code)
        getattr(self, method, self.generic_visit)(node, code, map)
        end = len(code)
        map[node] = (start, end)

    def generic_visit(self, node, code, map):
        if node is not None:
            for c_name, c in node.children():
                self.visit(c, code, map)

    def visit_Constant(self, n, c, m):
        c += list(str(n.value))

    def visit_ID(self, n, c, m):
        c += list(str(n.name))

    def visit_Pragma(self, n, c, m):
        ret = '#pragma'
        if n.string:
            ret += ' ' + n.string
        c += list(ret)

    def visit_ArrayRef(self, n, c, m):
        self._parenthesize_unless_simple(n.name, c, m)
        c.append('[')
        self.visit(n.subscript, c, m)
        c.append(']')

    def visit_StructRef(self, n, c, m):
        self._parenthesize_unless_simple(n.name, c, m)
        c += list(str(n.type))
        self.visit(n.field, c, m)

    def visit_FuncCall(self, n, c, m):
        self._parenthesize_unless_simple(n.name, c, m)
        c.append('(')
        self.visit(n.args, c, m)
        c.append(')')

    def visit_UnaryOp(self, n, c, m):
        if n.op in ('sizeof', "__alignof__", "__alignof"):
            # Always parenthesize the argument of sizeof since it can be
            # a name.
            c += list(str(n.op))
            c.append('(')
            self.visit(n.expr, c, m)
            c.append(')')
        else:
            if n.op not in ('p++', 'p--'):
                c += list(str(n.op))
            self._parenthesize_unless_simple(n.expr, c, m)
            if n.op == 'p++':
                c += list('++')
            elif n.op == 'p--':
                c += list('--')

    # Precedence map of binary operators:
    precedence_map = {
        # Should be in sync with c_parser.CParser.precedence
        # Higher numbers are stronger binding
        '||': 0,  # weakest binding
        '&&': 1,
        '|': 2,
        '^': 3,
        '&': 4,
        '==': 5, '!=': 5,
        '>': 6, '>=': 6, '<': 6, '<=': 6,
        '>>': 7, '<<': 7,
        '+': 8, '-': 8,
        '*': 9, '/': 9, '%': 9  # strongest binding
    }

    def visit_BinaryOp(self, n, c, m):
        # Note: all binary operators are left-to-right associative
        #
        # If `n.left.op` has a stronger or equally binding precedence in
        # comparison to `n.op`, no parenthesis are needed for the left:
        # e.g., `(a*b) + c` is equivalent to `a*b + c`, as well as
        #       `(a+b) - c` is equivalent to `a+b - c` (same precedence).
        # If the left operator is weaker binding than the current, then
        # parentheses are necessary:
        # e.g., `(a+b) * c` is NOT equivalent to `a+b * c`.
        self._parenthesize_if(
            n.left,
            lambda d: not (self._is_simple_node(d) or
                      self.reduce_parentheses and isinstance(d, c_ast.BinaryOp) and
                      self.precedence_map[d.op] >= self.precedence_map[n.op]),
            c, m
        )
        c.append(' ')
        c += list(str(n.op))
        c.append(' ')
        # If `n.right.op` has a stronger -but not equal- binding precedence,
        # parenthesis can be omitted on the right:
        # e.g., `a + (b*c)` is equivalent to `a + b*c`.
        # If the right operator is weaker or equally binding, then parentheses
        # are necessary:
        # e.g., `a * (b+c)` is NOT equivalent to `a * b+c` and
        #       `a - (b+c)` is NOT equivalent to `a - b+c` (same precedence).
        self._parenthesize_if(
            n.right,
            lambda d: not (self._is_simple_node(d) or
                      self.reduce_parentheses and isinstance(d, c_ast.BinaryOp) and
                      self.precedence_map[d.op] > self.precedence_map[n.op]),
            c, m
        )

    def visit_Assignment(self, n, c, m):
        self.visit(n.lvalue, c, m)
        c.append(' ')
        c += list(str(n.op))
        c.append(' ')
        self._parenthesize_if(
                            n.rvalue,
                            lambda n: isinstance(n, c_ast.Assignment),
                            c, m)

    def visit_IdentifierType(self, n, c, m):
        if n.names:
            c += list(str(n.names[0]))
        for name in n.names[1:]:
            c += list(' ' + name)

    def _visit_expr(self, n, c, m):
        if isinstance(n, c_ast.InitList):
            c.append('{')
        elif isinstance(n, c_ast.ExprList):
            c.append('(')
        self.visit(n, c, m)
        if isinstance(n, c_ast.InitList):
            c.append('}')
        elif isinstance(n, c_ast.ExprList):
            c.append(')')

    def visit_Decl(self, n, c, m, no_type=False):
        # no_type is used when a Decl is part of a DeclList, where the type is
        # explicitly only for the first declaration in a list.
        #
        if no_type:
            c += list(str(n.name))
        else:
            self._generate_decl(n, c, m)
        if n.bitsize:
            c += list(' : ')
            self.visit(n.bitsize, c, m)
        if n.init:
            c += list(' = ')
            self._visit_expr(n.init, c, m)

    def visit_DeclList(self, n, c, m):
        self.visit(n.decls[0], c, m)
        for decl in n.decls[1:]:
            c += list(', ')
            self.visit_Decl(decl, c, m, no_type=True)

    def visit_Typedef(self, n, c, m):
        if n.funcspec: c += list(' '.join(n.funcspec) + ' ')
        if n.storage : c += list(' '.join(n.storage) + ' ')
        self._generate_type(n.type, c, m)

    def visit_Cast(self, n, c, m):
        c.append('(')
        self._generate_type(n.to_type, c, m, emit_declname=False)
        c += list(') ')
        self._parenthesize_unless_simple(n.expr, c, m)

    def visit_ExprList(self, n, c, m):
        if len(n.exprs) > 0:
            self._visit_expr(n.exprs[0], c, m)
        for expr in n.exprs[1:]:
            c += list(', ')
            self._visit_expr(expr, c, m)

    def visit_InitList(self, n, c, m):
        if len(n.exprs) > 0:
            self._visit_expr(n.exprs[0], c, m)
        for expr in n.exprs[1:]:
            c += list(', ')
            self._visit_expr(expr, c, m)

    def visit_Enum(self, n, c, m):
        self._generate_struct_union_enum(n, c, m, name='enum')

    def visit_Alignas(self, n, c, m):
        c += list('_Alignas(')
        self.visit(n.alignment, c, m)
        c.append(')')

    def visit_Enumerator(self, n, c, m):
        self._make_indent(c)
        c += list(str(n.name))
        if n.value:
            c += list(' = ')
            self.visit(n.value, c, m)
        c.append(',\n')

    def visit_FuncDef(self, n, c, m):
        self.visit(n.decl, c, m)
        self.indent_level = 0
        c.append('\n')
        if n.param_decls:
            for p in n.param_decls:
                self.visit(p, c, m)
                c += list(';\n')
        self.visit(n.body, c, m)
        c.append('\n')

    def visit_FileAST(self, n, c, m):
        for ext in n.ext:
            self.visit(ext, c, m)
            if isinstance(ext, c_ast.Pragma):
                c.append('\n')
            elif not isinstance(ext, c_ast.FuncDef):
                c += list(';\n')

    def visit_Compound(self, n, c, m):
        self._make_indent(c)
        c += list('{\n')
        self.indent_level += 2
        if n.block_items:
            for stmt in n.block_items:
                self._generate_stmt(stmt, c, m)
        self.indent_level -= 2
        self._make_indent(c)
        c += list('}\n')

    def visit_CompoundLiteral(self, n, c, m):
        c.append('(')
        self.visit(n.type, c, m)
        c += list('){')
        self.visit(n.init, c, m)
        c.append('}')


    def visit_EmptyStatement(self, n, c, m):
        c.append(';')

    def visit_ParamList(self, n, c ,m):
        if len(n.params) > 0:
            self.visit(n.params[0], c, m)
        for param in n.params[1:]:
            c += list(', ')
            self.visit(param, c, m)

    def visit_Return(self, n, c, m):
        c += list('return')
        if n.expr:
            c.append(' ')
            self.visit(n.expr, c, m)
        c.append(';')

    def visit_Break(self, n, c, m):
        c += list('break;')

    def visit_Continue(self, n, c, m):
        c += list('continue;')

    def visit_TernaryOp(self, n, c, m):
        c.append('(')
        self._visit_expr(n.cond, c, m)
        c += list(') ? (')
        self._visit_expr(n.iftrue, c, m)
        c += list(') : (')
        self._visit_expr(n.iffalse, c, m)
        c.append(')')

    def visit_If(self, n, c, m):
        c += list('if (')
        if n.cond:
            self.visit(n.cond, c, m)
        c += list(')\n')
        self._generate_stmt(n.iftrue, c, m, add_indent=True)
        if n.iffalse:
            self._make_indent(c)
            c += list('else\n')
            self._generate_stmt(n.iffalse, c, m, add_indent=True)

    def visit_For(self, n, c, m):
        c += list('for (')
        if n.init:
            self.visit(n.init, c, m)
        c.append(';')
        if n.cond:
            c.append(' ')
            self.visit(n.cond, c, m)
        c.append(';')
        if n.next:
            c.append(' ')
            self.visit(n.next, c, m)
        c += list(')\n')
        self._generate_stmt(n.stmt, c, m, add_indent=True)

    def visit_While(self, n, c, m):
        c += list('while (')
        if n.cond:
            self.visit(n.cond, c, m)
        c += list(')\n')
        self._generate_stmt(n.stmt, c, m, add_indent=True)

    def visit_DoWhile(self, n, c, m):
        c += list('do\n')
        self._generate_stmt(n.stmt, c, m, add_indent=True)
        self._make_indent(c)
        c += list('while (')
        if n.cond:
            self.visit(n.cond, c, m)
        c += list(');')

    def visit_StaticAssert(self, n, c, m):
        c += list('_Static_assert(')
        self.visit(n.cond, c, m)
        if n.message:
            c.append(',')
            self.visit(n.message, c, m)
        c.append(')')

    def visit_Switch(self, n, c, m):
        c += list('switch (')
        self.visit(n.cond, c, m)
        c += list(')\n')
        self._generate_stmt(n.stmt, c, m, add_indent=True)

    def visit_Case(self, n, c, m):
        c += list('case ')
        self.visit(n.expr, c, m)
        c += list(':\n')
        for stmt in n.stmts:
            self._generate_stmt(stmt, c, m, add_indent=True)

    def visit_Default(self, n, c, m):
        c += list('default:\n')
        for stmt in n.stmts:
            self._generate_stmt(stmt, c, m, add_indent=True)

    def visit_Label(self, n, c, m):
        c += list(n.name + ':\n')
        self._generate_stmt(n.stmt, c, m)

    def visit_Goto(self, n, c, m):
        c += list('goto ' + n.name + ';')

    def visit_EllipsisParam(self, n):
        return '...'

    def visit_Struct(self, n, c, m):
        return self._generate_struct_union_enum(n, c, m, 'struct')

    def visit_Typename(self, n, c, m):
        return self._generate_type(n.type, c, m)

    def visit_Union(self, n, c, m):
        return self._generate_struct_union_enum(n, c, m, 'union')

    def visit_NamedInitializer(self, n, c, m):
        for name in n.name:
            if isinstance(name, c_ast.ID):
                c += list('.' + name.name)
            else:
                c.append('[')
                self.visit(name, c, m)
                c.append(']')
        c += list(' = ')
        self._visit_expr(n.expr, c, m)

    def visit_FuncDecl(self, n, c, m):
        return self._generate_type(n, c, m)

    def visit_ArrayDecl(self, n, c, m):
        return self._generate_type(n, c, m, emit_declname=False)

    def visit_TypeDecl(self, n, c, m):
        return self._generate_type(n, c, m, emit_declname=False)

    def visit_PtrDecl(self, n, c, m):
        return self._generate_type(n, c, m, emit_declname=False)

    # GNU C stuff --------------------------------

    def visit_GNUAttribute(self, n, c, m):
        c += list("__attribute__((")
        if n.attrs:
            self.visit(n.attrs[0], c, m)
        for attr in n.attrs[1:]:
            c += list(', ')
            self.visit(attr, c, m)
        c += list("))")

    def visit_GNUExtendedExpression(self, n, c, m):
        name = n.expr.__class__.__name__

        if name == "ID":
            c += list("__extension__ ")
            self.visit(n.expr, c, m)
        else:
            c += list("__extension__(")
            self.visit(n.expr, c, m)
            c.append(")")


    # --------------------------------------------

    def _generate_struct_union_enum(self, n, c, m, name):
        """ Generates code for structs, unions, and enums. name should be
            'struct', 'union', or 'enum'.
        """
        if name in ('struct', 'union'):
            members = n.decls
            body_function = self._generate_struct_union_body
        else:
            assert name == 'enum'
            members = None if n.values is None else n.values.enumerators
            body_function = self._generate_enum_body
        c += list(name + ' ' + (n.name or ''))
        if members is not None:
            # None means no members
            # Empty sequence means an empty list of members
            c.append('\n')
            self._make_indent(c)
            self.indent_level += 2
            c += list('{\n')
            body_function(members, c, m)
            self.indent_level -= 2
            self._make_indent(c)
            c.append('}')
    def _generate_struct_union_body(self, members, c, m):
        for decl in members:
            self._generate_stmt(decl, c, m)

    def _generate_enum_body(self, members, c, m):
        # `c[-1:] = ['\n']` removes the final `,` from the enumerator list
        for value in members:
            self.visit(value, c, m)
        c[-1] = '\n'

    def _generate_stmt(self, n, c, m, add_indent=False):
        """ Generation from a statement node. This method exists as a wrapper
            for individual visit_* methods to handle different treatment of
            some statements in this context.
        """
        typ = type(n)
        if typ != c_ast.Compound:
            # No extra indentation required before the opening brace of a
            # compound - because it consists of multiple lines it has to
            # compute its own indentation.
            if add_indent: self.indent_level += 2
            self._make_indent(c)
            if add_indent: self.indent_level -= 2

        self.visit(n, c, m)
        if typ in (
                c_ast.Decl, c_ast.Assignment, c_ast.Cast, c_ast.UnaryOp,
                c_ast.BinaryOp, c_ast.TernaryOp, c_ast.FuncCall, c_ast.ArrayRef,
                c_ast.StructRef, c_ast.Constant, c_ast.ID, c_ast.Typedef,
                c_ast.ExprList):
            # These can also appear in an expression context so no semicolon
            # is added to them automatically
            #
            c += list(';\n')
        elif typ not in (c_ast.Compound, c_ast.If):
            c.append('\n')

    def _generate_decl(self, n, c, m):
        """ Generation from a Decl node.
        """
        if n.funcspec:
            for spec in n.funcspec:
                c += list(spec + ' ')
        if n.storage:
            for stor in n.storage:
                c += list(stor + ' ')
        if n.attrs:
            for attr in n.attrs:
                self.visit(attr, c, m)
                c.append(' ')
        if n.align:
            self.visit(n.align[0], c, m)
            c.append(' ')
        self._generate_type(n.type, c, m)

    def _generate_type(self, n, c, m, modifiers=[], emit_declname = True):
        """ Recursive generation from a type node. n is the type node.
            modifiers collects the PtrDecl, ArrayDecl and FuncDecl modifiers
            encountered on the way down to a TypeDecl, to allow proper
            generation from it.
        """
        typ = type(n)
        #~ print(n, modifiers)

        if typ == c_ast.TypeDecl:
            if n.quals:
                for qual in n.quals:
                    c += list(qual + ' ')
            self.visit(n.type, c, m)
            if (n.declname and emit_declname) or modifiers:
                c.append(' ')
            self._generate_modifiers(n.declname if n.declname and emit_declname else '', c, m, modifiers)

            if n.attrs:
                for attr in n.attrs:
                    c.append(' ')
                    self.visit(attr, c, m)
        elif typ == c_ast.Decl:
            return self._generate_decl(n.type, c, m)
        elif typ == c_ast.Typename:
            return self._generate_type(n.type, c, m, emit_declname = emit_declname)
        elif typ == c_ast.IdentifierType:
            return ' '.join(n.names) + ' '
        elif typ == c_ast.FuncDecl:
            result = self._generate_type(n.type, c, m, modifiers + [n],
                                       emit_declname = emit_declname)
            
            if n.attrs:
                for attr in n.attrs:
                    c.append(' ')
                    self.visit(attr, c, m)

        elif typ in (c_ast.ArrayDecl, c_ast.PtrDecl):
            return self._generate_type(n.type, c, m, modifiers + [n],
                                       emit_declname = emit_declname)
        else:
            return self.visit(n, c, m)

    def _generate_modifiers(self, name, c, m, modifiers):
        if not modifiers:
            c += list(str(name))
            return

        # Resolve modifiers.
        # Wrap in parens to distinguish pointer to array and pointer to
        # function syntax.
        #
        if isinstance(modifiers[-1], c_ast.ArrayDecl):
            if len(modifiers) >= 2 and isinstance(modifiers[-2], c_ast.PtrDecl):
                c.append('(')
            self._generate_modifiers(name, c, m, modifiers[:-1])
            if len(modifiers) >= 2 and isinstance(modifiers[-2], c_ast.PtrDecl):
                c.append(')')
            c.append('[')
            if modifiers[-1].dim_quals:
                for dim_qual in modifiers[-1].dim_quals:
                    c += list(str(dim_qual) + ' ')
            self.visit(modifiers[-1].dim, c, m)
            c.append(']')
        elif isinstance(modifiers[-1], c_ast.FuncDecl):
            if len(modifiers) >= 2 and isinstance(modifiers[-2], c_ast.PtrDecl):
                c.append('(')
            self._generate_modifiers(name, c, m, modifiers[:-1])
            if len(modifiers) >= 2 and isinstance(modifiers[-2], c_ast.PtrDecl):
                c.append(')')
            c.append('(')
            self.visit(modifiers[-1].args, c, m)
            c.append(')')
        elif isinstance(modifiers[-1], c_ast.PtrDecl):
            c.append('*')
            if modifiers[-1].quals:
                for qual in modifiers[-1].quals:
                    c += list(' ' + str(qual))
                c += ' '
            self._generate_modifiers(name, c, m, modifiers[:-1])

    def _parenthesize_if(self, n, condition, c, m):
        """ Visits 'n' and returns its string representation, parenthesized
            if the condition function applied to the node returns True.
        """
        cond = condition(n)
        if cond:
            c += '('
        self._visit_expr(n, c, m)
        if cond:
            c += ')'

    def _parenthesize_unless_simple(self, n, c, m):
        """ Common use case for _parenthesize_if
        """
        return self._parenthesize_if(n, lambda d: not self._is_simple_node(d), c, m)

    def _is_simple_node(self, n):
        """ Returns True for nodes that are "simple" - i.e. nodes that always
            have higher precedence than operators.
        """
        return isinstance(n, (c_ast.Constant, c_ast.ID, c_ast.ArrayRef,
                              c_ast.StructRef, c_ast.FuncCall))

