import ply.yacc as yacc
import collections


def type_convert(value, var_type):
    if var_type == "String":
        return str(value)
    elif var_type == "float":
        return float(value)
    else:
        return int(value)


class MyParser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.precedence = (

            ('left', 'plus', 'minus'),
            ('left', 'times', 'divide'),
            ('left', 'exponential'),
            ('left', 'plus_equal', 'minus_equal', 'times_equal', 'divide_equal', 'exponential_equal'),
            ('nonassoc', 'GT', 'LT', 'EQU', 'NEQ', 'GTE', 'LTE'),
            ('left', 'ELSE'),
            ('left', 'IF')
        )

        self.var_list = []
        self.var_dict = {}
        self.type_list = []
        self.type_list_res = []

        self.comment_flag = False

        self.errors = []
        self.jumps = []

        self.array_list = [[], []]
        self.array_desc = [[], []]
        self.array_values = []
        self.array_offset = [[0], []]
        self.array_base = 0

        self.operands = []
        self.end_quad = []
        self.operators = []
        self.procedures = {}
        self.avail = collections.deque()
        self.max_avail = 0
        self.quadruples = []
        self.quad_count = 0

        self.switch_var = []
        self.switch_jumps = []
        self.for_id = []
        self.array_id = []

        self.m_array = 0
        self.i_array = 0
        self.k_array = 0

        self.temp_dict = {}
        self.procedure_exe = []

    def handle_avail(self):
        if not self.avail:
            self.max_avail += 1
            return f'T{self.max_avail - 1}'
        return self.avail.popleft()

    def return_to_avail(self, temp):
        if temp not in self.var_dict:
            if str(temp)[0] == "T":
                self.avail.append(temp)
            elif str(temp)[0] == "a":
                if temp[1] == "T":
                    self.avail.append(temp[1:])

    def result_type(self, opcode, var_type1, var_type2):
        temp = ""
        if "int" in str(var_type1):
            if "int" in str(var_type2):
                if opcode == "+" or opcode == "-" or opcode == "*" or opcode == "/" or opcode == "=" or \
                        opcode == "^" or opcode == "%":
                    temp = "int"
                elif ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
            elif str(var_type2) == "float":
                if opcode == "+" or opcode == "-" or opcode == "*" or opcode == "/" or \
                        opcode == "=" or opcode == "^" or opcode == "%":
                    temp = "float"
                elif ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode \
                        or "and" in opcode:
                    temp = "bool"
                elif opcode == "=":
                    temp = "int"
            elif str(var_type2) == "bool":
                if ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
                elif opcode == "=":
                    temp = var_type1
                else:
                    temp = ""
            else:
                temp = ""
        elif str(var_type1) == "float":
            if str(var_type2) == "float":
                if opcode == "+" or opcode == "-" or opcode == "*" or opcode == "/" or opcode == "=" or \
                        opcode == "^" or opcode == "%":
                    temp = "float"
                elif ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
            elif "int" in str(var_type2):
                if opcode == "+" or opcode == "-" or opcode == "*" or opcode == "/" or opcode == "=" or \
                        opcode == "^" or opcode == "%":
                    temp = "float"
                elif ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
            elif str(var_type2) == "bool":
                if ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
                elif opcode == "=":
                    temp = var_type1
                else:
                    temp = ""
            else:
                temp = ""
        elif str(var_type1) == "bool":
            if str(var_type2) == "float":
                if ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
                else:
                    temp = ""
            elif "int" in str(var_type2):
                if ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode:
                    temp = "bool"
                else:
                    temp = ""
            elif str(var_type2) == "bool":
                if ">" in opcode or "<" in opcode or opcode == "!=" or opcode == "==" or "or" in opcode or \
                        "and" in opcode or opcode == "=":
                    temp = "bool"
                else:
                    temp = ""
            else:
                temp = ""
        elif str(var_type1) == "String":
            if str(var_type2) == "String":
                if opcode == "+" or opcode == "=":
                    temp = "String"
                elif opcode == "==" or opcode == "!=":
                    temp = "bool"
                else:
                    temp = ""
            else:
                temp = ""
        else:
            temp = ""
            self.errors.append(var_type1 + " is not a valid type")
        self.type_list_res.append(temp)

    def converter(self, operand):
        if operand in self.var_dict:
            return self.var_dict[operand][1]
        elif "T" == str(operand)[0]:
            return self.temp_dict[operand]
        elif "a" == str(operand)[0]:
            if operand[1:] in self.var_dict:
                return self.array_values[self.var_dict[operand[1:]][1]]
            elif operand[1] == "T":
                return self.array_values[self.temp_dict[operand[1:]]]
            else:
                return self.array_values[self.array_offset[0][int(operand[1:])]: self.array_offset[0]
                [int(operand[1:]) + 1]]
        elif "l" == str(operand)[0]:
            return len(operand)
        elif "s" == str(operand)[0]:
            return operand[1:]
        else:
            return operand

    def quad_solver(self):
        pc = 0

        if len(self.array_desc) > 1:
            for i in range(0, self.array_offset[0][-1]):
                self.array_values.append([])

        while True:
            if len(self.quadruples[pc]) == 1:
                opcode = self.quadruples[pc][0]
                if opcode == "EndProgram":
                    break
                elif opcode == "EndProcedure":
                    pc = self.procedure_exe.pop()
            elif len(self.quadruples[pc]) == 2:
                opcode, operand1 = self.quadruples[pc]
                if opcode == "goSub":
                    self.procedure_exe.append(pc + 1)
                    pc = operand1
                elif opcode == "print":
                    to_print = self.converter(operand1)
                    print(to_print)
                    pc += 1
                elif opcode == "input":
                    self.assign_convert(operand1, int(input('> ')))
                    pc += 1
                elif opcode == "goTo":
                    pc = int(operand1)
            elif len(self.quadruples[pc]) == 3:
                opcode, operand1, operand2 = self.quadruples[pc]
                if opcode == "=":
                    temp1 = self.converter(operand1)
                    self.assign_convert(operand2, temp1)
                    pc += 1
                elif opcode == "goToF":
                    if str(operand1)[0] == "T":
                        if not self.temp_dict[operand1]:
                            pc = operand2
                        else:
                            pc += 1
                    else:
                        if not self.var_dict[operand1][1]:
                            pc = operand2
                        else:
                            pc += 1
                elif opcode == "goToT":
                    if str(operand1)[0] == "T":
                        if self.temp_dict[operand1]:
                            pc = operand2
                        else:
                            pc += 1
                    else:
                        if self.var_dict[operand1][1]:
                            pc = operand2
                        else:
                            pc += 1
                elif opcode == "v":
                    temp1 = self.converter(operand1)
                    temp2 = self.converter(operand2)
                    if temp1 >= temp2:
                        print(temp1)
                        print(temp2)
                        print("Index out of bounds in " + str(pc))
                        break
                    else:
                        pc += 1
                else:
                    self.quad_arithmetic(self.quadruples[pc])
                    pc += 1
            else:
                self.quad_arithmetic(self.quadruples[pc])
                pc += 1

    def assign_convert(self, operand, value):
        if operand in self.var_dict:
            self.var_dict[operand][1] = type_convert(value, self.var_dict[operand][0])
        elif "a" == str(operand)[0]:
            if operand[1:] in self.var_dict:
                self.array_values[self.var_dict[operand[1:]][1]] = value
            elif operand[1] == "T":
                self.array_values[self.temp_dict[operand[1:]]] = value
            else:
                self.array_values[self.array_offset[0][int(operand[1:])]:
                                  self.array_offset[0][int(operand[1:]) + 1]] = value
        elif "T" == str(operand)[0]:
            self.temp_dict[operand] = value

    def quad_arithmetic(self, quad):
        op = quad[0]
        operand1 = quad[1]
        operand2 = quad[2]

        if len(quad) == 4:
            _, operand1, operand2, operand3 = quad[:]
            temp1 = self.converter(operand1)
            temp2 = self.converter(operand2)
            if operand3 in self.var_dict:
                if op == "+":
                    self.var_dict[operand3][1] = temp1 + temp2
                elif op == "-":
                    self.var_dict[operand3][1] = temp1 - temp2
                elif op == "*":
                    self.var_dict[operand3][1] = temp1 * temp2
                elif op == "/":
                    self.var_dict[operand3][1] = temp1 / temp2
                elif op == "^":
                    self.var_dict[operand3][1] = temp1 ** temp2
                elif op == "%":
                    self.var_dict[operand3][1] = temp1 % temp2
                elif op == "<":
                    self.var_dict[operand3][1] = temp1 < temp2
                elif op == ">":
                    self.var_dict[operand3][1] = temp1 > temp2
                elif op == "<=":
                    self.var_dict[operand3][1] = temp1 <= temp2
                elif op == ">=":
                    self.var_dict[operand3][1] = temp1 >= temp2
                elif op == "==":
                    self.var_dict[operand3][1] = temp1 == temp2
                elif op == "!=":
                    self.var_dict[operand3][1] = temp1 != temp2
                elif op == "<<":
                    self.var_dict[operand3][1] = temp1 << temp2
                elif op == ">>":
                    self.var_dict[operand3][1] = temp1 >> temp2
                elif op == "or":
                    self.var_dict[operand3][1] = temp1 or temp2
                elif op == "and":
                    self.var_dict[operand3][1] = temp1 and temp2
            elif "T" == str(operand3)[0]:
                if op == "+":
                    if isinstance(temp1, int) and isinstance(temp2, str):
                        print(quad[:])
                    self.temp_dict[operand3] = temp1 + temp2
                elif op == "-":
                    self.temp_dict[operand3] = temp1 - temp2
                elif op == "*":
                    self.temp_dict[operand3] = temp1 * temp2
                elif op == "/":
                    self.temp_dict[operand3] = temp1 / temp2
                elif op == "^":
                    self.temp_dict[operand3] = temp1 ** temp2
                elif op == "%":
                    self.temp_dict[operand3] = temp1 % temp2
                elif op == "<":
                    self.temp_dict[operand3] = temp1 < temp2
                elif op == "<<":
                    self.temp_dict[operand3] = temp1 << temp2
                elif op == ">":
                    self.temp_dict[operand3] = temp1 > temp2
                elif op == ">>":
                    self.temp_dict[operand3] = temp1 >> temp2
                elif op == "<=":
                    self.temp_dict[operand3] = temp1 <= temp2
                elif op == ">=":
                    self.temp_dict[operand3] = temp1 >= temp2
                elif op == "==":
                    self.temp_dict[operand3] = temp1 == temp2
                elif op == "!=":
                    self.temp_dict[operand3] = temp1 != temp2
                elif op == "or":
                    self.temp_dict[operand3] = temp1 or temp2
                elif op == "and":
                    self.temp_dict[operand3] = temp1 and temp2

        else:
            temp1 = self.converter(operand1)
            if op == "length":
                self.assign_convert(operand2, temp1)
            elif str(op[0]) == "k":
                cast_var = temp1
                if op[1] == "i":
                    cast_var = int(temp1)
                elif op[1] == "f":
                    cast_var = float(temp1)
                elif op[1] == "b":
                    cast_var = int(temp1)
                elif op[1] == "S":
                    cast_var = str(temp1)

                self.assign_convert(operand2, cast_var)
            elif op == "+=":
                self.var_dict[operand2][1] += temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "-=":
                self.var_dict[operand2][1] -= temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "*=":
                self.var_dict[operand2][1] *= temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "/=":
                self.var_dict[operand2][1] /= temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "^=":
                self.var_dict[operand2][1] **= temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "%=":
                self.var_dict[operand2][1] %= temp1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "++":
                self.var_dict[operand2][1] += 1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])
            elif op == "--":
                self.var_dict[operand2][1] -= 1
                self.var_dict[operand2][1] = type_convert(self.var_dict[operand2][1], self.var_dict[operand2][0])

    def clear_var(self):
        self.var_list = []
        self.var_dict = {}
        self.type_list = []
        self.type_list_res = []

        self.comment_flag = False

        self.errors = []
        self.jumps = []

        self.array_list = [[], []]
        self.array_desc = [[], []]
        self.array_values = []
        self.array_offset = [[0], []]
        self.array_base = 0

        self.operands = []
        self.end_quad = []
        self.operators = []
        self.procedures = {}
        self.avail = collections.deque()
        self.max_avail = 0
        self.quadruples = []
        self.quad_count = 0

        self.switch_var = []
        self.switch_jumps = []
        self.for_id = []
        self.array_id = []

        self.m_array = 0
        self.i_array = 0
        self.k_array = 0

        self.temp_dict = {}
        self.procedure_exe = []

    def p_Programa(self, p):
        '''
        Programa : Programa3 B cor2
        '''
        self.quadruples.append(["EndProgram"])
        self.quad_count += 1

        if self.errors:
            print("Error: ", end="")
            print(self.errors)
            self.errors.clear()
        else:
            print("No syntax errors")

        if self.quadruples:
            print(self.quadruples)
            self.quad_solver()

        self.clear_var()

    def p_ProgramStart(self, p):
        '''
        ProgramStart :
        '''
        self.quad_count = 0
        self.quadruples.append(["goTo"])
        self.quad_count += 1

    def p_Programa2(self, p):
        '''
        Programa2 : ProgramStart VCall P
        '''

    def p_Programa3(self, p):
        '''
        Programa3 : Programa2 PROGRAM cor1
        '''
        self.quadruples[0].append(self.quad_count)

    def assign_var(self, var):
        if var in self.var_dict:
            self.operands.append(var)
            self.type_list.append(self.var_dict[var][0])
            temp1 = self.operands.pop()
            temp2 = self.operands.pop()

            self.quadruples.append([self.operators.pop(), temp2, temp1])
            if len(self.type_list) > 1:
                t2 = self.type_list.pop()
                t1 = self.type_list.pop()
                self.result_type(self.quadruples[self.quad_count][0], t1, t2)
            self.quad_count += 1
        else:
            self.errors.append("\'" + var + "\' was not declared")

    def declare_var(self, var_type):
        for var in self.var_list:
            if var in self.var_dict:
                self.errors.append("\'" + var + "\' was already declared")
            else:
                if var_type == "String":
                    self.var_dict[var] = [var_type, ""]
                else:
                    self.var_dict[var] = [var_type, 0]
        while self.var_list:
            self.var_list.pop()

    def p_VCall(self, p):
        '''
        VCall : VCall V
              | VCall DV
              |
        '''

    def p_V(self, p):
        '''
        V : DIM X id AS INT
            | DIM X id AS FLOAT
            | DIM X id AS BOOL
            | DIM X id AS STRING
            | LET id equal E
            | LET id plus_equal E
            | LET id minus_equal E
            | LET id times_equal E
            | LET id divide_equal E
            | LET id mod_equal E
            | LET id exponential_equal E
            | LET arrayEnd equal E
            | LET arrayEnd plus_equal E
            | LET arrayEnd minus_equal E
            | LET arrayEnd times_equal E
            | LET arrayEnd divide_equal E
            | LET arrayEnd mod_equal E
            | LET arrayEnd exponential_equal E
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                if p[1] == 'dim':
                    self.var_list.append(p[3])
                    self.declare_var(p[5])
                elif p[1] == 'let' and p[2]:
                    self.operators.append(p[3])
                    self.assign_var(p[2])
                    if len(self.end_quad):
                        self.quadruples.append(self.end_quad.pop())
                        self.quad_count += 1
                else:
                    op1 = self.operands.pop()
                    op2 = self.operands.pop()

                    self.quadruples.append(["=", op1, op2])
                    self.quad_count += 1

                    self.return_to_avail(op2)
                    self.return_to_avail(op1)

    def p_DVStart(self, p):
        '''
            DVStart : DIM INT id
                    | DIM FLOAT id
                    | DIM BOOL id
                    | DIM STRING id
        '''

        if not self.comment_flag and not self.errors:
            self.array_id.append(p[3])
            self.array_desc[0].append(p[3])
            self.array_desc[1].append(p[2])
            self.array_list[0].append(p[3])
            self.array_list[1].append(p[2])
            self.array_offset[1].append(len(self.array_desc[1]) - 1)
            self.m_array = 1
            self.i_array = 1

    def p_DV(self, p):
        '''
        DV : DVStart ADV scor1 num scor2
        '''

        if not self.comment_flag and not self.errors:

            m_temp = []
            self.array_desc[0].append(p[4])
            self.m_array *= int(p[4])
            m_temp.append(self.m_array)

            for j in range(1, self.i_array):
                m_temp.append(m_temp[j - 1] // self.array_desc[0]
                [j + self.array_desc[0].index(self.array_id[-1])])
                self.array_desc[1].append(m_temp[j])

            self.array_base += self.m_array
            self.array_desc[0].append([])
            self.array_desc[1].append([])
            self.array_offset[0].append(self.array_base)
            self.array_id.pop()

    def p_ADV(self, p):
        '''
        ADV : ADV scor1 num scor2
            |
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.array_desc[0].append(p[3])
                self.m_array *= int(p[3])
                self.i_array += 1

    def p_arrayCallStart(self, p):
        '''
        arrayCallStart : id
        '''

        if not self.comment_flag and not self.errors and p[1] is not None:
            self.operands.append(self.array_desc[0].index(p[1]))

    def p_arrayCall(self, p):
        '''
        arrayCall : arrayCallStart scor1
        '''

        if not self.comment_flag and not self.errors:
            var_id = self.operands.pop()
            self.array_id.append(var_id)
            self.k_array = 1

    def array_in_bounds(self):
        self.quadruples.append(
            ["v", self.operands[-1], int(self.array_desc[0][self.array_id[-1] + self.k_array])])
        self.quad_count += 1

        if self.array_desc[1][self.array_offset[1][self.array_list[0].index(
                self.array_desc[0][self.array_id[-1]])] + self.k_array - 1] != [] and not isinstance(
            self.array_desc[1][self.array_offset[1][self.array_list[0].index(
                self.array_desc[0][self.array_id[-1]])] + self.k_array], str):

            opr = self.array_desc[1][self.array_offset[1][self.array_list[0]
                .index(self.array_desc[0][self.array_id[-1]])] + self.k_array]
            if opr:
                aux = self.operands.pop()
                self.return_to_avail(aux)

                temp = self.handle_avail()
                self.quadruples.append(["*", aux, opr, temp])
                self.quad_count += 1
                self.operands.append(temp)

            if self.k_array > 1:
                aux1 = self.operands.pop()
                aux2 = self.operands.pop()

                self.return_to_avail(aux1)
                self.return_to_avail(aux2)

                temp = self.handle_avail()
                self.quadruples.append(["+", aux2, aux1, temp])
                self.quad_count += 1
                self.operands.append(temp)

    def p_arrayEnd(self, p):
        '''
        arrayEnd : arrayCall xArray E scor2
        '''

        if not self.comment_flag and not self.errors:
            self.array_in_bounds()
            aux3 = self.operands.pop()
            self.return_to_avail(aux3)

            temp = self.handle_avail()
            if self.k_array + 1 != len(self.array_offset[0]):
                self.quadruples.append(["+", aux3, self.array_offset[0]
                [self.array_list[0].index(self.array_desc[0][self.array_id[-1]])], temp])
                self.quad_count += 1
            self.operands.append(f'a{temp}')
            self.array_id.pop()

    def p_xArray(self, p):
        '''
        xArray : xArray E scor2 scor1
               |
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.array_in_bounds()
                self.k_array += 1

    def p_X(self, p):
        '''
        X : X id coma
            |
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.var_list.append(p[2])

    def p_PStart(self, p):
        '''
        PStart : P SUB id cor1
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.procedures[p[3]] = self.quad_count

    def p_P(self, p):
        '''
        P : PStart B cor2
            |
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.quadruples.append(["EndProcedure"])
                self.quad_count += 1

    def p_SCom(self, p):
        '''
        SCom : start_com
        '''

        self.comment_flag = True

    def p_MCom(self, p):
        '''
            MCom : SCom
                 | MCom num
                 | MCom letters
        '''

        self.comment_flag = False

    def p_SMulCom(self, p):
        '''
        SMulCom : start_multcom
        '''
        self.comment_flag = True

    def p_MMulCom(self, p):
        '''
            MMulCom : SMulCom
                    | MMulCom num
                    | MMulCom letters
        '''

    def p_EMulCom(self, p):
        '''
        EMulCom : MMulCom end_multcom
        '''

        self.comment_flag = False

    def p_ECom(self, p):
        '''
        ECom : MCom
             | EMulCom
        '''

    def p_B(self, p):
        '''
        B : B S
            |
        '''

    def p_S(self, p):
        '''
        S : GOSUB id
            | INPUT INP
            | PRINT PR
            | EQ
            | LOOPS
            | V
            | DV
            | endCase
            | DOWHILE
            | EndFor
            | ECom
            | E
            | ConditionEnd
        '''

        if not self.comment_flag and not self.errors:

            if len(self.end_quad) > 0:
                self.quadruples.append(self.end_quad.pop())
                self.quad_count += 1

            if p[1] == "goSub":
                self.quadruples.append([p[1], self.procedures[p[2]]])
                self.quad_count += 1

    def assign_false_index(self, false_index):
        self.quadruples[self.jumps.pop()].append(false_index)

    def conditional(self, go_to, type_check=True):
        if type_check:
            aux = self.type_list_res.pop()
            if aux != "bool" and "int" not in aux and aux != "float":
                self.errors.append("if can't compare non boolean")

        result = self.operands.pop()
        self.return_to_avail(result)

        self.quadruples.append([])
        self.quadruples[self.quad_count].append(go_to)
        self.quadruples[self.quad_count].append(result)
        self.quad_count += 1

    def p_ConditionStart(self, p):
        '''
        ConditionStart : E interrogacion
        '''

        if not self.comment_flag and not self.errors:
            self.conditional("goToF", False)
            self.jumps.append(self.quad_count - 1)

    def p_Condition(self, p):
        '''
        Condition : ConditionStart S dos_puntos
                  | ConditionStart dos_puntos
        '''

        if not self.comment_flag and not self.errors:
            self.assign_false_index(self.quad_count + 1)

            self.quadruples.append([])
            self.quadruples[self.quad_count].append("goTo")
            self.quad_count += 1

            self.jumps.append(self.quad_count - 1)

    def p_ConditionEnd(self, p):
        '''
        ConditionEnd : Condition S
        '''

        if not self.comment_flag and not self.errors:
            self.assign_false_index(self.quad_count)

    def p_INP(self, p):
        '''
        INP : id
            | arrayEnd
        '''

        if not self.comment_flag and len(self.errors) == 0:
            if p[1] is not None:
                self.operands.append(p[1])
            to_print = self.operands.pop()
            self.quadruples.append(["input", to_print])
            self.quad_count += 1

    def p_PR(self, p):
        '''
        PR : E
        '''

        if not self.comment_flag and not self.errors:
            to_print = self.operands.pop()
            self.quadruples.append(["print", to_print])
            self.quad_count += 1

    def p_Q(self, p):
        '''
        Q : IF parenthesis1 E parenthesis2
        '''

        if not self.comment_flag and not self.errors:
            self.conditional("goToF")
            self.jumps.append(self.quad_count - 1)

    def p_QTHEN(self, p):
        '''
        QTHEN : Q cor1 B cor2
                | Q S
        '''

    def p_EQ(self, p):
        '''
        EQ : QTHEN QElse cor1 B cor2
            | QTHEN QElse S
            | QTHEN
        '''

        if not self.comment_flag and not self.errors:
            if "if" in p:
                self.conditional("goToF")
                self.jumps.append(self.quad_count - 1)
            self.assign_false_index(self.quad_count)

    def p_QElse(self, p):
        '''
            QElse : ELSE
        '''

        if not self.comment_flag and not self.errors:
            self.quadruples.append([])
            self.quadruples[self.quad_count].append("goTo")

            self.assign_false_index(self.quad_count + 1)
            self.quad_count += 1

            self.jumps.append(self.quad_count - 1)

    def p_startFor(self, p):
        '''
        startFor : FOR id
        '''

        if not self.comment_flag and not self.errors:
            self.operands.append(p[2])

    def p_FREach(self, p):
        '''
        FREach : FOR EACH id IN id
        '''

        if not self.comment_flag and not self.errors:
            index = self.handle_avail()
            self.for_id.append(index)

            self.quadruples.append(["=", 0, index])
            self.quad_count += 1

            self.quadruples.append(["<", index, "l" + p[5], self.avail[-1]])
            self.quad_count += 1

            self.operands.append(index)

            self.jumps.append(self.quad_count - 1)

            temp = self.handle_avail()
            self.quadruples.append(["goToF", temp])
            self.quad_count += 1
            self.avail.append(temp)

    def p_FR(self, p):
        '''
        FR : startFor equal E
        '''

        if not self.comment_flag and not self.errors:
            exp1 = self.operands.pop()
            self.return_to_avail(exp1)

            index = self.operands[-1]
            self.for_id.append(index)

            self.quadruples.append(["=", exp1, index])
            self.quad_count += 1

    def p_TOFR(self, p):
        '''
        TOFR : FR TO E
        '''

        if not self.comment_flag and not self.errors:
            exp2 = self.operands.pop()
            temp = self.handle_avail()

            index = self.for_id.pop()

            self.quadruples.append(["<=", index, exp2, temp])
            self.quad_count += 1
            self.quadruples.append(["goToF", temp])
            self.quad_count += 1
            self.avail.append(temp)

            self.jumps.append(self.quad_count - 2)

    def p_EndFor(self, p):
        '''
        EndFor : TOFR S
               | TOFR cor1 B cor2
               | FREach S
               | FREach cor1 B cor2
        '''

        if not self.comment_flag and not self.errors:
            index = self.operands.pop()

            self.quadruples.append(["+", index, 1, index])
            self.quad_count += 1
            go_back = self.jumps.pop()

            self.quadruples.append([])
            self.quadruples[self.quad_count].append("goTo")
            self.quadruples[self.quad_count].append(go_back)
            self.quad_count += 1
            self.quadruples[go_back + 1].append(self.quad_count)

            self.return_to_avail(index)

    def p_DOWH(self, p):
        '''
        DOWH : DO S
             | DO cor1 B cor2
        '''

        if not self.comment_flag and not self.errors:
            self.jumps.append(self.quad_count - 2)

    def p_DOWHILE(self, p):
        '''
        DOWHILE : DOWH WHILE parenthesis1 E parenthesis2
        '''

        if not self.comment_flag and not self.errors:
            self.conditional("goToT")
            false_index = self.jumps.pop()
            self.quadruples[self.quad_count - 1].append(false_index)

    def p_WH1(self, p):
        '''
            WH1 : WHILE
            '''

        if not self.comment_flag and not self.errors:
            self.jumps.append(self.quad_count)

    def p_WH(self, p):
        '''
        WH : WH1 parenthesis1 E parenthesis2
        '''

    def p_LO(self, p):
        '''
        LO : WH
        '''

        if not self.comment_flag and not self.errors:
            self.conditional("goToF")
            self.jumps.append(self.quad_count - 1)

    def p_LOOPS(self, p):
        '''
            LOOPS : LO cor1 B cor2
                 | LO S
        '''

        if not self.comment_flag and not self.errors:
            false_index = self.jumps.pop()
            self.quadruples.append([])
            self.quad_count += 1
            self.quadruples[self.quad_count - 1].append("goTo")
            self.quadruples[self.quad_count - 1].append(self.jumps.pop())
            self.quadruples[false_index].append(self.quad_count)

    def handle_operations(self, p, i=2):
        if len(p) > 2:
            if i == 1:
                if p[1] == "length":
                    self.operators.append(p[1])
                else:
                    self.operators.append("k" + p[1][0])

                opr = self.operands.pop()
                self.return_to_avail(opr)

                result = self.handle_avail()
                self.quadruples.append([self.operators.pop(), opr, result])

                if len(self.type_list) > 1:
                    self.type_list.pop()
                    if p[1] == "length":
                        self.type_list.append("int")
                    else:
                        self.type_list.append(p[1])
            elif i == 2:
                self.operators.append(p[2])

                opr = []
                for i in range(0, 2):
                    opr.append(self.operands.pop())
                    self.return_to_avail(opr[-1])

                result = self.handle_avail()
                self.quadruples.append([self.operators.pop(), opr[1], opr[0], result])
                if len(self.type_list) > 1:
                    t2 = self.type_list.pop()
                    t1 = self.type_list.pop()
                    self.result_type(self.quadruples[self.quad_count][0], t1, t2)

            self.operands.append(result)
            self.quad_count += 1

    def p_E(self, p):
        '''
        E : EOR
        '''

    def p_EOR(self, p):
        '''
        EOR : EOR OR EAND
             | EOR NOR EAND
             | EAND
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_EAND(self, p):
        '''
        EAND : EAND AND ER1
             | EAND NAND ER1
             | ER1
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_ER1(self, p):
        '''
        ER1 : ER1 EQU ER2
             | ER1 NEQ ER2
             | ER2
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_ER2(self, p):
        '''
        ER2 : ER2 GT EBSHIFT
             | ER2 LT EBSHIFT
             | ER2 GTE EBSHIFT
             | ER2 LTE EBSHIFT
             | EBSHIFT
        '''
        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_EBSHIFT(self, p):
        '''
        EBSHIFT : EBSHIFT shift_left ES
                | EBSHIFT shift_right ES
                | ES
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_ES(self, p):
        '''
        ES : T
            | ES plus T
            | ES minus T
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_T(self, p):
        '''
        T : EXP
            | T times EXP
            | T divide EXP
            | T mod EXP
        '''

        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_EXP(self, p):
        '''

        EXP : CAST
            | EXP exponential CAST
        '''
        if not self.comment_flag and not self.errors:
            self.handle_operations(p)

    def p_CAST(self, p):
        '''
        CAST : INT parenthesis1 CAST parenthesis2
            | FLOAT parenthesis1 CAST parenthesis2
            | BOOL parenthesis1 CAST parenthesis2
            | STRING parenthesis1 CAST parenthesis2
            | LENGTH parenthesis1 CAST parenthesis2
            | F
        '''
        if not self.comment_flag and not self.errors:
            self.handle_operations(p, 1)

    def p_F(self, p):
        '''
        F : id
            | id plusplus
            | plusplus id
            | id minusminus
            | minusminus id
            | num
            | minus num
            | minus id
            | TRUE
            | FALSE
            | str
            | arrayEnd
            | arrayEnd plusplus
            | plusplus arrayEnd
            | arrayEnd minusminus
            | minusminus arrayEnd
            | parenthesis1 E parenthesis2
        '''

        if not self.comment_flag and not self.errors:
            if "++" in p or "--" in p:
                if p[2] == "++" or p[2] == "--":
                    if p[1] is not None:
                        self.end_quad.append([p[2], p[1], p[1]])
                        self.operands.append(p[1])
                        self.type_list.append(self.var_dict[p[1]][0])
                    else:
                        self.end_quad.append([p[2], self.operands[-1], self.operands[-1]])
                else:
                    if p[2] is not None:
                        self.quadruples.append([p[1], p[2], p[2]])
                        self.quad_count += 1
                        self.operands.append(p[2])
                        self.type_list.append(self.var_dict[p[2]][0])
                    else:
                        self.quadruples.append([p[1], self.operands[-1], self.operands[-1]])
                        self.quad_count += 1
            elif p[1] is not None:
                if str(p[1]) == "True":
                    self.operands.append(1)
                elif str(p[1]) == "False":
                    self.operands.append(0)
                elif str(p[1])[0] == "\"":
                    self.operands.append("s" + p[1].replace("\"", ""))
                elif not str(p[1])[0].isdigit() and "(" not in p[1] and "-" not in p[1]:
                    self.operands.append(p[1])
                    self.type_list.append(self.var_dict[p[1]][0])
                elif str(p[1])[0].isdigit():
                    self.operands.append(p[1])
                elif str(p[2])[0].isdigit() and p[1] == "-":
                    self.operands.append(-p[2])
                elif not str(p[2])[0].isdigit() and p[1] == "-":
                    result = self.handle_avail()
                    self.quadruples.append(["-", 0, p[2], result])
                    self.quad_count += 1
                    self.operands.append(result)

    def assign_false_switch(self, false_index):
        self.quadruples[self.switch_jumps.pop()].append(false_index)

    def p_SWC(self, p):
        '''
        SWC : SWITCH parenthesis1 id parenthesis2 cor1
            | SCase2
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                self.switch_var.append(p[3])

    def p_SCase(self, p):
        '''
        SCase : SWC CASE E dos_puntos
        '''

        if not self.comment_flag and not self.errors:
            if len(p) > 2:
                result = self.handle_avail()
                self.quadruples.append(["==", self.switch_var[-1], self.operands.pop(), result])
                self.quad_count += 1

                self.return_to_avail(result)

                self.quadruples.append([])
                self.quadruples[self.quad_count].append("goToF")
                self.quadruples[self.quad_count].append(result)
                self.quad_count += 1

                self.switch_jumps.append(self.quad_count - 1)

    def p_SCase2(self, p):
        '''
        SCase2 : SCase B BREAK
                | SCase B
        '''
        if not self.comment_flag and not self.errors:
            self.assign_false_switch(self.quad_count + 1)

            if len(p) > 3:
                self.quadruples.append([])
                self.quadruples[self.quad_count].append("goTo")
                self.quad_count += 1

                self.switch_jumps.append(self.quad_count - 1)

    def p_endCase(self, p):
        '''
        endCase : SCase2 cor2
        '''
        if not self.comment_flag and not self.errors:
            for i in range(0, len(self.switch_jumps)):
                self.assign_false_switch(self.quad_count)
            self.switch_var.pop()

    def p_error(self, p):
        if p is not None:
            print("Syntax error in line " + str(p.lineno))

        if self.errors:
            print("Error: ", end="")
            print(self.errors)
            self.errors.clear()

        self.clear_var()

    def parse(self):
        return yacc.yacc(module=self)
