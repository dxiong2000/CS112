#! /usr/bin/env python3
'''
Daniel Xiong dxiong5@ucsc.edu id#1660652
tli.py
Due 11/22/2019
'''
import fileinput
import io
import sys

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr:
	def __init__(self,operator,op1=None,op2=None):
		"""
		operator types:
		var, constant, printstring, 
		plus, minus, mult, div, lt, gt, le, ge, eq, neq
		"""
		
		self.op1 = op1
		self.operator = operator
		self.op2 = op2

	def __str__(self):
		if self.op2 == None:
			return self.operator + " " + str(self.op1)
		else:
			return str(self.op1) + " " + self.operator + " " +  str(self.op2)

	def eval(self, symTable, lineNum):
		""" evaluates Expr objects """

		if self.operator == "var":
			if self.op1 in symTable:
				return symTable[self.op1]
			else:
				print("Undefined variable {} at line {}.".format(self.op1, lineNum))
				sys.exit()
		elif self.operator == "constant":
			return self.op1
		elif self.operator == "printstring":
			return self.op1
		elif self.operator == "plus":
			return self.op1.eval(symTable, lineNum)+self.op2.eval(symTable, lineNum)
		elif self.operator == "minus":
			return self.op1.eval(symTable, lineNum)-self.op2.eval(symTable, lineNum)
		elif self.operator == "mult":
			return self.op1.eval(symTable, lineNum)*self.op2.eval(symTable, lineNum)
		elif self.operator == "div":
			return self.op1.eval(symTable, lineNum)/self.op2.eval(symTable, lineNum)
		elif self.operator == "lt":
			if self.op1.eval(symTable, lineNum) < self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		elif self.operator == "gt":
			if self.op1.eval(symTable, lineNum) > self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		elif self.operator == "le":
			if self.op1.eval(symTable, lineNum) <= self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		elif self.operator == "ge":
			if self.op1.eval(symTable, lineNum) >= self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		elif self.operator == "eq":
			if self.op1.eval(symTable, lineNum) == self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		elif self.operator == "neq":
			if self.op1.eval(symTable, lineNum) != self.op2.eval(symTable, lineNum):
				return 1
			else:
				return 0
		else:
			print("Syntax error on line {}.".format(lineNum))
			sys.exit()

class Scanner:
    def __init__(self):
        self.currentLine = []
    def readInt(self):
        if len(self.currentLine) == 0:
            self.currentLine = input().split()
        next = self.currentLine[0]
        self.currentLine = self.currentLine[1:]
        return next

class Stmt :
	""" Stmt object """

	def __init__(self,keyword,var=None,gotoLabel=None,exprs=None):
		self.keyword = keyword
		self.exprs = exprs
		self.var = var
		self.gotoLabel = gotoLabel

	def __str__(self):
		others = ""
		if self.keyword == 'let':
			others = others + " " + self.var
			others = others + " " + str(self.exprs)
		elif self.keyword == 'print':
			for exp in self.exprs:
				others = others + " " + str(exp)
		elif self.keyword == 'if':
			others = others + " " + str(self.exprs)
			others = others + ' goto ' + self.gotoLabel
		elif self.keyword == 'input':
			others = others + " " + self.var
		return self.keyword + others

	def perform(self, output, symTable, lineNum, scan):
		""" performs a Stmt object, returns updated parameters """

		if self.keyword == 'let': # if let statement, update symtable and lineNum and return
			symTable[self.var] = self.exprs.eval(symTable, lineNum)
			return (output, symTable, lineNum+1)
		elif self.keyword == 'print': # if print statement, update output and lineNum and return
			for e in self.exprs:
				if e == self.exprs[-1]:
					output = output + str(e.eval(symTable, lineNum)) + '\n'
				else:
					output = output + str(e.eval(symTable, lineNum)) + ' '
			return (output, symTable, lineNum+1)
		elif self.keyword == 'if': # if ifgoto statement, check if label is in symtable and update lineNum and return
			if self.exprs.eval(symTable, lineNum) != 0:
				if self.gotoLabel in symTable:
					return (output, symTable, symTable[self.gotoLabel])
				else:
					print("Illegal goto {} at line {}.".format(self.gotoLabel[:-1], lineNum))
					sys.exit()
			else:
				return (output, symTable, lineNum+1)
		elif self.keyword == 'input': # if input statement, update symtable and return
			try:
				i = float(scan.readInt())
			except:
				print('Illegal or missing input.')
				sys.exit()
			symTable[self.var] = i

			return (output, symTable, lineNum+1)
		elif self.keyword == 'no-op':
			return (output, symTable, lineNum+1)


def parseLine(lines, stmtList, symTable):
	""" sends each line to parseStmt, returns list of statements and updated symtable """

	if len(lines) == 0:
		return (stmtList, symTable)

	for i, line in enumerate(lines):
		if len(line) == 0:
			stmtList.append(Stmt(keyword="no-op"))
			continue
		lineNum = i + 1
		if line[0].endswith(':'): # if the line starts with a label, remove label and run parseStmt on rest of the line
			symTable[line[0]] = lineNum
			statement = parseStmt(line[1:], lineNum)
		else:
			statement = parseStmt(line, lineNum)
		stmtList.append(statement)

	return (stmtList, symTable)

def parseStmtPrintHelper(tokens, lineNum):
	""" helper function for parseStmt, handles print statement """

	curExpr = []
	exprList = []
	while len(tokens) != 0:
		s = tokens.pop(0)
		
		if len(tokens) == 0: # if only one thing left in tokens list
			curExpr.append(s)
			if s.endswith('"'):
				string = ' '.join(curExpr)
				exprList.append(parseExpr(list([string]), lineNum))
			else:
				exprList.append(parseExpr(curExpr, lineNum))
		
		if s == ',': # if current token is the end of an expr, then parse the expr and reset the curExpr list
			if curExpr[-1].endswith('"'):
				string = ' '.join(curExpr)
				exprList.append(parseExpr(list([string]), lineNum))
			else:
				exprList.append(parseExpr(curExpr, lineNum))
			curExpr = []
		elif s.endswith(','): # if current token is the end of an expr, then parse the expr and reset the curExpr list
			curExpr.append(s)
			if s[-2] == '"':
				string = ' '.join(curExpr).strip(',')
				exprList.append(parseExpr(list([string]), lineNum))
				curExpr = []
			elif curExpr[0][0] != '"':
				string = ' '.join(curExpr).strip(',')
				curExpr = string.split()
				exprList.append(parseExpr(curExpr, lineNum))
				curExpr = []
		else:
			curExpr.append(s)

	return exprList

def parseStmt(line, lineNum):
	""" identifies statement type and returns Stmt object """ 

	if line[0] == 'let' and line[2] == '=':
		e = parseExpr(line[3:], lineNum)
		return Stmt(keyword=line[0], var=line[1], exprs=e)
	elif line[0] == 'print':
		exprList = parseStmtPrintHelper(line[1:], lineNum)
		return Stmt(keyword=line[0], exprs=exprList)
	elif line[0] == 'if' and line[-2] == 'goto':
		e = parseExpr(line[1:-2], lineNum)
		l = line[-1] + ':'
		return Stmt(keyword=line[0], gotoLabel=l, exprs=e)
	elif line[0] == 'input':
		return Stmt(keyword=line[0], var=line[1])
	else:
		print("Syntax error on line {}.".format(lineNum))
		sys.exit()

def parseExpr(tokens, lineNum):
	""" parses each expression and returns Expr object """

	# if there is only one token
	if len(tokens) == 1: 
		x = tokens[0]
		if x[0].isalpha(): # if the token is a variable
			return Expr(op1=x, operator='var')
		elif x.isnumeric(): # if the token is a number
			return Expr(op1=float(x), operator='constant')
		else: # token must be a print string
			return Expr(op1=x.strip('"'), operator='printstring')

	# if actual expression
	if tokens[1] == '+':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='plus',op2=e2)
	elif tokens[1] == '-':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='minus',op2=e2)
	elif tokens[1] == '*':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='mult',op2=e2)
	elif tokens[1] == '/':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='div',op2=e2)
	elif tokens[1] == '<':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='lt',op2=e2)
	elif tokens[1] == '>':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='gt',op2=e2)
	elif tokens[1] == '<=':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='le',op2=e2)
	elif tokens[1] == '>=':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='ge',op2=e2)
	elif tokens[1] == '==':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='eq',op2=e2)
	elif tokens[1] == '!=':
		e1 = parseExpr(tokens[:1], lineNum)
		e2 = parseExpr(tokens[2:], lineNum)
		return Expr(op1=e1, operator='neq',op2=e2)
	else: 
		print("Syntax error on line {}.".format(lineNum))
		sys.exit()

if __name__ == '__main__':

	# reads file
	infile = sys.argv[1]
	with open(infile, "r+") as fin:
		# contents is a 2D list, where each row is a line, with line number zero indexed
		contents = []
		for line in fin.readlines():
			tokens = line.strip('\n').strip('\t').strip('\r').split()
			contents.append(tokens)

	# gets parsed list of statements and symtable with label-linenum pairs
	stmt_list, symTable = parseLine(contents, [], {})

	scan = Scanner()
	output = ''
	lineNum = 1
	stmt_list.insert(0,None) # 1 indexing
	while lineNum < len(stmt_list):
		# executes each statement
		output, symTable, lineNum = stmt_list[lineNum].perform(output=output, symTable=symTable, lineNum=lineNum, scan=scan)

	# prints final output
	print(output, end='')