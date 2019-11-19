#! /usr/bin/env python3
'''
Daniel Xiong dxiong5@ucsc.edu id#1660652
tli.py
Pair Programming partner: Scott Zin nzin@ucsc.edu id#1679510
'''
import fileinput
import io
import sys
from sys import stdin

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr:
	def __init__(self,operator,op1=None,op2=None):
		self.op1 = op1
		self.operator = operator
		self.op2 = op2

	def __str__(self):
		if self.op2 == None:
			return self.operator + " " + str(self.op1)
		else:
			return str(self.op1) + " " + self.operator + " " +  str(self.op2)


	# evaluate this expression given the environment of the symTable
	def eval(self, symTable):
		if self.operator == "var":
			return symTable[self.op1]
		elif self.operator == "constant":
			return self.op1
		elif self.operator == "plus":
			return symTable[self.op1]+symTable[self.op2]
		elif self.operator == "minus":
			return symTable[self.op1]-symTable[self.op2]
		elif self.operator == "mult":
			return symTable[self.op1]*symTable[self.op2]
		elif self.operator == "div":
			return symTable[self.op1]/symTable[self.op2]
		elif self.operator == "lt":
			if self.op1 < self.op2:
				return 1
			else:
				return 0
		elif self.operator == "gt":
			if self.op1 > self.op2:
				return 1
			else:
				return 0
		elif self.operator == "le":
			if self.op1 <= self.op2:
				return 1
			else:
				return 0
		elif self.operator == "ge":
			if self.op1 >= self.op2:
				return 1
			else:
				return 0
		elif self.operator == "eq":
			if self.op1 == self.op2:
				return 1
			else:
				return 0
		elif self.operator == "neq":
			if self.op1 != self.op2:
				return 1
			else:
				return 0
		else:
			print("I should not be seeing this message.")
			return 0

def parseExpr(tokens):
	# if there is only one token
	if len(tokens) == 1: 
		x = tokens[0]
		if x[0].isalpha(): # if the token is a variable
			return Expr(op1=x, operator='var')
		elif x.isnumeric(): # if the token is a number
			return Expr(op1=float(x), operator='constant')
		else:
			print("Error message here maybe")
			return [Expr(operator='ExprError')]

	# if actual expression
	if tokens[1] == '+':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='plus',op2=e2)
	elif tokens[1] == '-':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='minus',op2=e2)
	elif tokens[1] == '*':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='mult',op2=e2)
	elif tokens[1] == '/':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='div',op2=e2)
	elif tokens[1] == '<':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='lt',op2=e2)
	elif tokens[1] == '>':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='gt',op2=e2)
	elif tokens[1] == '<=':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='le',op2=e2)
	elif tokens[1] == '>=':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='ge',op2=e2)
	elif tokens[1] == '==':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='eq',op2=e2)
	elif tokens[1] == '!=':
		e1 = parseExpr(tokens[:1])
		e2 = parseExpr(tokens[2:])
		return Expr(op1=e1, operator='neq',op2=e2)
	else: 
		...

# used to store a parsed TL statement
class Stmt :
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
				others = others + " " + str(self.exprs)
		elif self.keyword == 'if':
			others = others + " " + str(self.exprs)
			others = others + ' goto ' + self.gotoLabel
		elif self.keyword == 'input':
			others = others + " " + self.var
		return self.keyword + others

	# perform/execute this statement given the environment of the symTable
	def perform(self, symTable):
		print ("Doing: " + str(self))


def parseLine(lines, stmtList, symTable):
	if len(lines) == 0:
		return (stmtList, symTable)

	for i, line in enumerate(lines):
		lineNum = i + 1
		if line[0].endswith(':'):
			symTable[line[0]] = lineNum
			statement = parseStmt(line[1:], lineNum)
		else:
			statement = parseStmt(line, lineNum)
		stmtList.append(statement)

	return (stmtList, symTable)

def parseStmtPrintHelper(tokens):
	

def parseStmt(line, lineNum):
	if line[0] == 'let' and line[2] == '=':
		e = parseExpr(line[3:])
		return Stmt(keyword=line[0], var=line[1], exprs=e)
	elif line[0] == 'print':
		exprList = parseStmtPrintHelper(line[1:])
		return Stmt(keyword=line[0], exprs=exprList)
	elif line[0] == 'if' and line[-2] == 'goto':
		e = parseExpr(line[1:-2])
		return Stmt(keyword=line[0], gotoLabel=line[-1], exprs=e)
	elif line[0] == 'input':
		return Stmt(keyword=line[0], var=line[1])
	else:
		print("Syntax error on line", lineNum)


if __name__ == '__main__':
	# takes in user input
	inputList = []
	for line in stdin:
		if line == "\n":
			break
		inputList.append(float(line))

	# reads file
	infile = sys.argv[1]
	with open(infile, "r+") as fin:
		# contents is a 2D list, where each row is a line, with line number zero indexed
		contents = []
		for line in fin.readlines():
			line = line.replace('"', '')
			tokens = line.strip('\n').strip('\t').strip('\r').split()
			contents.append(tokens)
		
	print(contents)
	stmt_list, env = parseLine(contents, [], {})

	print(env)
	for stmt in stmt_list:
		print(stmt)





	