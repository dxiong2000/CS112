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
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1
        else:
            return self.op1 + " " + self.operator + " " +  self.op2


    # evaluate this expression given the environment of the symTable
    def eval(self, symTable):
        if self.operator == "var":
            return symTable[self.op1]
        elif self.operator == "constant":
        	return symTable[self.op1]
        elif self.operator == "plus":
        	return symTable[self.op1]+symTable[self.op2]
        elif self.operator == "minus":
        	return symTable[self.op1]-symTable[self.op2]
        elif self.operator == "mult":
        	return symTable[self.op1]*symTable[self.op2]
        elif self.operator == "div":
        	return symTable[self.op1]/symTable[self.op2]
        elif self.operator == "lt":
        	if 
        else:
            return 0

def parseExpr(tokens):
    	if len(tokens) == 1:
    		x = tokens[0]
    		if x[0].isalpha():
    			return Expr(op1=x, operator='var')
    		elif x.isnumeric():
    			return Expr(op1=float(x), operator='constant')
    		else:
    			
    

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs

    def __str__(self):
        others = ""
        for exp in self.exprs:
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable):
        print ("Doing: " + str(self))


def runParseLine(lines, stmtList, symTable, lineNum):
	if len(lines) == 0:
		return (stmtList, symTable)

	head = lines.pop(0)
	statement, env = parseLine(head, symTable, lineNum)
	stmtList.append(statement)
	lineNum += 1
	runParseLine(lines, stmtList, env, lineNum)

def parseLine(line, symTable, lineNum):
	head = line.pop(0)
	if head.endswith(':'):
		symTable[head] = lineNum
		parseLine(line, symTable, lineNum)
	else:
		return (parseStmt(head, line, lineNum), symTable)

def parseStmt(head, rest, lineNum):


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
		contents = [line.strip('\n').strip('\t').strip('\r').split() for line in fin.readlines()]

	print(contents)
	stmt_list, env = runParseLines(contents, [], {}, 1)




	