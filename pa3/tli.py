#! /usr/bin/env python3
'''
Daniel Xiong dxiong5@ucsc.edu id#1660652
tli.py
Pair Programming partner: Scott Zin nzin@ucsc.edu id#1679510
'''
import fileinput
import sys

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
        	return 
        else:
            return 0

    def 

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


def parseLines():

if __name__ == '__main__':
	infile = sys.argv[1]
	outfile = sys.argv[2]
	f = open(infile, "r")
	fo = open(outfile, "w")

	