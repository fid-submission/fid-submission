#usage :  

# let ail to dump union_1 and union_2; 
# diff faddr.txt fadd1.txt > diff
#  then use this to judge.

import sys, os

lines = []

with open ('diff') as f:
    lines = f.readlines()

sl = []
with open ('union_2.txt') as f:
    sl = f.readlines()

sl = map(lambda s: s.strip(), sl) 


for l in lines:
    if ">" in l:
        d = l.split()[1]
        if d in sl:
            print d
