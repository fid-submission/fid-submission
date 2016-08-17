from z3 import *
import utility as U


def and_process(o, dst, src):
    U.process_32(o, dst, src, "&")

def andb_process(o, dst, src):
    U.process_8(o, dst, src, "&")

def or_process(o, dst, src):
    U.process_32(o, dst, src, "|")

def xor_process(o, dst, src):
    U.process_32(o, dst, src, "^")

def not_process(o, dst):
    U.process_32(o, dst, dst, "~")
