from z3 import *
import utility as U

def mov_process(o, dst, src):
    U.process_32(o, dst, src, "mov")

def movb_process(o, dst, src):
    U.process_8(o, dst, src, "mov")


def movsbl_process(o, dst, src):
    U.process_32(o, dst, src, "movsbl")


def movzbl_process(o, dst, src):
    U.process_32(o, dst, src, "movzbl")


def xchg_process(o, dst, src):
    if dst == src:
        return

    t = dst
    U.process_32(o, dst, src, "mov")
    U.process_32(o, src, t, "mov")
