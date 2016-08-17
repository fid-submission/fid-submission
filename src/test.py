lines = []

with open("final.s") as f:
    lines = f.readlines()


def help(l):
    l = l.strip()
    if l.endswith(":"):
	return False
    return True
lines = filter(help, lines)

with open("final.s", 'w') as f:
    map(lambda l : f.write(l), lines)
