# Counting repeatdly... do not work
import sys

v = int(sys.argv[1])

VALUES = [1, 5, 10, 25, 50]
mm = {}


def coin_change(amount, coins=5):
    if amount == 0: return 1
    return mm.get(amount, 0)


def coin_change_iter(value):
    for amm in xrange(value + 1):
        mm[amm] = sum(coin_change(amm - c) for c in VALUES)
    return mm[value]

print coin_change_iter(v)
