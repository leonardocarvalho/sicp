import collections
VALUES = [1, 5, 10, 25, 50]
mm = collections.defaultdict(lambda: {0: 0})
mm[0] = {i: 1 for i in xrange(6)}

def coin_change(amount, coins=5):
    if amount < 0: return 0
    if coins not in mm[amount]:
        mm[amount][coins] = (coin_change(amount - VALUES[coins - 1], coins) +
                             coin_change(amount, coins - 1))
    return mm[amount][coins]


def coin_change_iter(value):
    for amm in xrange(value + 1):
        coin_change(amm)
    return coin_change(value)


print coin_change_iter(20000)
