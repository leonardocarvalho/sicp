VALUES = [1, 5, 10, 25, 50]


def coin_change(amount, coins=5):
    if amount == 0: return 1
    if amount < 0: return 0
    if coins == 0: return 0
    return coin_change(amount - VALUES[coins - 1], coins) + coin_change(amount, coins - 1)


print coin_change(200)
