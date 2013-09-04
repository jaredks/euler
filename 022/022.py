with open('022.txt') as f:
    names = f.read()[1:-1]
    names = names.split('","')
    names.sort()

print sum(pos * sum(ord(letter) - ord('A') + 1 for letter in name) for pos, name in enumerate(names, start=1))
