x = input()
y = input()
z = input()
sum = x + y

if (sum > z) and not (z == 0):
    print(sum)
else:
    if z > sum:
        print(z)
    else:
        print(0)
