import math


def f(x):
    if abs(x) < 1e-5:
        return 6.166177
    elif x < 0:
        # return x * 30 * math.pi / 180
        # return math.cos(x * 30 * math.pi / 180)
        return (2/3) ** (48 / 7) * -abs(x - 2) ** (1/5) * math.cos(x * 30 * math.pi / 180)
    elif x < 1:
        return math.sin(x * 60 * math.pi / 180)
        return -abs(x - 1) ** (1/3) * math.sin(x * 60 * math.pi / 180)
    else:
        return math.sin(x * 60 * math.pi / 180)
        return (x - 1) ** (1/3) * math.sin(x * 60 * math.pi / 180)

sum = 0

for i in range(10, 48 + 1):
    sum += f(-6.6 + i * 0.33)
    print(f(-6.6 + i * 0.33))

print(sum)