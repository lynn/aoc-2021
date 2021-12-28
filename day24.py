# Hello! Oops!!! I ended up accidentally solving this in Python.
# Day 24 is a "what does this low-level code do?" style problem, so
# this is more of a notepad that I gradually translated my input into,
# than a program that reads the input and produces the output.

z = 0

def f(w, A, B, C):
    global z
    x = z % 26 + B
    z //= A
    if x != w:
        z = 26*z + w + C

steps = [
    (1, 14, 12), # push(A=w+12)
    (1, 10, 9), # push(B=w+9)
    (1, 13, 8), # push(C=w+8)
    (26, -8, 3), # expect(C-8)
    (1, 11, 0), # push(D=w+0)
    (1, 11, 11), # push(E=w+11)
    (1, 14, 10), # push(F=w+10)
    (26, -11, 13), # expect(F-11)
    (1, 14, 3), # push(G=w+3)
    (26, -1, 10), # expect(G-1)
    (26, -8, 10), # expect(E-8)
    (26, -5, 14), # expect(D-5)
    (26, -16, 6), # expect(B-16)
    (26, -6, 5), # expect(A-6)
]

# *** 39999698799429

stack = []
for i in range(len(steps)):
    if steps[i][0] == 1:
        stack.append(i)
    else:
        j = stack.pop()
        si = steps[i]
        steps[i] += steps[j] # aaadfgjhk merry christmas
        steps[j] += si
print(steps)

maxcode = ''
for (A,B,C,Am,Bm,Cm) in steps:
    if A == 1:
        choice = 9
        while choice + C + Bm > 9:
            choice -= 1
        maxcode += str(choice)
        stack.append(choice + C)
    else:
        maxcode += str(stack.pop() + B)

mincode = ''
for (A,B,C,Am,Bm,Cm) in steps:
    if A == 1:
        choice = 1
        while choice + C + Bm < 1:
            choice += 1
        mincode += str(choice)
        stack.append(choice + C)
    else:
        mincode += str(stack.pop() + B)

print(maxcode)
print(mincode)

# for (A,B,C,Am,Bm,Cm) in steps:
#     oldz = z
#     for k in range(1, 10): f(k,A,B,C); print(k, z); z = oldz
#     print('target:', z % 26 + B)
#     w = int(input())
#     f(w,A,B,C)
# 
# n = 10 ** 14
# for _ in range(10):
#     z = 0
#     n -= 1
#     for (A,B,C,Am,Bm,Cm), d in zip(steps, str(n)):
#         f(int(d), A,B,C)
#     print(n, z)
#     if z == 0:
#         print(n)
# 
# print(z)
