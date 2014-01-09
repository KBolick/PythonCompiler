x = 3 if 1 == 2 else 4
y = 3 if False else 8 - 7/2
global a, b
b = 3
def foo():
    global a
    a = 3
    b = 2
print(a)
print(b)

