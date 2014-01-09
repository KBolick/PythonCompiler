def f():
 print("called f")
 return 1

def g():
 print("called g")
 return 0

a = [[10,20],[30,40],[50,60]]

a[f()][g()] += 30

print(a[f()])

