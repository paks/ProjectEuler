#!/usr/bin/python

def permu(xs):
    if xs:
        r , h = [],[]
        for x in xs:
            if x not in h:
                ts = xs[:]; ts.remove(x)
                for p in permu(ts):
                    r.append([x]+p)
            h.append(x)
        return r
    else:
        return [[]]

for x in permu([0,1,2,3,4,5,6,7,8,9]):
    print x

#print permu([0,1,2,3,4,5,6,7,8,9])
print

