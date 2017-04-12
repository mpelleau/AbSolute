# Domains
var x >= 0, <= 1000;
var y >= 0, <= 1000;
var z >= 0, <= 1000;
var w >= 0, <= 1000;
var t >= 0, <= 1000;

var u1 >= -1.0e8, <= 1.0e8;
var u2 >= -1.0e8, <= 1.0e8;
var u3 >= -1.0e8, <= 1.0e8;
var u4 >= -1.0e8, <= 1.0e8;


# Constants
param a := 9;
param b := 4;
param c := 5;
param d := 16;

subject to
cons1 : y*u1 + u3 = a;
cons2 : x*u1 + u3 = b;
cons3 : w*u2 + u4 = c;
cons4 : z*u2 + u4 = d;
cons5 : 2*u4*u1 + 2*u3*u2 = 9*t;

cons6 : u1 = z+w;
cons7 : u2 = x+y;
cons8 : u3 = w*z;
cons9 : u4 = x*y;



#  PB original
# y*z + z*w + w*y = a,
# z*x + x*w + w*z = b,
# w*x + x*y + y*w = c,
# x*y + y*z + z*x = d,
# 2*x*y*z + 2*y*z*w + 2*z*w*x + 2*w*x*y = 9*t
# 

#solve;
#display t, u1, u2, u3, u4, w, x, y, z;
