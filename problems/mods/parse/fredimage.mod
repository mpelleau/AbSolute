# Domains
var x0 >= 1, <= 1;
var x1 >= -1, <= 1;
var x2 >= -1, <= 1;
var x3 >= -1, <= 1;
var x4 >= -1, <= 1;

var y0 >= 0, <= 0;
var y1 >= -1, <= 1;
var y2 >= -1, <= 1;
var y3 >= -1, <= 1;
var y4 >= -1, <= 1;

var z0 >= 0, <= 0;
var z1 >= -1, <= 1;
var z2 >= -1, <= 1;
var z3 >= -1, <= 1;
var z4 >= -1, <= 1;

var K >= 0.305, <= 0.307;

subject to

cons1 : x1^2 + y1^2 + z1^2 = 1;
cons2 : x2^2 + y2^2 + z2^2 = 1;
cons3 : x3^2 + y3^2 + z3^2 = 1;
cons4 : x4^2 + y4^2 + z4^2 = 1;

cons5 : x0*x1 + y0*y1 + z0*z1 = 1 - K/2;
cons6 : x0*x2 + y0*y2 + z0*z2 = 1 - K/2;
cons7 : x0*x3 + y0*y3 + z0*z3 = 1 - K/2;
cons8 : x0*x4 + y0*y4 + z0*z4 = 1 - K/2;

cons9 : x1*x2 + y1*y2 + z1*z2 = 1 - K/2;
cons10 : x2*x3 + y2*y3 + z2*z3 = 1 - K/2;
cons11 : x3*x4 + y3*y4 + z3*z4 = 1 - K/2;
cons12 : x4*x1 + y4*y1 + z4*z1 = 1 - K/2;

# 
# 
# x1^2 + y1^2 + z1^2 = 1,
# x2^2 + y2^2 + z2^2 = 1,
# x3^2 + y3^2 + z3^2 = 1,
# x4^2 + y4^2 + z4^2 = 1,
# 
# x1   = 1 - K/2,
# x2   = 1 - K/2,
# x3   = 1 - K/2,
# x4   = 1 - K/2,
# 
# x1*(x2-1) + y1*y2 + z1*z2 = 0,
# x2*(x3-1) + y2*y3 + z2*z3 = 0,
# x3*(x4-1) + y3*y4 + z3*z4 = 0,
# x4*(x1-1) + y4*y1 + z4*z1 = 0
# 


#solve;
#display K, x0, x1, x2, x3, x4, y0, y1, y2, y3, y4, z0, z1, z2, z3, z4;
