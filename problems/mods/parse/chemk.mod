# Kearfott
# 
#  10. Combustion chemistry problem
# 



# Domains

var x1 >= 0, <= 1;
var x2 >= 0, <= 1;
var x3 >= 0, <= 1;
var x4 >= 0, <= 1;


subject to

cons1 : x1^2 - x2 = 0;
cons2 : x4^2 - x3 = 0;

cons3 : 2.177e7*x2 - 1.697e7*x2*x4 + 0.55*x1*x4 + 0.45*x1 = x4;

cons4 : 1.585e14*x2*x4 + 4.126e7*x1*x3 - 8.282e6*x1*x4 + 2.284e7*x3*x4 - 1.918e7*x3 + 48.4*x4 = 27.73;

# 
# ,2.177e7*x1^2 - 1.697e7*x1^2*x4 + 0.55*x1*x4 + 0.45*x1 - x4 = 0,
# 
# 1.585e14*x1^2*x4 + 4.126e7*x1*x4^2 - 8.282e6*x1*x4 + 2.284e7*x4^3
# - 1.918e7*x4^2 + 48.4*x4 - 27.73 = 0;
# 

#solve;
#display x1, x2, x3, x4;
