
# Domains
var X1 >= -2, <= 2;
var X2 >= -2, <= 2;
var X3 >= -2, <= 2;
var X4 >= -2, <= 2;
var X5 >= -2, <= 2;

subject to

cons1 : 2*X1 + X2 + X3 + X4 + X5 - 6 = 0;
cons2 : X1 + 2*X2 + X3 + X4 + X5 - 6 = 0;
cons3 : X1 + X2 + 2*X3 + X4 + X5 - 6 = 0;
cons4 : X1 + X2 + X3 + 2*X4 + X5 - 6 = 0;
cons5 : X1 * X2 * X3 * X4 * X5 - 1 = 0;

# 
# 
# X1 + X2 + X3 + 2*X4 + X5 - 6 = 0,
# 3*X2 + X3 + X4 + X5 - 6 = 0,
# 4*X3 + X4 + X5 - 6 = 0,
# 5*X4 + X5 - 6 = 0,
# X5^5 - 24*X5^4 + 216*X5^3 - 864*X5^2 + 1296*X5 - 625 = 0
# 


#solve;
#display X1, X2, X3, X4, X5;
