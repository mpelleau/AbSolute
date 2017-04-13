


# Domains
var x1 >= -10, <= 10;
var x2 >= -10, <= 10;
var x3 >= -10, <= 10;
var x4 >= -10, <= 10;

subject to
cons1 : x1 + x2 + x3 + x4 - 1 = 0;
cons2 : x1 + x2 - x3 + x4 - 3 = 0;
cons3 : x1^2 + x2^2 + x3^2 + x4^2 - 4 = 0;
cons4 : x1^2 + x2^2 + x3^2 + x4^2 - 2*x1 - 3 = 0;

# , 2*x1 = 1, x3 + 1 = 0


#solve;
#display x1, x2, x3, x4;
