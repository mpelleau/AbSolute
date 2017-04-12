#   12 solutions; difficult for many homotopy methods 

var    x1 >= -0.6, <= 6;
var    x2 >= -0.6, <= 0.7;
var    x3 >= -5, <= 6;

subject to

cons1:       5*x1^9 - 6*x1^5*x2^2 + x1*x2^4 + 2*x1*x3 = 0;

cons2:      -2*x1^6*x2 + 2*x1^2*x2^3 + 2*x2*x3 = 0;

cons3:      x1^2 + x2^2 = 0.265625;

#solve;
#display x1, x2, x3;
