# Domains
var x1 >= -2, <= 3;
var x2 >= -2, <= 3;

subject to

cons1 : 6*x1^5-25.2*x1^3+24*x1-6*x2 = 0;
cons2 : 12*x2-6*x1 = 0;

#solve;
#display x1, x2;
