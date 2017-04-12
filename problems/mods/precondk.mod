# Domains
var x >= 0.9, <= 1.2;
var y >= -0.1, <= 0.1;

subject to
cons1 : x^2 - y^2 = 1;
cons2 : 2*x*y = 0;

#solve;
#display x, y;
