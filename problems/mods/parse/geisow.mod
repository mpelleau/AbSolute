# Domains
var x >= -1, <= 3;
var y >= -2, <= 2;


subject to
cons1 : 36*x^2 - 24*x^3 - 38*x - 12*x*y^2 + 11*y^2 = 0;
cons2 : 22*x*y - 12*x^2*y + 6*y - 16*y^3 = 0;

#solve;
#display x, y;
