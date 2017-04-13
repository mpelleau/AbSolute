

# Domains
var x >= -3, <= 6;


subject to
cons1 : x^6/200 - 0.038*x^5 + 0.005*x^4 + (2/5)*x^3 - (17/50)*x^2 - 1.112*x + 24/25 = 0;



#solve;
#display x;
