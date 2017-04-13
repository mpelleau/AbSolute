# 
# 
# 


# Domains
var x >= -705, <= 700;
var y >= 0, <= 1.0e160;

subject to
cons1 : (1+y^2)/sqrt(1+y^3) = 1;
cons2 : exp(x)*y^2 = 1;

#solve;
#display x, y;
