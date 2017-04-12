# 
# TITLE: parabola
# 
# A very simple system: intersection of a circle and a parabola
# with 2 real solutions.
# 



# Domains
var y >= 0, <= 2;
var x >= 0, <= 10;

subject to
cons_parabola: x^2 = y;
cons_circle:   y^2 + x^2 = 2;

#solve;
#display x, y;
