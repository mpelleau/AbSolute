# 
# 
# TITLE : robot kinematics problem
# 
# ROOT COUNTS :
# 
# total degree : 64
# mixed volume : 64
# 
# REFERENCES :
# 
# Bellido, A.M.:" Construction de fonctions d'iteration pour le calcul
#   simultane des solutions d'equations et de systemes d'equations
#   algebriques", These 1992, Universite Paul Sabatier, Toulouse.
# Anne-Mercedes Bellido:
#   "Construction of iteration functions for the simultaneous computation
#    of the solutions of equations and algebraic systems"
#   Numerical Algorithms Vo. 6, Nos 3-4, pages 317--351, 1992.
# 
# Inbox #solver => 8 real roots in 70s (Sparc 2 200MHz)
# 


# Domains
var z1 >= -1.0e8, <= 1.0e8;
var z2 >= -1.0e8, <= 1.0e8;
var z3 >= -1.0e8, <= 1.0e8;
var z4 >= -1.0e8, <= 1.0e8;
var z5 >= -1.0e8, <= 1.0e8;
var z6 >= -1.0e8, <= 1.0e8;
var z7 >= -1.0e8, <= 1.0e8;
var z8 >= -1.0e8, <= 1.0e8;
var z9 >= -1.0e8, <= 1.0e8;

subject to
# 
# (z1-6)^2 + z2^2 + z3^2 = 104,
# z4^2 + (z5-6)^2 + z6^2 = 104,
# z7^2 + (z8-12)^2 + (z9-6)^2  = 80,
# 
# z1*(z4-6) + z5*(z2-6) + z3*z6 = 52,
# z1*(z7-6) + z8*(z2-12) + z9*(z3-6) = -64,
# z4*z7 + z8*(z5-12) + z9*(z6-6) - 6*z5 = -32,
# 
# 2*(z2+z3-z6) = z4 + z5 + z7 + z9 - 18,
# z1 + z2 + 2*(z3+z4+z6-z7) + z8 - z9 = 38,
# z1 + z3 + z5 - z6 + 2*(z7-z8-z4) = -8
# 

# 
# ORIGINAL SYSTEM:
# 

cons1 : z1^2 + z2^2 + z3^2 - 12*z1 = 68;
cons2 : z4^2 + z5^2 + z6^2 - 12*z5 = 68;
cons3 : z7^2 + z8^2 + z9^2 - 24*z8 - 12*z9 = -100;

cons4 : z1*z4 + z2*z5 + z3*z6 - 6*z1 - 6*z5 = 52;
cons5 : z1*z7 + z2*z8 + z3*z9 - 6*z1 - 12*z8 - 6*z9 = -64;
cons6 : z4*z7 + z5*z8 + z6*z9 - 6*z5 - 12*z8 - 6*z9 = -32;

cons7 : 2*z2 + 2*z3 - z4 - z5 - 2*z6 - z7 - z9 = -18;
cons8 : z1 + z2 + 2*z3 + 2*z4 + 2*z6 - 2*z7 + z8 - z9 = 38;
cons9 : z1 + z3 - 2*z4 + z5 - z6 + 2*z7 - 2*z8 = -8;

#solve;
#display z1, z2, z3, z4, z5, z6, z7, z8, z9;
