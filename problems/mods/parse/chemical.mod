# **************************************************************************
# File: Morgan
#       Chemical Equilibrium System
# 
# Reference :
# 
# @article{                 morgan-singular,
#    author       = "Alexander P. Morgan and Andrew J. Sommese
#                                        and Charles C. Wampler",
#    title        = "{C}omputing {S}ingular {S}olutions to {P}olynomial {S}ystems",
#    year         = 1992,
#    journal      = "Advances in Applied Mathematics",
#    volume       = 13,
#    pages        = "305--327"
# }
# 
# Solutions:
#        Z1 in [+131.747564 , +131.747565]
#        Z2 in [-24.6278783 , -24.6278782]
#        Z3 in [+212.903082 , +212.903083]
# 
#        Z1 in [+124.764348 , +124.764349]
#        Z2 in [+25.2854606 , +25.2854607]
#        Z3 in [+224.69353 , +224.693531]
# 
# ***************************************************************************



# Domains
var z1 >= -1000, <= 1000;
var z2 >= -1000, <= 1000;
var z3 >= -1000, <= 1000;

subject to
cons1 : 14*z1^2 + 6*z1*z2 + 5*z1 - 72*z2^2 - 18*z2 = 850*z3 - 2.0e-9;
cons2 : 0.5*z1*z2^2 + 0.01*z1*z2 + 0.13*z2^2 + 0.04*z2 = 4.0e4;
cons3 : 0.03*z1*z3 + 0.04*z3 = 850;


#solve;
#display z1, z2, z3;
