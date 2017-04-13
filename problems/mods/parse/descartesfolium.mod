# -----------------------------------------------------------------
#  Reference :
# @inproceedings{OlderConstraint,
# author    = "W. Older and A. Vellino",
# title     = "{C}onstraint {A}rithmetic on {R}eal {I}ntervals",
# booktitle = "Constraint Logic Programming: Selected Research",
# year      = 1993,
# editor    = "Fr{\'{e}}d{\'{e}}ric Benhamou and Alain Colmerauer",
# publisher = "MIT Press"
# }
# -----------------------------------------------------------------


# Domains
var X >= -100, <= 100;
var Y >= -100, <= 100;

subject to
cons1 : (X^2)/Y + (Y^2)/X = 2;
cons2 : Y = exp(-X);


# 
# Possible redundant constraint:
# 
#    X**3 + exp(-3*X) = 2*X*exp(-X)
# 

#solve;
#display X, Y;
