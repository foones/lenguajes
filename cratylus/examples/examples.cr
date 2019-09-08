
# Add X^n Y^m =>> Z^(n + m).
#
# adds the exponents of X and Y into Z
#
Add X => Z Add.
Add Y => Z Add.
Add => 1.    # equivalently: "Add."

? Add X^9 Y^7.    # =>> Z^16

# Erase X^n =>> 1.
#
# factors X out
#
Erase X => Erase.
Erase.

? Erase X^9 Y^7.    # =>> Y^7

# Copy X^n =>> Y^n Z^n.
#
# copies the exponent of X into Y and Z
#
Copy X => Y Z Copy.
Copy.

? Copy X^9.   # =>> Y^9 Z^9

