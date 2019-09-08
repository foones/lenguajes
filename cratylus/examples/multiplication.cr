
# Mul X^n Y^m =>> Z^(n * m)
#
# multiplies the exponents of X and Y into Z
#

Mul X Y => Copy X Y.
Mul => Erase.

Copy Y => Copy Y1 Z.
Copy   => Rename.

Rename Y1 => Rename Y.
Rename    => Del1.

Del1 X   => Mul.
Del1     => Mul.

Erase Y  => Erase.
Erase.

? Mul X^11 Y^9.
