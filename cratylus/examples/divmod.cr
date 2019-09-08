
# DivMod X^n Y^m =>> Q^(n div m) R^(n mod m)
#
# calculates the quotient and the remainder
#

DivMod X Y => Copy X Y.
DivMod => End.

Copy Y => Copy Y1 Y2.
Copy => Sub.

Sub X Y2 => Sub.
Sub Y2 => Rem Y2.
Sub => Continue Q.

Continue Y1 => Continue Y.
Continue => DivMod.

Rem Y1 Y2 => Rem.
Rem Y1 => Rem R.
Rem.

End Y => End.
End.

? DivMod X^62 Y^11.

