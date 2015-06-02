
# core.matrix.complex

A core.matrix imlementation for complex valued matrices.


## Structure

Matrices are implemented as a real/imaginary matrix pairs, and by default operations are outsourced to the underlying matrices.
This implies that users can use whatever core.matrix implementations they wish for the underlying matrices.

For example, matrix multiplication can be treated as:

    (A + Bi) * (C + Di) = (AC - BD) + (AD + BC)i

Addition is even simpler:

    (A + Bi) + (C + Di) = (A + C) + (B + D)i

This demonstrates that in many computational instances, we can benefit from performance characteristics of the underlying matrix implementations.
More tailored algorithms and routines can of course be added as needed, as well as functions which would only pertain to complex valued matrices.


