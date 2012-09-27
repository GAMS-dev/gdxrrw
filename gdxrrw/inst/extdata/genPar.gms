set I / i1 * i30 /;
alias (I,J);
parameter A(I,J);
A(I,J) = 1;
* execute_unload 'aIJ';
execute_unload 'aBare', A;
execute_unload 'aRelaxed', A, I, J;
execute_unload 'aFull';
