set I / i1 * i3000 /;
alias (I,J);
parameter A(I,J);
A(I,J) = 1;
execute_unload 'aIJ';
