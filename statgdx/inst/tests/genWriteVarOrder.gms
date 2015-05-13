$ontext
GAMS code to generate GDX "target" for wgdx test
$offtext

set i / i1 * i2 /;
alias (i,j);
scalar n / 10 /;
abort$[n <= card(i)] 'n too small';
variable v(i,j);

v.l(i,j) = n * ord(i) + ord(j);
v.lo(i,j) = v.l(i,j) - 1;
v.up(i,j) = v.l(i,j) * 2;
v.m(i,j) = ord(j);
v.scale(i,j) = ord(i);
execute_unload 'tVar2d';
