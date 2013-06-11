$ontext
GAMS code to generate GDX "target" for wgdx test of variable types
binary, integer, positive, negative, free, sos1, sos2, semicont, semiint
$offtext

set i / i1 * i10 /;
binary variable v_binary(i) 'text for v_binary';
integer variable v_integer(i)  'CHECK CASING';

v_binary.l ('i1') = 1;
v_binary.m ('i2') = 0.5;
v_binary.lo('i3') = 1;
v_binary.up('i4') = 0;
*v_binary.scale('i5') = 1;
v_binary.lo('i5') = eps;

v_integer.l ('i1') = 1.5;
v_integer.m ('i2') = 0.5;
v_integer.lo('i3') = 3;
v_integer.up('i4') = 16;
*v_integer.scale('i5') = 1;
v_integer.lo('i5') = eps;

execute_unload 'tVarTypes';
