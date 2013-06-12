$ontext
GAMS code to generate GDX "target" for wgdx test of variable types
binary, integer, positive, negative, free, sos1, sos2, semicont, semiint
$offtext

set i / i1 * i10 /;
binary   variable v_binary(i)    'text for v_binary';
integer  variable v_integer(i)   'CHECK CASING';
positive variable v_positive(i)  'try some special values';
negative variable v_negative(i)  '';
free     variable v_free(i)      'WHAT if we use a long text with special chars __ %% -- @@  ?? !! jj';
sos1     variable v_sos1(i)      'v_sos1';
sos2     variable v_sos2(i)      'v_sos2';
semicont variable v_semicont(i)  'v_semicont';
semiint  variable v_semiint(i)   'v_semiint text';

v_binary.l ('i1') = 1;
v_binary.m ('i2') = 0.5;
v_binary.lo('i3') = 1;
v_binary.up('i4') = 0;
* v_binary.scale('i5') = 10; for discrete var must set .prior
v_binary.prior('i5') = 10;

v_integer.l ('i1') = 1.5;
v_integer.m ('i2') = 0.5;
v_integer.lo('i3') = 3;
v_integer.up('i4') = 16;
*v_integer.scale('i5') = 1;
v_integer.prior('i5') = 1000;

* note the reordering
v_positive.l ('i5') = 322;
v_positive.m ('i4') = -1024;
v_positive.lo('i3') = -INF;
v_positive.up('i2') = 0;
v_positive.scale('i1') = 10;

* note the reordering
v_negative.l ('i5') = 525;
v_negative.m ('i4') = eps;
v_negative.lo('i3') = 1;
v_negative.up('i2') = -1;
v_negative.scale('i1') = 0.5;

v_free.l ('i1') = -12.5;
v_free.m ('i2') = 0.5;
v_free.lo('i3') = 3;
v_free.up('i4') = -8;
v_free.scale('i5') = 512;

v_sos1.l ('i1') = 123;
v_sos1.m ('i2') = -10.5;
v_sos1.lo('i3') = 2;
v_sos1.up('i4') = 10;
v_sos1.prior('i5') = 100;

v_sos2.l ('i1') = 123;
v_sos2.m ('i2') = -10.5;
v_sos2.lo('i3') = 2;
v_sos2.up('i4') = 10;
v_sos2.prior('i5') = 100;

v_semicont.l ('i1') = 10.5;
v_semicont.m ('i2') = .875;
v_semicont.lo('i3') = 13;
v_semicont.up('i4') = 1000;
* v_semicont.scale('i5') = 999;
v_semicont.prior('i5') = 999;

v_semiint.l ('i1') = 10.5;
v_semiint.m ('i2') = .875;
v_semiint.lo('i3') = 13;
v_semiint.up('i4') = 1000;
* v_semiint.scale('i5') = 1;
v_semiint.prior('i5') = 1e5;


execute_unload 'tVarTypes';
