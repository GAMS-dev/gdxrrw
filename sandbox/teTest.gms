sets
  I / i1 * i4 /
  J /
    j1 'j1 text'
    j2 'j2 text'
    j3 'j3 text'
  /
  c 'cities' /
    berlin 'city of airport delays'
    paris  'city of light'
    vienna 'city of dreams'
  /
  IJ(I,J) /
    i1.j1 'one.one'
    i1.j3 'one.three'
    i2.j2 'two.two'
    i2.j3 'two.three'
    i3.j3 'three.three'
  /
  IJc(I,J,c) /
    i1.j1.berlin 'eins eins tempelhof'
    i1.j3.berlin 'eins drei tempelhof'
    i2.j2.paris  'deux deux orly'
    i2.j3.paris  'deux trois orly'
    i3.j3.vienna 'drei drei schwechat'
  /
  ;
parameter
  AIc(I,c) /
    i1.berlin 11
    i1.paris  12
    i2.paris  22
    i2.vienna 23
    i3.vienna 33
  /
  AIJc(I,J,c) /
    i1.j1.berlin 111
    i1.j3.berlin 131
    i2.j2.paris  222
    i2.j3.paris  232
    i3.j3.vienna 333
  /
  ;
