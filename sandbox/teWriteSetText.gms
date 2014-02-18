sets
  I /
    i1   'i1 associated text'
    i2   "i2's text here"
    i3   '  '
    i4
  /
  J /
    j1 'j1 text'
    j2 'j2 text'
    j3 'j3 text'
  /
  IJ(I,J) /
    i1.j1 'one.one'
    i2.j1
    i2.j2 'trailing blank '
    i3.j3 'three.three'
    i4.j3 ' '
  /
  ;
execute_unload 'teWriteSetText.gdx';
