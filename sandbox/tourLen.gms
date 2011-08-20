* should be run like "gams thisFile --gdxIn=aaa --gdxOut=bbb"

$if not set gdxIn  then $abort "expected to run with --gdxIn=something"
$if not set gdxOut then $abort "expected to run with --gdxOut=something"

set cities;
parameter dist(cities,cities);

$gdxin %gdxIn%
$load cities dist
$gdxin

scalar tourLen / 0 /;
loop{cities,
  tourLen = tourLen + dist(cities,cities++1);
}

execute_unload '%gdxOut%', tourLen;
