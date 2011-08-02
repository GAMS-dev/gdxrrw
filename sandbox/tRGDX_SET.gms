set
  I / i1 * i20 /
  J / j1 * j50 /
  K / k1 * k40 /
  ijk(I,J,K)
  ;

ijk(I,J,K) = yes$[uniform(0,1) < .02];
execute_unload 'ijk', I, J, K, ijk;

* now write out .csv files with the same info: R will read and compare
file ifp / 'I.csv' /;
put ifp '"I"';
loop{I,
  put / '"'I.tl:0'"';
};
putclose;

file jfp / 'J.csv' /;
put jfp '"J"';
loop{J,
  put / '"'J.tl:0'"';
};
putclose;

file kfp / 'K.csv' /;
put kfp '"K"';
loop{K,
  put / '"'K.tl:0'"';
};
putclose;

file fp / 'ijk.csv' /;
put fp  '"I","J","K"';
loop{ijk(I,J,K),
  put / '"'I.tl:0'","'J.tl:0'","'K.tl:0'"';
}

file lg / '' /; put lg;
put / 'card(I) = ', card(I):0;
put / 'card(J) = ', card(J):0;
put / 'card(K) = ', card(K):0;
put / 'card(ijk) = ', card(ijk):0;
put / 'density = ', (card(ijk)/[card(I)*card(J)*card(K)]) /;
