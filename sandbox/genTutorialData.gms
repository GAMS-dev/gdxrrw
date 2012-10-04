* this model generates the data for the tutorial in inst/doc

Sets
     i   canning plants   / seattle, san-diego /
     j   markets          / new-york, chicago, topeka / ;

Parameters
     a(i)  capacity of plant i in cases
       /    seattle     350
            san-diego   600  /

     b(j)  demand at market j in cases
       /    new-york    325
            chicago     300
            topeka      275  / ;

Parameter c(i,j)  transport cost in thousands of dollars per case ;
Table c(i,j)
                  new-york       chicago      topeka
    seattle          2.5           1.7          1.8
    san-diego        2.5           1.8          1.4  ;

c(i,j) = 90 * c(i,j) / 1000 ;
execute_unload 'inputs';
