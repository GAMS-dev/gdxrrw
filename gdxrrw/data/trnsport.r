# We create a GDX file to mimic the data in the trnsport model in the GAMS model library

# sf is a structure representing a GAMS scalar, i.e. a 0-dimensional parameter
sf <- list(name='f',type='parameter',form='full',val=90,ts='freight in dollars per case per thousand miles');

# convenient to create the set of all UELs we will use in this GDX example
uels <- list(c('seattle','san-diego','new-york','chicago','topeka'));
si <- list(name='i',ts='canning plants',type='set',dim=1,
           form='full',uels=uels,val=array(c(1,1)));
sj <- list(name='j',ts='markets',type='set',dim=1,
           form='full',uels=uels,val=array(c(0,0,1,1,1)));
sa <- list(name='a',ts='capacity of plant i in cases',type='parameter',dim=1,
           form='sparse',uels=uels,val=matrix(c(1,2,350,600),nrow=2));
sb <- list(name='b',ts='demand at market j in cases',type='parameter',dim=1,
           form='sparse',uels=uels,val=matrix(c(3,4,5,325,300,275),nrow=3));
d <- matrix(c(1,3, 2.5,
              1,4, 1.7,
              1,5, 1.8,
              2,3, 2.5,
              2,4, 1.8,
              2,5, 1.4),
              nrow=6,ncol=3,byrow=TRUE);
sd <- list(name='d',ts='distance in thousands of miles',type='parameter',dim=2,
           form='sparse',uels=c(uels,uels), val=d);
