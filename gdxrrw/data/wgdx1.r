# sc is a structure representing a GAMS scalar, i.e. a 0-dimensional parameter
sc <- list(name='c',type='parameter',form='full',val=exp(1));

uels <- list(c('seattle','san-diego','new-york','chicago','topeka'));
# ss <- list(name='I',type='set',form='full',dim=1,uels=uels,val=array(c(1,1,0,0,0)));
sI <- list(name='I',type='set',form='full',uels=uels,val=array(c(1,1,0,0,0)));

# sI <- list(name='I',type='set',form='full',uels=uels,val=array(c(1:2),c(2,1)));
# sI <- list(name='I',type='set',form='sparse',uels=uels,val=array(c(1:2),c(2,1)));
# sI <- list(name='I',type='set',form='full',uels=uels,val=array(c(1:2)));
# sJ <- list(name='J',type='set',form='full',uels=uels);
# sAll <- list(name='Ix',type='set',uels=uels);
