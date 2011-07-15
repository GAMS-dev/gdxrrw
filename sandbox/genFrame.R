# play with data frame creation

src <- c("seattle","seattle","seattle","san-diego","san-diego","san-diego")
dst <- c("new-york", "chicago", "topeka","new-york", "chicago", "topeka")
v <- c(1,5,3,2,3,2)
flow <- data.frame(src,dst,v)

plant <- c("seattle","san-diego")
demand <- c("new-york", "chicago", "topeka")
# factor(plant)
# ordered(plant)

i <- factor(src,plant)
j <- factor(dst,demand)
flow2 <- data.frame(i,j,v)

slist <- list()
snam <- "i"
slist[[snam]] <- factor(src,plant)
snam <- "j"
slist[[snam]] <- factor(dst,demand)

flow3 <- data.frame(slist, v)

