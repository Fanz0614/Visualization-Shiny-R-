x <-range(casualty$Age)
x <- seq(x[1], x[2], length.out=104)    
y <- range(casualty$Time)
y <- seq(y[1], y[2], length.out=24)

z <-  matrix(data=0, nrow=length(x), ncol=length(y))
i <- 0
for(i in 1:length(casualty$Age))
{
  a <-  casualty$Age[i]
  b <-  casualty$Time[i]
  z[a+1,b+1] <-  z[a+1,b+1] + 1
}

z2 = matrix(data=0, nrow=length(x), ncol=length(y))
i <- 0
for(i in 1:length(casualtySerious$Age))
{
  a <-  casualtySerious$Age[i]
  b <-  casualtySerious$Time[i]
  z2[a+1,b+1] <-  z2[a+1,b+1] + 1
}

z3 = matrix(data=0, nrow=length(x), ncol=length(y))
i <- 0
for(i in 1:104){
  for (j in 1:24){
    z3[i,j] <-  z2[i,j]/z[i,j]
  }
}

write.csv(z, file = "./3D_z_axis/z_all(a).csv")
write.csv(z3, file = "./3D_z_axis/z_all(b).csv")