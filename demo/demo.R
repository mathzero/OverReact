library(tidyverse)


### source all relevant scripts
scripts=paste0("R/",list.files("R/"))
lapply(scripts,source)

### create dummy data
n=1000
dat=data.frame(x1=rnorm(n =n,mean = 0,sd = 1),
               x2=rnorm(n =n,mean = 0,sd = 1),
               x3=rnorm(n =n,mean = 0,sd = 1),
               x4=rnorm(n =n,mean = 0,sd = 1)
)

### add binary y variable
dat$y=sample(c(0,1),size = n,replace = T)
### add gaussian y variable
dat$y=rnorm(n =n,mean = 0,sd = 1)

### add categorical factor variable y variable
dat$cat=factor(sample(c("Case","Control"),size = n,replace = T))


# Create table one --------------------------------------------------------
rowvar_list=c("x1","x2","x3","x4")
tab1=crossTabMulti(dat = dat,rowvar_list = rowvar_list,colvar = "cat",confint = T,comma_thousands = )




# Run models --------------------------------------------------------------

myvars=c("x1","x2","x3","x4")
mymods=ModelMakerMulti(dat = dat,list_of_variables_of_interest = myvars,outcome = "y",sf = 2,
                       simpleround = T,
                       joint_adjustment_vars = myvars,cov_name_list = NULL)
mymods$plot_output
mymods$df_output

mod=glm(as.formula("y~x1"),family = "gaussian",data = dat)

summary(mod)

makeORTable(mod)


