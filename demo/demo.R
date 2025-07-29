library(tidyverse)
library(OverReact)


### source all relevant scripts
scripts=paste0("R/",list.files("R/"))
lapply(scripts,source)

set.seed(123)
### create dummy data
n=100000
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

### add another categorical factor variable y variable
dat$abcat=factor(sample(c("A some text","B more text","3"),size = n,replace = T))

# add some random NAs to test
for(i in 1:50){
  set.seed(i)
  dat[sample(1:n,size = 1,replace = F),sample(1:7,size = 1,replace = F)] <- NA
}

# add some NAs into the abcat
dat$abcat[sample(1:n,size = 50,replace = F)] <- NA
dat$cat[sample(1:n,size = 100,replace = F)] <- NA

# add some signal to x1
dat$x1[dat$cat=="Case" & !is.na(dat$cat)] <- dat$x1[dat$cat=="Case"& !is.na(dat$cat)]+1


# Create table one --------------------------------------------------------

rowvar_list=c("x1","x2","x3","x4","abcat","x3","x4","xMessedUp")
colvar = "cat"
rowvar_names=c("X1","X2","X3","X4","ABCAT!","X3","X4",NA)
rowvar_names <- as.list(rowvar_names)
names(rowvar_names) <- rowvar_list
options(scipen = 999)
# create table one
tab1=tableOne(dat = dat |> mutate(xMessedUp=0),
              rowvars = rowvar_list,colvar = "cat",statistical_test = T,confint = T,cov_names = rowvar_names,include_percentages = F,
              summary_stat = "mean",formatPvalsForEpiPaper = T,includeNAsColvar = T,includeNAsRowvar = T)
tab1


# savePrettyExcelWorkbook(listOfTables = list(tab1=tab1),workbookName = "test2")
#
# weights=NULL
# statistical_test = T
# confint = F
# summary_stat = "mean"
# formatPvalsForEpiPaper = T
# includeNAsColvar = T
# includeNAsRowvar = T
# rowvar="abcat"
# rowwise_precentages=T
# comma_thousands=F
# include_percentages=T
# addNobsTopRow=T
# cov_names=NULL

# Run models --------------------------------------------------------------
options(modelmaker.name_map=c(x1="X1 var",x2="X2 var",x3="X3 var",x4="X4 var"))

myvars=c("x1","x2","x3","x4")
mymods=ModelMakerMulti(dat = dat,list_of_variables_of_interest = myvars,outcome = "y",sf = 2,
                       simpleround = T,ncores = 1,auto_pretty = T,
                       joint_adjustment_vars = myvars)
mymods$plot_output
mymods$df_output
vif=mymods$vif_output




vif |>
  filter(adjustment==max(adjustment)) |>
  ggplot(aes(y=term, x=vif))+
  geom_bar(stat = "identity",width=0.01)+
  # geom_col()+
  geom_point()+
  ggforce::facet_col(vars(Variable),
                     scales = "fixed",
                     space  = "free",
                     shrink = TRUE,
                     drop   = TRUE)+
  coord_cartesian (xlim = c(1, NA), clip = "on") +
  OverReact::theme_react() +
  theme(legend.position = legend.position,
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(size = rel(0.1),linetype = "dashed")
  ) +
  labs(x="Variance Inflation Factor (VIF)", y="")





mod=glm(as.formula("y~x1"),family = "gaussian",data = dat)

summary(mod)

makeORTable(mod)
future::plan("sequential")

