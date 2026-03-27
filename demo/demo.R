library(tidyverse)
library(OverReact)

# roxygen2::roxygenize()
# devtools::document()
# devtools::check()


### source all relevant scripts
scripts=paste0("R/",list.files("R/"))
lapply(scripts,source)

set.seed(123)
### create dummy data
n=10000
dat=data.frame(x1=rnorm(n =n,mean = 0,sd = 1),
               x2=rnorm(n =n,mean = 0,sd = 1),
               x3=rnorm(n =n,mean = 0,sd = 1),
               x4=rnorm(n =n,mean = 0,sd = 1)
)

### add binary y variable
dat$y=sample(c(0,1),size = n,replace = T)

### add gaussian y variable
dat$y=rnorm(n =n,mean = 0,sd = 1)

# add some signal
dat$y=dat$y+0.3*dat$x1

### add categorical factor variable y variable
dat$cat=factor(sample(c("Case","Control"),size = n,replace = T))

### add another categorical factor variable y variable
dat$abcat=factor(sample(c("A some text","B more text","3","Fourth cat"),size = n,replace = T,prob = c(0.7,0.18,0.02,0.1)), levels=c("A some text","B more text","3","Fourth cat"))

# add some random NAs to test
for(i in 1:100){
  set.seed(i)
  dat[sample(1:n,size = 1,replace = F),sample(1:7,size = 1,replace = F)] <- NA
}

# add some NAs into the abcat
dat$abcat[sample(1:n,size = 50,replace = F)] <- NA
dat$cat[sample(1:n,size = 100,replace = F)] <- NA

# add some signal to x1
dat$x1[dat$cat=="Case" & !is.na(dat$cat)] <- dat$x1[dat$cat=="Case"& !is.na(dat$cat)]+1


myvars=c("x1","x2","x3","x4","cat","abcat")
dat <- dat |> mutate(y_bin=as.numeric(y>0))


# Run automated analysis --------------------------------------------------

res_auto <- run_auto_tableone_regressions(dat = dat,outcome = "y_bin",
                                          covariates = myvars[1:3],
                                          independent_variables = myvars,
                                          report_path = "/Users/mw418/codebase/OverReact/demo/report.txt"
                                            )

res_auto$model_outputs$df_output
res_auto$model_outputs$diagnostics

# Run models --------------------------------------------------------------
library(tictoc)


tic()
mymods=ModelMakerMulti(dat = dat,
                         list_of_variables_of_interest = myvars,
                         outcome = "y_bin",
                         sf = 2,
                        #  incremental = T,
                        #  include_crude = T,
                       simpleround = T,
                       remove_intercept_from_results = T,
                       ncores = 1,
                       auto_pretty = T,
                       joint_adjustment_vars = myvars
                      #  include_rd = F,
                      #  n_sim = 20
                      )
toc()

plot_output <- mymods$plot_output
df_output <- mymods$df_output
df_output_RDs=mymods$df_output_RDs

df_output
df_output_RDs

# Forest plots -----------------------------------------------------------

forest_plot_mm <- plotReactForest(
  mymods,
  adjustment_numbers = c(0, 1, 3),
  adjustment_descriptions = c("Crude", "+ X1", "Full model"),
  title = "Forest plot from ModelMakerMulti() output"
)
print(forest_plot_mm)

# Run non-sequential models -----------------------------------------------


tic()
mymods_non_inc=ModelMakerMultiRD(dat = dat,
                         list_of_variables_of_interest = myvars,
                         outcome = "y_bin",
                         sf = 2,
                         incremental = F,
                         include_crude = F,
                         simpleround = T,
                         remove_intercept_from_results = T,
                         ncores = 1,
                         auto_pretty = T,
                         joint_adjustment_vars = myvars[1:3],
                         include_rd = T,
                         n_sim = 20)
toc()

plot_output_noninc <- mymods_non_inc$plot_output
df_output_noninc <- mymods_non_inc$df_output
df_output_RDs_noninc=mymods_non_inc$df_output_RDs





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





# Test individual models --------------------------------------------------



testmod <- glm(formula = as.formula(y_bin  ~ x1+x2+x3+x4+cat),family = "binomial",data = dat)
makeORTable(mod = testmod,ref_level = "Case",dp = 3)

forest_plot_glm <- plotReactForest(
  testmod,
  variables = c("x1", "cat"),
  title = "Forest plot from a raw logistic model"
)
print(forest_plot_glm)

testmod_lm <- lm(formula = y ~ x1 + x2 + cat, data = dat)
forest_plot_lm <- plotReactForest(
  testmod_lm,
  variables = c("x1", "cat"),
  title = "Forest plot from a raw linear model"
)
print(forest_plot_lm)

library(tictoc)

tic()
makeRDTable(mod = testmod,variable_name = "x3",dp = 10,data = dat,n_sim = 100)
toc()


tic()
makeRDTable(mod = testmod,variable_name = "cat",ref_level = "Case",dp = 10,data = dat,n_sim = 100)
toc()

