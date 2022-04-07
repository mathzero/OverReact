### Demo ###

library(extrafont)
library(dplyr)
library(mgcv)
library(stats)
library(OverReact)
library(progress)
library(ggplot2)
library(hrbrthemes)
fonttab <- fonttable()




# make dummy data
n=10000
df <- data.frame(y=sample(c(0,1),size = n,replace = T),
                 x1=runif(n = n,min = 0,max = 1),
                 x2=rnorm(n = n,mean = 0,sd = 1),
                 x3=rnorm(n = n,mean = 0,sd = 1),
                 x4=rnorm(n = n,mean = 0,sd = 1),
                 x5=rnorm(n = n,mean = 0,sd = 1),
                 x6=rnorm(n = n,mean = 0,sd = 1),
                 z=as.factor(rep("A",n)),
                 w=as.factor(sample(c("A","B"),size = n,replace = T)),
                 v=as.factor(sample(c("A","B","C"),size = n,replace = T))
                 )
df$x1=df$x1+abs(df$x1*df$y)
df$w[df$y==1 & sample(c(T,F),size = n,replace = T,prob = c(0.05,0.95))] <- "A"


## Run multiple model makes
predictor_vars=c(paste0("x",1:6),"z","w")
cov_name_list=as.list(predictor_vars)
names(cov_name_list) <- predictor_vars

# run univariate models
univ_res <- OverReact::ModelMakerMulti(dat = df,
                           list_of_variables_of_interest = predictor_vars,
                           outcome = "y",
                           joint_adjustment_vars = predictor_vars,sf = 2,format = "f",
                           cov_name_list = (cov_name_list))

# Get outputs for tables and plotting
univ_df <- univ_res$df_output
univ_df_plot<- univ_res$plot_output

# create plot
univ_df_plot %>% ggplot(aes(OR,Lower, col = model))+
  geom_point() +
  OverReact::theme_react(base_family = "IBM Plex Sans",plot_title_margin = 1) +
  labs(title ="My test title",
       subtitle = "Some words that might constitute a subtitle")
# +
#   theme(text = element_text(family = "IBM Plex Sans"),
#         plot.title = element_text(family = "IBM Plex Sans SemiBold"),
#         plot.subtitle = element_text(family = "IBM Plex Sans"))


# Cross tabs --------------------------------------------------------------

tab=crossTab(dat = df,rowvar = "v",colvar = "w",separator = "-")
tab_cont=crossTabContinuous(dat = df,rowvar = "x1",colvar = "w")

tabmulti=crossTabMulti(dat = df,rowvar_list = c("x1", "v"),colvar = "w",cov_names=NULL)

tabmulti_w=crossTabMulti(dat = df,rowvar_list = c("x1", "v"),colvar = "w",cov_names=NULL,weights = "x1")







