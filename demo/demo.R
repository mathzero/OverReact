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
                 x1=rnorm(n = n,mean = 0,sd = 1),
                 x2=rnorm(n = n,mean = 0,sd = 1),
                 x3=rnorm(n = n,mean = 0,sd = 1),
                 x4=rnorm(n = n,mean = 0,sd = 1),
                 x5=rnorm(n = n,mean = 0,sd = 1),
                 x6=rnorm(n = n,mean = 0,sd = 1),
                 z=as.factor(rep("A",n)),
                 w=as.factor(sample(c("A","B"),size = n,replace = T))
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



joint_adjustment_vars = predictor_vars
sf = 2
list_of_variables_of_interest = predictor_vars
format = "f"
outcome="y"
variable_name="z"
data=df

fonttab <- fonttable()

getwd()

library(extrafont)
extrafont::font_import(paths = "C:/Windows/Fonts/",
                       recursive = T,prompt = F)
suppressWarnings(extrafont::loadfonts(device = "win",quiet = T))
# extrafont::font_install(fontpkg = hrbrthemes::font_ps,prompt = F)

loadfonts(quiet = T,device ="win")
extrafont::font_import(prompt = F)

load_packages <- function(package.list){
  new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
  print(paste(length(new.packages), "packages require installation. Installing now"))
  if(length(new.packages)) install.packages(new.packages, repos="http://se-r.sm.med.ic.ac.uk", dependencies = TRUE)
  print("Loading packages")
  lapply(package.list, require, character.only=TRUE)
}
windowsFonts("IBMPlexSans-Medium" = windowsFont("IBMPlexSans-Medium"),
             "IBMPlexSans-Bold" = windowsFont("IBMPlexSans-Bold"),
             "IBMPlexSans" = windowsFont("IBMPlexSans"))

windowsFonts()$IBMPlexSans

list.files("//se-transfer-i/react_s/mw418")
devtools::install_local("//se-transfer-i/react_s/mw418/Rttf2pt1_1.3.8.tar.gz")




