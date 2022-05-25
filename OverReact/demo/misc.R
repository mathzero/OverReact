"http://ahsc-r-server.sm.med.ic.ac.uk"

install.packages("devtools", repos = "http://ahsc-r-server.sm.med.ic.ac.uk")

### MATT! Use this to check and sort out the overreact package
devtools::check(cran = F)


install.packages("E:/Group/react2_study5/report_phases_combined/projects/react_package/OverReact",
                 repos=NULL, type="source")


  getwd()

Sys.getenv("R_PROFILE")
?Startup

local({r <- getOption("repos")
r["cran"] <- "http://ahsc-r-server.sm.med.ic.ac.uk"
options(repos=r)})


file.edit(file.path("~", ".Rprofile"))


remove.packages("rlang")
install.packages("rlang")


rhub::check(platform="windows-x86_64-devel",
            env_vars = c(R_COMPILE_AND_INSTALL_PACKAGES="always"))



result_var = "res"
strat_var = "vacc_status_binary"




# Test significant figures ------------------------------------------------

testvect <- x <- c(0.12,0.1234,1.123,12.221,123.32,423489, 1.000,12.000)
sf <- k<- 3

# quick function to make sure 2dp always shown
specifyDecimal <- function(x,k){
  formatC(signif(x = x,digits = k),digits = k,format = "fg",flag = "#")
}

specifyDecimal(testvect,3)
prettyNum(testvect,zero.print = T,drop0trailing = F,)
formatC(testvect,digits = 3,format = "fg",drop0trailing = T,replace.zero = F,zero.print = T,flag = "#")


