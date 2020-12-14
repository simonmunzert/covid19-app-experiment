# install packages from CRAN
p_needed <- c("texreg", # load first bc conflict with magrittr/tidyr
              "tidyverse", "rlang", "haven", "lubridate", # tidverse tools
              "magrittr", "janitor", "broom", # clean up stuff
              "readxl", "writexl", "readstata13", #  import/export
              "stargazer", "xtable", "summarytools", # format stuff
              "ggthemes", "hrbrthemes",  "RColorBrewer", "networkD3", "grid", "gridExtra", "viridis", "scales", "sjPlot", "cowplot", "patchwork", "colormap",  # plot stuff
              "survey", "arm", "misreport", "mvtnorm", "list", "MBESS", "pwr", "reldist", "estimatr", "glmnet", "ivdesc", "lemon", # stats stuff
              "extrafont","descr"
)

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)


# standard error of proportion
se.prop <- function(x, na.rm = FALSE) {
  if(na.rm == TRUE) {
    x <- na.omit(x)
  }
  se <- sqrt((mean(x)*(1-mean(x)))/length(x))
  se
}