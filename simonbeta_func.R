#
# simonbeta_func.R
#
# created on 09/02/23
# Stephanie Homan, stephanie dot homan at bli dot uzh dot ch
#-----------------------------------------------------------------------
#
# common libraries
if (!require("pacman")) install.packages("pacman")
library("pacman")
pacman::p_load(
  "rJava",
  "devtools",
  "grid",
  "papaja",
  "bookdown",
  "rmarkdown",
  "knitr",
  "tidyverse",
  "readxl",     
  "cowplot",
  "stats",
  "backports",     # to revive the isFALSE() function for sim_slopes()
  "effects",       # for probing interactions
  "interactions",  # for probing/plotting interactions
  "plyr",          # for data manipulation
  "car", 
  "arsenal",
  "magrittr",
  "lme4",         # for multilevel models
  "nlme",         # for multilevel models
  "lattice",
  "lmerTest",     # for p-values
  "lubridate",
  "gridExtra",
  "ggpubr",
  "cowplot",
  "here", 
  "writexl",
  "reshape",
  "varian",
  "descr",
  "lm.beta",      # extract betas from lm
  "sjPlot",       # lm dotplots
  "broom",        # collect p-values from lm
  "data.table",   # change names in data.frame
  "psych",        # descriptives
  "corrr",        # correlation analysis
  "ggcorrplot",   # visualize correlation matrix
  "FactoMineR",    # perform PCA
  "factoextra",
  "gtsummary",
  "gt"
)


# Trim character vector for multiple columns
#-----------------------------------------------------------------------

# trim character vector after the first 2 characters
trim_string <- function(item) {
  return(as.numeric(strtrim(item, 2)))
}

# apply trim_string function to multiple scales in df
## scl = scale, len = length of scale, n = number of subj, dat = df
scale_str2num <- function(scl, len, n, dat) {
  vals <- matrix(NA, len, n)
  for (i in 1:len) {
    # call the specified scale
    str <- dat[paste0(scl, i)]
    # apply trim function
    vals[i,] <- trim_string(unlist(str))
    # save trimmed vector as new column
    dat[paste0(tolower(scl), i, "_n")] <- vals[i,]
    #dat[paste0(scl, i)] <- vals[i,]
  }
  return(dat)
}


# Frequency bar plot
#-----------------------------------------------------------------------

# define color palette
palette_risk <- c("lightpink", "seagreen2", "lightgoldenrod1")

gg_freqbar <- function(dat, 
                       outcome = outcome, 
                       colorcode = colorcode,
                       base_size = 17, 
                       bar_width = .4,
                       text_size = 5,
                       text_pos = .4,
                       text_distance = -.5, 
                       colorscheme = "Blues",
                       legend_pos = "bottom"
                       ){
  
  # mutate var as factors
  dat <- dat %>% 
    mutate(outcome = factor(outcome),
           colorcode = factor(colorcode))
  
  # plot data
  dat %>% 
    ggplot(aes(x = outcome, fill = colorcode)) +
    geom_bar(position = position_dodge(preserve = 'single'), 
             width = bar_width) +
    theme_minimal(base_size = base_size) +
    theme(legend.position = legend_pos, 
          legend.title = element_blank()) +
    scale_fill_brewer(palette = colorscheme) +
    scale_x_discrete(limits = rev(levels(outcome))) +
    geom_text(aes(x = outcome, label = ..count..),
              stat = "count",
              position = position_dodge(width = text_pos),
              hjust = text_distance, size = text_size) +
    xlab("") + ylab("Number of comparisons") +
    coord_flip()
}




# Blubble plot no filter
#-----------------------------------------------------------------------
gg_bubble <- function(df,
                      x = "x",
                      y = "y",
                      color = "darkblue",
                      size = "study_N",
                      base_size = 16,
                      xlab = "Mean pre-post difference score",
                      ylab = "SD of pre-post difference score",
                      title = "Mean-variance relationship",
                      lable_position = 0
){
  p1 <- df %>% 
    ggscatter(x = x, y = y,
              color = color, size = size, alpha = 0.3, 
              add = "reg.line", 
              add.params = list(color = color, fill = color, alpha = .6),         
              conf.int = TRUE, 
              cor.coef = TRUE, 
              cor.coeff.args = list(method = "pearson", 
                                    label.x = lable_position, 
                                    label.sep = "\n")) +
    #facet_grid(disorder_cat ~ group) +
    scale_size(range = c(3, 12)) +
    theme_gray(base_size = base_size) +
    theme(legend.position = "", 
          text = element_text(face = "bold")) +
    labs(x = xlab, y = ylab, title = title)
}



# Write test statistic in plot
#-----------------------------------------------------------------------
annotate_regression <- function(lmfit, ggp, font_size = 10) {
  #
  # annotate a regression plot
  slmfit <- coef(summary(lmfit))
  nr     <- nrow(slmfit)
  nc     <- ncol(slmfit)
  r      <- slmfit[nr, 1]
  p      <- slmfit[nr, nc]
  a      <- represearch::starsfromp(p)
  ggp2   <- ggp +
    annotate("text", x = Inf, y = Inf, size = font_size,
             hjust = 1, vjust = 1,
             label = paste0("r=", round(r, 2), starsfromp(p)))
  return(ggp2)
} 


# Report p-values with *
#-----------------------------------------------------------------------
starsfromp <- function(pval) {
  # Parses pvalues, returns asterisks
  
  as <- vector(mode = "character", length = length(pval))
  for (i in 1:length(pval)) {
    p <- pval[i]
    if (p < 0.1) as[i] <- "~"
    if (p < 0.05) as[i] <- "*"
    if (p < 0.01) as[i] <- "**"
    if (p < 0.001) as[i] <- "***"
  }
  return(as)
}

# Paste random-effects model (metafor) results
#-----------------------------------------------------------------------
parse_metafor   <- function(rma, model, statistic = "VR = "){
  #
  # print back-transformed VR (or SMD), CI upper & CI lower
  
  print(paste(statistic, round(rma[1,"es"], 2),
              ", 95% CI: ", round(rma[1, "ci.lb"], 2), 
              ", ", round(rma[1, "ci.ub"], 2),
              ", P = ", round(coef(summary(model))$pval, 3), 
              sep = ""))
}

