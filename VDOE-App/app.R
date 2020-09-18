library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(shinythemes)
library(tableHTML)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(RColorBrewer)
library(readxl)
library(plotly)
library(GGally)
library(ggrepel)
library(DT)

source("IA_parcoords.R")

# load VA data ---------------------------------------------------------------
virginia <- st_read("data/VA/va_FAFSA_CO/va_FAFSA_CO.shp")
superintendent <- st_read("data/VA/super_simple/superintendent_simple.shp")
vccs <- st_read("data/VA/vccs/vccs.shp")
hs <- st_read("data/VA/hs_new/hs_new.shp")
data_download <- read.csv("data/VA/data_download_2_reldif_updated.csv")
data_download$DIS12 <- formatC(round(as.numeric(as.character(data_download$DIS12 )), 0))
df <- readRDS("data/VA/hi_ed_acs.rds")  
hi_ed <- df[c(1,2,9,14:54)]

colleges<- st_read("data/VA/colleges/colleges.shp")

demo_df <- read_excel("data/VA/Demo.xlsx", col_names = TRUE)

pell <- readRDS("data/VA/pell_acs.rds")

division <- st_read("data/VA/division/division.shp")
division$bin  <- factor(division$bin , levels = c("[-80, -60)", "[-60, -40)", "[-40, -20)", "[-20, 0)", "0", "(0, 20]", "(20, 40]", "(40, 60]"))

#
# load IA data -----------------------------------------------------------
#

IA_grad_plans <- readRDS("data/IA/IA_grad_plans.rds") #%>% st_as_sf()
IA_colleges <- readRDS("data/IA/colleges.rds") 
IA_hs_fafsa <- readRDS("data/IA/hs_fafsa.rds")
IA_fafsa_by_county <- readRDS("data/IA/fafsa_by_county.rds") #%>% st_as_sf() 
IA_aea <- st_read("data/IA/IowaAEAs.shp")

IA_aea <- st_transform(IA_aea, 4326)


#
# VA plot labels --------------------------------------------------------------------------------------------------
#

VA_co_labels <- lapply(paste("<strong>County: </strong>",
                             virginia$`County`,
                             "<br />",
                             "<strong> # Seniors: </strong>",
                             round(virginia$POP,0),
                             "<br />",
                             "<strong> % Seniors that completed FAFSA: </strong>",
                             round(virginia$CO_Per,0)
), htmltools::HTML)


label_division <- lapply(
  paste("<strong>Division: </strong>",
        division$new_name,
        "<br />",
        "<strong>RPD 2019-20: </strong>",
        division$RPD,
        "<br />",
        "<strong># of Completed FAFSAs in 2020: </strong>",
        division$COM2020, 
        "<br />",
        "<strong># of Completed FAFSAs in 2019: </strong>",
        division$COM2019),
  
  htmltools::HTML
)


labels <- lapply(paste("<strong>High School Name: </strong>",
                       hs$SCHOOL,
                       "<br />",
                       "<strong>Superintendent Region</strong>",
                       hs$Region,
                       "<br />",
                       "<strong> # 2019-20 Seniors: </strong>",
                       hs$POP12_2019,
                       "<br />",
                       "<strong> # Disadvantaged Seniors: </strong>",
                       round(hs$DIS12, 0),
                       "<br />",
                       "<strong> # Senior English Learners: </strong>",
                       hs$EL12,
                       "<br />",
                       "<strong> # Senior Minority Students: </strong>",
                       hs$MnrtS12
                       #"<br />",
                       #"<strong> # Black, Not of Hispanic Origin: </strong>",
                       #hs$AA12
), htmltools::HTML)


label2 <- lapply(paste("<strong>Community College Name: </strong>",
                       vccs$Instt_N,
                       "<br />",
                       "<strong>Website: </strong>",
                       paste("<a href = ","https://", paste(vccs$Website), " >", sep = ""),
                       vccs$Website, paste("</a>")), htmltools::HTML)

#
# IA plot labels --------------------------------------------------------------
#

IA_hs_labels <- lapply(paste("<strong>High School Name: </strong>",
                       IA_hs_fafsa$`School Name.x`,
                       "<br />",
                       "<strong>AEA Region: </strong>",
                       IA_hs_fafsa$`AEA Name`,
                       "<br />",
                       "<strong> # Seniors: </strong>",
                       IA_hs_fafsa$seniors,
                       "<br />",
                       "<strong> # Seniors Receiving Free or Reduced Lunch: </strong>",
                       IA_hs_fafsa$free_reduced_lunch,
                       "<br />",
                       "<strong> # Senior English Learners: </strong>",
                       IA_hs_fafsa$english_learners,
                       "<br />",
                       "<strong> # Senior Minority Students: </strong>",
                       IA_hs_fafsa$minority 
                       #"<strong> # Black, Not of Hispanic Origin: </strong>",
                       #IA_hs_fafsa$black
), htmltools::HTML)


IA_co_labels <- lapply(paste("<strong>County: </strong>",
                        IA_fafsa_by_county$`County Name`,
                        "<br />",
                        "<strong> # Seniors: </strong>",
                        round(IA_fafsa_by_county$num_seniors,0),
                        "<br />",
                        "<strong> % Seniors that completed FAFSA: </strong>",
                        round(IA_fafsa_by_county$perc_complete_20,0)
), htmltools::HTML)



IA_relfaf_labels <- lapply(
  paste("<strong>County: </strong>",
        IA_fafsa_by_county$`County Name`,
        "<br />",
        "<strong>Relative Percent Difference 2019-20: </strong>",
        round(IA_fafsa_by_county$pct_rel_diff,2),
        "<br />",
        "<strong># of Completed FAFSAs in 2020: </strong>",
        IA_fafsa_by_county$num_complete_fafsa_20, 
        "<br />",
        "<strong># of Completed FAFSAs in 2019: </strong>",
        IA_fafsa_by_county$num_complete_fafsa_19),
  
  htmltools::HTML
)








virginia[virginia$new_name == "COLONIAL HEIGHTS CITY", "CO_Per"] <- 100

#
# Vicki's parallel coordinates code -----------------------------------------
#

parcoord.val<- function (data, columns = 1:ncol(data), groupColumn = NULL, scale = "std", 
                         scaleSummary = "mean", centerObsID = 1, missing = "exclude", 
                         order = columns, showPoints = FALSE, splineFactor = FALSE, 
                         alphaLines = 1, boxplot = FALSE, shadeBox = NULL, mapping = NULL, 
                         title = "") {
  
  if (!identical(class(data), "data.frame")) {
    data <- as.data.frame(data)
  }
  
  saveData <- data
  
  if (is.null(groupColumn)) {
    if (any(tolower(order) %in% c("anyclass", "allclass"))) {
      stop("can't use the 'order' methods anyClass or allClass without specifying groupColumn")
    }
  } else if (!((length(groupColumn) == 1) && (is.numeric(groupColumn) || is.character(groupColumn)))) {
    stop("invalid value for 'groupColumn'; must be a single numeric or character index")
  }
  
  if (!(tolower(scale) %in% c("std", "robust", "uniminmax", 
                              "globalminmax", "center", "centerobs"))) {
    stop(str_c("invalid value for 'scale'; must be one of ", 
               "'std', 'robust', 'uniminmax', 'globalminmax', 'center', or 'centerObs'"))
  }
  
  if (!(centerObsID %in% 1:dim(data)[1])) {
    stop("invalid value for 'centerObsID'; must be a single numeric row index")
  }
  
  if (!(tolower(missing) %in% c("exclude", "mean", "median", "min10", "random"))) {
    stop("invalid value for 'missing'; must be one of 'exclude', 'mean', 'median', 'min10', 'random'")
  }
  
  if (!(is.numeric(order) || (is.character(order) && (order %in% 
                                                      c("skewness", "allClass", "anyClass", "Outlying", "Skewed", 
                                                        "Clumpy", "Sparse", "Striated", "Convex", "Skinny", 
                                                        "Stringy", "Monotonic"))))) {
    stop(str_c("invalid value for 'order'; must either be a vector of column indices or one of ", 
               "'skewness', 'allClass', 'anyClass', 'Outlying', 'Skewed', 'Clumpy', 'Sparse', 'Striated', ", 
               "'Convex', 'Skinny', 'Stringy', 'Monotonic'"))
  }
  
  if (!(is.logical(showPoints))) {
    stop("invalid value for 'showPoints'; must be a logical operator")
  }
  
  alphaLinesIsCharacter <- is.character(alphaLines)
  
  if (alphaLinesIsCharacter) {
    if (!(alphaLines %in% names(data))) {
      stop("'alphaLines' column is missing in data")
    }
    alphaVar <- data[[alphaLines]]
    alphaRange <- range(alphaVar)
    if (any(is.na(alphaRange))) {
      stop("missing data in 'alphaLines' column")
    }
    if (alphaRange[1] < 0 || alphaRange[2] > 1) {
      stop("invalid value for 'alphaLines' column; max range must be from 0 to 1")
    }
  } else if ((alphaLines < 0) || (alphaLines > 1)) {
    stop("invalid value for 'alphaLines'; must be a scalar value between 0 and 1")
  }
  
  if (!(is.logical(boxplot))) {
    stop("invalid value for 'boxplot'; must be a logical operator")
  }
  
  if (!is.null(shadeBox) && length(shadeBox) != 1) {
    stop("invalid value for 'shadeBox'; must be a single color")
  } else {
    valid_color <- tryCatch(is.matrix(grDevices::col2rgb(shadeBox)), 
                            error = function(e) FALSE)
    if (!valid_color) {
      stop("invalid value for 'shadeBox'; must be a valid R color")
    }
  }
  
  if (is.logical(splineFactor)) {
    if (splineFactor) {
      splineFactor <- 3
    } else {
      splineFactor <- 0
    }
  } else if (!is.numeric(splineFactor)) {
    stop("invalid value for 'splineFactor'; must be a logical or numeric value")
  }
  
  if (is.numeric(groupColumn)) {
    groupColumn <- names(data)[groupColumn]
  }
  
  if (!is.null(groupColumn)) {
    groupVar <- data[[groupColumn]]
  }
  
  if (is.character(columns)) {
    columns_ <- c()
    for (colPos in seq_along(columns)) {
      columns_[colPos] <- which(colnames(data) == columns[colPos])
    }
    columns <- columns_
  }
  
  char.vars <- GGally:::column_is_character(data)
  
  if (length(char.vars) >= 1) {
    for (char.var in char.vars) {
      data[[char.var]] <- factor(data[[char.var]])
    }
  }
  
  fact.vars <- GGally:::column_is_factor(data)
  
  #fact.vars <- setdiff(fact.vars, groupColumn)
  fact.vars <- c("Region","Division.Name", "LOCATION")
  if (length(fact.vars) >= 1) {
    for (fact.var in fact.vars) {
      data[[fact.var]] <- as.numeric(data[[fact.var]])
    }
  }
  
  saveData2 <- data
  
  if (!is.null(groupColumn)) {
    saveData2[[groupColumn]] <- as.numeric(saveData2[[groupColumn]])
  }
  
  # begin plot
  p <- c(ncol(data) + 1, ncol(data) + 2)
  
  data$.ID <- as.factor(1:nrow(data))
  
  data$anyMissing <- apply(is.na(data[, columns]), 1, any)
  
  columnsPlusTwo <- c(columns, p)
  
  inner_rescaler_default <- function(x, type = "sd", ...) {
    switch(type, rank = rank(x, ...), var = , sd = (x - mean(x, 
                                                             na.rm = TRUE))/sd(x, na.rm = TRUE), robust = (x - 
                                                                                                             median(x, na.rm = TRUE))/mad(x, na.rm = TRUE), I = x, 
           range = (x - min(x, na.rm = TRUE))/diff(range(x, 
                                                         na.rm = TRUE)))
  }
  
  inner_rescaler <- function(x, type = "sd", ...) {
    continuous <- sapply(x, is.numeric)
    if (any(continuous)) {
      if (type %in% c("sd", "robust", "range")) {
        singleVal <- sapply(x, function(col) {
          if (length(unique(col)) == 1) {
            TRUE
          }
          else {
            FALSE
          }
        })
        ind <- continuous & !singleVal
        x[ind] <- lapply(x[ind], inner_rescaler_default, 
                         type = type, ...)
        x[singleVal] <- 1
      }
      else {
        x[continuous] <- lapply(x[continuous], inner_rescaler_default, 
                                type = type, ...)
      }
    }
    x
  }
  
  
  if (tolower(scale) %in% c("std", "robust", "uniminmax", "center")) {
    rescalerType <- c(std = "sd", robust = "robust", uniminmax = "range", 
                      center = "range")[tolower(scale)]
    data[columnsPlusTwo] <- inner_rescaler(data[columnsPlusTwo], 
                                           type = rescalerType)
    if (tolower(scale) == "center") {
      data[columns] <- apply(data[columns], 2, function(x) {
        x <- x - eval(parse(text = paste(scaleSummary, 
                                         "(x, na.rm=TRUE)", sep = "")))
      })
    }
  }
  
  if (tolower(missing) == "exclude") {
    dataCompleteCases <- complete.cases(data[columnsPlusTwo])
    if (!is.null(groupColumn)) {
      groupVar <- groupVar[dataCompleteCases]
    }
    if (alphaLinesIsCharacter) {
      alphaVar <- alphaVar[dataCompleteCases]
    }
    data <- data[dataCompleteCases, ]
  } else if (tolower(missing) %in% c("mean", "median", "min10", 
                                     "random")) {
    missingFns <- list(mean = function(x) {
      mean(x, na.rm = TRUE)
    }, median = function(x) {
      median(x, na.rm = TRUE)
    }, min10 = function(x) {
      0.9 * min(x, na.rm = TRUE)
    }, random = function(x) {
      num <- sum(is.na(x))
      idx <- sample(which(!is.na(x)), num, replace = TRUE)
      x[idx]
    })
    missing_fn <- missingFns[[tolower(missing)]]
    data[columns] <- apply(data[columns], 2, function(x) {
      if (any(is.na(x))) {
        x[is.na(x)] <- missing_fn(x)
      }
      return(x)
    })
  }
  
  if (tolower(scale) == "centerobs") {
    data[columnsPlusTwo] <- inner_rescaler(data[columnsPlusTwo], 
                                           type = "range")
    data[columns] <- apply(data[columns], 2, function(x) {
      x <- x - x[centerObsID]
    })
  }
  
  meltIDVars <- colnames(data)[-columns]
  
  if (!is.null(groupColumn)) {
    meltIDVars <- union(groupColumn, meltIDVars)
  }
  if (alphaLinesIsCharacter) {
    data <- cbind(data, alphaVar)
    names(data)[dim(data)[2]] <- alphaLines
    meltIDVars <- union(meltIDVars, alphaLines)
  }
  
  data.m <- reshape2:::melt(data, id.vars = meltIDVars, measure.vars = columns)
  
  
  if (length(order) > 1 & is.numeric(order)) {
    data.m$variable <- factor(data.m$variable, levels = names(saveData)[order])
  }else if (order %in% c("Outlying", "Skewed", "Clumpy", "Sparse", 
                         "Striated", "Convex", "Skinny", "Stringy", "Monotonic")) {
    require_namespaces("scagnostics")
    scag <- scagnostics::scagnostics(saveData2)
    data.m$variable <- factor(data.m$variable, levels = scag_order(scag, 
                                                                   names(saveData2), order))
  }else if (tolower(order) == "skewness") {
    abs.skew <- abs(apply(saveData2, 2, skewness))
    data.m$variable <- factor(data.m$variable, levels = names(abs.skew)[order(abs.skew, 
                                                                              decreasing = TRUE)])
  }else if (tolower(order) == "allclass") {
    f.stats <- rep(NA, length(columns))
    names(f.stats) <- names(saveData2[columns])
    for (i in 1:length(columns)) {
      f.stats[i] <- summary(lm(saveData2[, i] ~ groupVar))$fstatistic[1]
    }
    data.m$variable <- factor(data.m$variable, levels = names(f.stats)[order(f.stats, 
                                                                             decreasing = TRUE)])
  } else if (tolower(order) == "anyclass") {
    axis.order <- singleClassOrder(groupVar, saveData2)
    data.m$variable <- factor(data.m$variable, levels = axis.order)
  }
  
  
  # mapping2
  if (!is.null(groupColumn)) {
    mapping2 <- aes_string(x = "variable", y = "value", group = ".ID",
                           colour = groupColumn, text = "SCHOOL")
  } else {
    mapping2 <- aes_string(x = "variable", y = "value", group = ".ID", text = "SCHOOL")
  }
  
  mapping2 <- GGally:::add_and_overwrite_aes(mapping2, mapping)
  
  
  p <- ggplot(data = data.m, mapping = mapping2)
  
  if (!is.null(shadeBox)) {
    d.sum <- ddply(data.m, c("variable"), summarize, min = min(value), 
                   max = max(value))
    p <- p + geom_linerange(data = d.sum, size = I(10), col = shadeBox, 
                            inherit.aes = FALSE, mapping = aes_string(x = "variable", 
                                                                      ymin = "min", ymax = "max", group = "variable"))
  }
  if (boxplot) {
    p <- p + geom_boxplot(mapping = aes_string(group = "variable"), 
                          alpha = 0.8, notch=TRUE, fill="#F0E442", colour="#F0E442")
  }
  if (!is.null(mapping2$size)) {
    lineSize <- mapping2$size
  }else {
    lineSize <- 0.5
  }
  
  # this is where the issue starts
  
  if (splineFactor > 0) {
    data.m$ggally_splineFactor <- splineFactor
    if (class(splineFactor) == "AsIs") {
      data.m <- ddply(data.m, ".ID", transform, spline = spline(variable, 
                                                                value, n = ggally_splineFactor[1]))
    }else {
      data.m <- ddply(data.m, ".ID", transform, spline = spline(variable, 
                                                                value, n = length(variable) * ggally_splineFactor[1]))
    }
    
    linexvar <- "spline.x"
    lineyvar <- "spline.y"
    
    if (alphaLinesIsCharacter) {
      p <- p + geom_line(aes_string(x = linexvar, y = lineyvar, 
                                    alpha = alphaLines), size = lineSize, data = data.m) + 
        scale_alpha(range = alphaRange)
    } else {
      p <- p + geom_line(aes_string(x = linexvar, y = lineyvar), 
                         alpha = alphaLines, size = lineSize, data = data.m)
    }
    
    if (showPoints) {
      p <- p + geom_point(aes(x = as.numeric(variable), 
                              y = value))
    }
    xAxisLabels <- levels(data.m$variable)
    p <- p + scale_x_continuous(breaks = seq_along(xAxisLabels), 
                                labels = xAxisLabels, minor_breaks = FALSE)
  } else {
    if (alphaLinesIsCharacter) {
      p <- p + geom_line(aes_string(alpha = alphaLines), 
                         size = lineSize, data = data.m) + scale_alpha(range = alphaRange)
    } else {
      p <- p + geom_line(data = subset(data.m, data.m[,1] == "No Decrease"), alpha = alphaLines, lwd=1)
      p <- p + geom_line(data = subset(data.m, data.m[,1] == "Decrease"), alpha = alphaLines, lwd=1)
    }
    
    if (showPoints) {
      p <- p + geom_point(data = subset(data.m, data.m[,1] == "No Decrease"), size=2)
      p <- p + geom_point(data = subset(data.m, data.m[,1] == "Decrease"), size=2)
      
    }
  }
  
  if (title != "") {
    p <- p + labs(title = title)
  }
  
  p
  
}


# theme_SDAD fcn ----------------------------------------------------------

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_SDAD<-function (base_size=12, base_family="sans", style=c("default","darkunica"), bgcolor=NULL)
{
  if (!is.null(bgcolor)) {
    warning("`bgcolor` is deprecated. Use `style` instead.")
    style<-bgcolor
  }
  style<-match.arg(style)
  bgcolor<-switch(style, default="#FFFFFF", darkunica="#2A2A2B")
  ret<-theme(rect=element_rect(fill=bgcolor, linetype=0, colour = NA),
             text=element_text(size=base_size, family=base_family),
             title=element_text(hjust=0.5),
             axis.title.x=element_text(hjust=0.5),
             axis.title.y=element_text(hjust=0.5),
             panel.grid.major.y=element_line(colour="#D8D8D8"),
             panel.grid.minor.y=element_blank(),
             panel.grid.major.x=element_line(colour="#D8D8D8"),
             panel.grid.minor.x=element_blank(),
             panel.border=element_blank(),
             panel.background=element_blank(),
             legend.position="bottom",
             legend.key=element_rect(fill="#FFFFFF"))
  if (style=="darkunica") {
    ret<-(ret + theme(rect=element_rect(fill=bgcolor),
                      text=element_text(colour="#A0A0A3"),
                      title=element_text(colour="#FFFFFF"),
                      axis.title.x=element_text(colour="#A0A0A3"),
                      axis.title.y=element_text(colour="#A0A0A3"),
                      panel.grid.major.y=element_line(colour="#707073"),
                      legend.title=element_text(colour="#A0A0A3")))
  }
  ret
}

#
#---------------------- PLOTLY - VA FAFSA Plots ------------------------------------#
#

VAHS <- read_excel("data/VA/VA_HS.xlsx", 
                   col_types = c("text", "numeric", "numeric", 
                                 "text", "text", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))

for(i in 1:8){
  
  data <-VAHS[VAHS$Region==i,]
  DECR <-ifelse(data$RELDIF<=-25, "Decrease", "No Decrease")
  DECR <-ordered(DECR, levels=c("No Decrease", "Decrease"))
  data <-data.frame(cbind(data, DECR))
  
  
  p <- parcoord.val(data,
                    columns=c(26,24,22,20,18,16), scale="globalminmax", 
                    groupColumn=28, showPoints=TRUE, shadeBox=NULL,
                    title=paste("Region", i, "Parallel Coordinate Plot for Completed FAFSA Submissions over Time by High School"),
                    alphaLines=1, boxplot=TRUE) +
    scale_color_manual(values=cbPalette[c(7,1)]) +
    scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
    theme_SDAD() + 
    xlab("") + ylab("Number of Completed FAFSA Applications") +
    theme(
      plot.title = element_text(size=14, hjust=0.5),
      axis.text.x=element_text(size=12), 
      axis.text.y=element_text(size=12), 
      legend.position="none")
  
  ly <- ggplotly(p, tooltip = "text")
  
  ly$x$data[[1]]$hoverinfo <- "none"
  ly$x$data[[1]]$notched <- TRUE
  
  
  ly$x$layout$title$font[["family"]] <- "arial"
  ly$x$layout$xaxis$tickfont[["family"]] <- "arial"
  ly$x$layout$yaxis$tickfont[["family"]] <- "arial"
  ly$x$layout$yaxis$title$font[["family"]] <- "arial"
  
  assign( paste("ly", i, sep = ""), ly)
  
  assign( paste("VAHS", i, sep = ""), data)
  
}


# VA fafsa tables--------------------------------------------------------------------------------------------

for(i in 1:8){
  data <- data_download %>% 
    select(SCHOOL, RELDIF, PERDISADV12.2019, POP12.2019, Region)%>% 
    filter(RELDIF < -25 & Region ==i ) %>% 
    arrange(-RELDIF) %>%
    mutate(RELDIF = formatC(round(RELDIF, 2), format = 'f', digits = 2),
           PERDISADV12.2019 = formatC(round(as.numeric(as.character(PERDISADV12.2019)), 2), format = 'f', digits = 2))%>%
    select(-Region) %>%
    rename("High School" = "SCHOOL", "Relative Percent Difference in Completed FAFSA Applications" = "RELDIF", "Percent Disadvantaged Seniors"= "PERDISADV12.2019", "Number of Seniors" = "POP12.2019") 
  
  assign( paste("region", i, sep = ""), data)
}

#
#------------------PLOTLY - IA FAFSA plots ------------------------------------
#

i=1

for(name in c("Central Rivers", "Grant Wood", "Great Prairie", "Green Hills", 
              "Heartland", "Keystone", "Mississippi Bend", "Northwest", 
              "Prairie Lakes"))
{
  
  data <- IA_hs_fafsa[IA_hs_fafsa$`AEA Name` == name,]
  DECR <- ifelse(data$rel_diff_20_19<=-25, "Decrease", "No Decrease")
  DECR <- ordered(DECR, levels=c("No Decrease", "Decrease"))
  data <- data.frame(cbind(data, DECR))
  
  
  p <- IA_parcoord.val(data,
                    columns=c(37, 35, 33, 31, 29, 27), #
                    scale="globalminmax",
                    groupColumn=39, showPoints=TRUE, shadeBox=NULL,
                    title=paste(name, "Parallel Coordinate Plot for Completed FAFSA Submissions over Time by High School"),
                    alphaLines=1, boxplot=TRUE) +
    scale_color_manual(values=cbPalette[c(7,1)]) +
    scale_x_discrete(labels=c("2015-16","2016-17","2017-18","2018-19","2019-20","2020-21")) +
    theme_SDAD() +
    xlab("") + ylab("Number of Completed FAFSA Applications") +
    theme(
      plot.title = element_text(size=14, hjust=0.5),
      axis.text.x=element_text(size=12),
      axis.text.y=element_text(size=12),
      legend.position="none")
  
  ly <- ggplotly(p, tooltip = "text")
  
  ly$x$data[[1]]$hoverinfo <- "none"
  ly$x$data[[1]]$notched <- TRUE
  
  
  ly$x$layout$title$font[["family"]] <- "arial"
  ly$x$layout$xaxis$tickfont[["family"]] <- "arial"
  ly$x$layout$yaxis$tickfont[["family"]] <- "arial"
  ly$x$layout$yaxis$title$font[["family"]] <- "arial"
  
  assign( paste("IA_ly", i, sep = ""), ly)
  
  i = i + 1
  
  #assign( paste("VAHS", i, sep = ""), data)
  
}


# IA fafsa tables--------------------------------------------------------------------------------------------

for(name in c("Central Rivers", "Grant Wood", "Great Prairie", "Green Hills", "Heartland", "Keystone",
              "Mississippi Bend", "Northwest", "Prairie Lakes"))
{
  data <- IA_hs_fafsa %>%
    select(`School Name.x`, rel_diff_20_19, free_reduced_lunch, seniors, `AEA Name`) %>%
    filter(`AEA Name` == name & rel_diff_20_19 < -25) %>%
    arrange(-rel_diff_20_19) %>%
    mutate(rel_diff_20_19 = formatC(round(rel_diff_20_19, 2), format = 'f', digits = 2)) %>%
    select(-`AEA Name`) %>%
    rename("High School" = "School Name.x",
           "Relative Percent Difference in Completed FAFSA Applications" = "rel_diff_20_19",
           "Number of Seniors with Free or Reduced Lunch"= "free_reduced_lunch",
           "Number of Seniors" = "seniors")

  assign(name, data) # saves each table
}


# color palettes --------------------------------------------

IA_fpal <- colorQuantile("Blues", domain = IA_fafsa_by_county$perc_complete_20, probs = seq(0, 1, length = 6), 
                         na.color = 'gray', right = FALSE)

VA_fpal <- colorQuantile(palette ="Blues", domain = virginia$CO_Per, probs = seq(0, 1, length = 6), 
                         na.color = 'gray', right = FALSE)

#
# ui -----------------------------------------------------------------------------------------
#

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
  tags$style(
    ".leaflet .legend {width:200px; text-align: left;}",
    ".leaflet .legend i{float: left;}",
    ".leaflet .legend label{float:left; text-align: left;}"
  ),
  tags$head(tags$style(HTML(" .sidebar { font-size: 40%; } "))),
  
  headerPanel(
    #tags$a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", 
    img(src = "Marks_wave_bolder-15.jpg", 
        class = "topimage", width = "50%", style = "display: block; margin-left: auto; margin-right: auto;" #)
    )),
  hr(),
  
  fluidRow(width = 12,style = "margin = 20px",  column(12, align = "center", h2(strong("COVID-19 Impact on High School Seniors and College Students")))),
  
  hr(),
  
  
  fluidRow(width = 12,
           column(1),
           column(10, 
                  p(),
                  p('The 2019-2020 high school seniors and college students have ended their final year being confined to their homes, 
                    taking classes and tests online, and 
                    are now facing disruptions in their post-secondary education… will college start in the fall and if it does will their 
                    parents be able to pay for it? Here we describe high schools in Iowa and Virginia to identify potentially vulnerable high
                    school seniors; for example, the percentage of 
                    students who are economically disadvantaged, English learners, African Americans, and Hispanics. Federal administrative 
                    data on the number of completed Free Applications for Federal Student Aid (FAFSA) is used to identify high schools 
                    that have seen a decrease in completed applications that may be attributed to COVID-19 pandemic.  
                    We further identifity a potential', em('brain gain'), 'across the state, the home counties of young adults that attend colleges across the state.')),
           column(1)),
  hr(),
  
  tabsetPanel(
    
    # High school tab -------------------------------------------
    
    tabPanel(h4("High School Map"), 
             
             br(),
             fluidRow(width = 12,
                      column(1),
                      column(width=4, align="left",
                             selectInput("hs_state", h4(strong("Select a State")),
                                         choices = list("Iowa",
                                                        "Virginia"
                                         ),
                                         selected = "Iowa")
                      )
             ),
             
             conditionalPanel("input.hs_state == 'Iowa'",
             
                              fluidRow(width = 12,
                                       column(1),
                                       column(10, align = 'center',
                                              h3(strong('Percentage of 2019-2020 High School Seniors that have Completed FAFSA Forms by Iowa Counties and Cities'))),
                                       column(1)),
                              
                              fluidRow(width = 12,
                                       column(1),
                                       column(3, p("Iowa Area Education Agencies (AEA) Regions", align = "center"),
                                              tags$img(src = "IA/aea-map.jpg", width = "100%"),
                                              p("Graphic Source: http://www.iowaaea.org/find-my-aea/")
                                       ),
                                       column(4, align = "center", br(), br(), 
                                              tags$img(src = "IA/aea_barplot.png", width = "100%")),
                                       column(3, br(), br(), 
                                              p(tags$span(' Orange ', style = "background-color: orange; color: white;border-radius: 25px; white-space: pre-wrap;"),
                                                'circles locate public high schools; click on the circle to view data on the 2019-2020 seniors.'),
                                              p('Thick borders indicate AEA Regions.'),
                                              checkboxInput(inputId = "IAshowPoints", label = "Show High Schools", value = TRUE),
                                              p(em('Please be patient as the map loads below.'))#,
                                              
                                       ),
                                       column(1)
                              ),
                              
                              br(),
                              
                              fluidRow(column(1),
                                       column(10, align = "center", leafletOutput("IA_hs_tab_map", width = "100%", height = "600px")),
                                       column(1)
                              )
                              
             ),
             
             conditionalPanel("input.hs_state == 'Virginia'",
             
                              fluidRow(width = 12,
                                       column(1),
                                       column(10, align = 'center', 
                                              h3(strong('Percentage of 2019-2020 High School Seniors that have Completed FAFSA Forms by Virginia Counties and Cities'))),
                                       column(1)),
                              
                              fluidRow(width = 12,
                                       column(1),
                                       column(6, p("Virginia Superintendent Regions", align = "center"),  
                                              tags$img(src = "regions.png", width = "100%")),
                                       column(4, p(tags$span(' Orange ', style = "background-color: orange; color: white;border-radius: 25px; white-space: pre-wrap;"), 
                                                   'circles locate public high schools; click on the circle to view data on the 2019-2020 seniors.'), 
                                              p('Thick borders indicate Superintendent Districts.'),
                                              p("Use the checkbox below to show or hide the high schools."), 
                                              checkboxInput(inputId = "showPoints", label = "Show High Schools", value = TRUE),
                                              p(em('Please be patient as the map loads below.'))),
                                       column(1)
                              ),
                              
                              br(),
                              
                              fluidRow(column(1),
                                       column(10, align = "center", leafletOutput("mymap1", width = "100%", height = "600px")),
                                       column(1)
                              )
             )
             
    ),
    
    # College tab -------------------------------------------------------------
    
    tabPanel(h4("College Map"), 
             
             br(),
             fluidRow(width = 12,
                      column(1),
                      column(width=4, align="left",
                             selectInput("col_state", h4(strong("Select a State")),
                                         choices = list("Iowa",
                                                        "Virginia"
                                         ),
                                         selected = "Iowa")
                      )
             ),
             
             
             conditionalPanel("input.col_state == 'Iowa'",
                              fluidRow(width = 12, 
                                       column(12, align = "center", h3(strong("Higher-Education Graduate Plans by Locality")))
                              ),
                              
                              
                              fluidRow(width = 12,
                                       column(1),
                                       column(10, 
                                              
                                              p('Use the selectors to choose academic year and higher-education type. Hover over a county to show the 
                                                county name, and number of students planning to attend for the 
                                                year and higher-education type selected.'),
                                              p(tags$span(' Orange ', style = "background-color: #E69F00; color: white;border-radius: 25px; white-space: pre-wrap;"),  'circles locate public, four-year colleges.'),
                                              p(tags$span(' Green ', style = "background-color: #009E73; color: white;border-radius: 25px; white-space: pre-wrap;"), 'circles locate private, four-year colleges.'), 
                                              p(tags$span(' Pink ', style = "background-color: #CC79A7; color: white;border-radius: 25px; white-space: pre-wrap;"), 'circles locate community colleges.'), p("Click on circle to view college name and website.")
                                       ),
                                       column(1)
                              ),
                              
                              br(),
                              
                              
                              
                              fluidRow(width = 12, 
                                       column(width =1),
                                       column(width = 3, align = "left",
                                              selectInput("IAwhichyear", "Academic Year",
                                                          choices = list(
                                                            "2018-2019",
                                                            "2017-2018",
                                                            "2016-2017",
                                                            "2015-2016"
                                                          ),
                                                          selected = "2018-2019")
                                       ),
                                       
                                       column(width=4, align="left",
                                              selectInput("IAwhichtype", "Higher-Education Type",
                                                          choices = list("All Institutions",
                                                                         "Private, Non-Profit, Four-Year Institutions",
                                                                         "Public Four-Year Institutions",
                                                                         "Community Colleges"
                                                          ),
                                                          selected = "All Institutions")
                                       ),
                                       
                                       column(width=4, align="center",
                                              h4(strong("Iowa Summary")),
                                              textOutput("IAyear"),
                                              tags$head(tags$style("#year{font-size: 20px}"))
                                              
                                       )
                                       
                              ),
                              
                              
                              fluidRow(
                                column(1),
                                column(width = 7, 
                                       leafletOutput("IAcolleges_map", width = "100%", height = "500px")
                                ),
                                
                                column(width = 4, align = "center",
                                       plotOutput("IAenroll_plot", height = "250px", width = "400px"),
                                       tableOutput("IAenroll_table")
                                       #plotOutput("FT_PT", height = "150px", width = "360px")
                                )
                                
                              ),
                              
                              
                              fluidRow(align="center",
                                       print("Source: Iowa Department of Education, Graduate Intentions Dataset")
                              )
                              
             ),
             
             conditionalPanel("input.col_state == 'Virginia'",
                              
                              fluidRow(width = 12, 
                                       column(12, align = "center", h3(strong("In-State Higher-Education Undergraduate Enrollment by Locality")))
                              ),
                              
                              
                              fluidRow(width = 12,
                                       column(1),
                                       column(10, 
                                              
                                              p('Use the selectors to choose academic year and higher-education type. Hover over a county to show the 
                               county name, and number of students enrolled and percentage of fall students receiving PELL for the 
                               year and higher-education type selected.'),
                                              p('PELL data for 2019-2020 is not available yet.  All of the 2019-2020 PELL data is listed as NA.'),
                                              p(tags$span(' Orange ', style = "background-color: #E69F00; color: white;border-radius: 25px; white-space: pre-wrap;"),  'circles locate public, four-year colleges.'),
                                              p(tags$span(' Green ', style = "background-color: #009E73; color: white;border-radius: 25px; white-space: pre-wrap;"), 'circles locate private, four-year colleges.'), 
                                              p(tags$span(' Pink ', style = "background-color: #CC79A7; color: white;border-radius: 25px; white-space: pre-wrap;"), 'circles locate community colleges.'), p("Click on circle to view college name and website.")
                                       ),
                                       column(1)
                              ),
                              
                              br(),
                              
                              
                              
                              fluidRow(width = 12, 
                                       column(width =1),
                                       column(width = 3, align = "left",
                                              selectInput("whichyear", "Academic Year",
                                                          choices = list(
                                                            "2019-2020",
                                                            "2018-2019",
                                                            "2017-2018",
                                                            "2016-2017",
                                                            "2015-2016"
                                                          ),
                                                          selected = "2019-2020")
                                       ),
                                       
                                       column(width=4, align="left",
                                              selectInput("whichtype", "Higher-Education Type",
                                                          choices = list("Grand Total, All Reporting Institutions",
                                                                         "Total Private, Non-Profit, Four-Year Institutions",
                                                                         "Total Public Four-Year Institutions",
                                                                         "Total Public Two-Year Institutions"
                                                          ),
                                                          selected = "Grand Total, All Reporting Institutions")
                                       ),
                                       
                                       column(width=4, align="center",
                                              h4(strong("Virginia Summary")),
                                              textOutput("year"),
                                              tags$head(tags$style("#year{font-size: 20px}"))
                                              
                                       )
                                       
                              ),
                              
                              
                              fluidRow(
                                column(1),
                                column(width = 7, 
                                       leafletOutput("leafplot", width = "100%", height = "500px")
                                ),
                                
                                column(width = 4, align = "center",
                                       plotOutput("VA_enr_by_type", height = "250px", width = "400px"),
                                       tableOutput("enroll_table"),
                                       plotOutput("FT_PT", height = "150px", width = "360px")
                                )
                                
                              ),
                              #fluidRow(
                              # plotOutput("FT_PT", width = '100%')
                              #),
                              
                              fluidRow(align="center",
                                       print("Source: State Council of Higher Education for Virginia, 
                            Tables LD03, E02, and FA31C")
                              ),
                              
                              hr(),
                              fluidRow(align="center",
                                       h3(strong("In-State Undergraduate Demographic Trends")),
                                       br()),
                              fluidRow(align="center",
                                       selectInput("hi_ed_type", "Higher-Education Type",
                                                   choices = list("Grand Total, All Reporting Institutions",
                                                                  "Total Private, Non-Profit, Four-Year Institutions",
                                                                  "Total Public Four-Year Institutions",
                                                                  "Total Public Two-Year Institutions"),
                                                   
                                                   selected = "Grand Total, All Reporting Institutions")),
                              fluidRow( style = "margin: 20px;",
                                        column(width = 6, plotOutput("race_eth", width = '100%', height = "450px")),
                                        column(width = 6, plotOutput("gender", width = '100%'))),
                              
                              fluidRow(align="center", style = "margin: 20px;",
                                       print("Source: State Council of Higher Education for Virginia, Table E02")
                              )
                              
             )
             
    ),
    
    # FAFSA tab ----------------------------------------------------------
   
     tabPanel(h4("FAFSA Plots"),
              
              br(),
              fluidRow(width = 12,
                       column(width=2, align="center",
                              selectInput("faf_state", h4(strong("Select a State")),
                                          choices = list("Iowa",
                                                         "Virginia"
                                          ),
                                          selected = "Iowa"),
                              
                              conditionalPanel("input.faf_state == 'Iowa'", 
                                               
                                               fluidRow(width = 12, style = "margin: 20px 0px 20px 20px",
                                                        img(src = "IA/aea-map.jpg", width = "100%"),
                                                               p("Graphic Source: http://www.iowaaea.org/find-my-aea/"))),
                              conditionalPanel("input.faf_state == 'Virginia'", 
                                               
                                               fluidRow(width = 12, style = "margin: 20px 0px 20px 20px",
                                                        img(src = "region-map.png", width = "100%")))
                              
                       ),column(1),
                       column(width = 8, align = "left",br(),
                              p("The parallel coordinate plots display the number of completed Free Applications for Federal Student Aid (FAFSA)
                                                    over time for each high school by AEA Region. There is a line for each high school; high schools with
                                                    greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21 are displayed with orange lines
                                                    and identified in the table. Notched yellow box plots for each year display the median number of completed applications
                                                    (center of notch), the upper quartile (top of the box), and lower quartile (bottom of the box). 50% of all high
                                                    schools fall within the upper and lower quartile. The notch boundary is the 95% confidence interval of the median."),
                              p("How to interpret the data: Applicants can submit FAFSA over an 18-month period for each school year. For example, 
                                                    the FAFSA for the 2020-2021 award year is available from January 1, 2020 through June 30, 2021. In order to 
                                                    make comparisons over the years and evaluate the impact of COVID-19, the data displayed for each school 
                                                    year, 2015-2016, 2016-2017, 2017-2018, 2018-2019, 2019-2020, 2020-2021, are the number of completed 
                                                    applications through April 30th of the first award year.")),
                       column(1)
              ),
              
              
              conditionalPanel("input.faf_state == 'Iowa'", 
                         
                               
                               fluidRow(width = 12, style = "margin: 20px",
                                        
                                        navlistPanel(widths = c(2, 10),
                                                     
                                                     tabPanel("Central Rivers",
                                                              
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10, h3(strong( "Central Rivers: Out of 51 high schools, 28 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Central Rivers High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("CR_table")
                                                                              #tableHTML(`Central Rivers`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Central Rivers`))) %>% 
                                                                              #  add_css_column(css = list('text-align', 'right'), columns = names(`Central Rivers`))
                                                                       ),
                                                                      column(1)
                                                                      ),
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("CR", height = '700px')
                                                              )
                                                     ),
                                                     
                                                     
                                                     tabPanel("Grant Wood",
                                                              
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10, h3(strong( "Grant Wood: Out of 35 high schools, 22 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Grant Wood High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("GW_table")
                                                                              #tableHTML(`Grant Wood`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Grant Wood`))) %>% 
                                                                              #    add_css_column(css = list('text-align', 'right'), columns = names(`Grant Wood`)),
                                                                       ), 
                                                                      column(1)
                                                                      ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("GW",height = '700px')
                                                              )
                                                     ),
                                                     
                                                     tabPanel("Great Prairie",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Great Prairie: Out of 31 high schools, 17 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Great Prairie High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(`Great Prairie`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Great Prairie`)))),
                                                                              tableOutput("GP_table")
                                                                              ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("GP", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Green Hills",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Green Hills: Out of 43 high schools, 27 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Green Hills High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("GH_table")
                                                                              #tableHTML(`Green Hills`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Green Hills`)))),
                                                                       ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("GH", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Heartland",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Heartland: Out of 55 high schools, 23 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Heartland High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("H_table")
                                                                              #tableHTML(Heartland, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(Heartland)))),
                                                                       ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("H", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Keystone",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Keystone: Out of 22 high schools, 12 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Keystone High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(Keystone, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(Keystone)))),
                                                                              tableOutput("K_table")
                                                                              ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("K", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Mississippi Bend",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Mississippi Bend: Out of 20 high schools, 12 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Mississippi Bend High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(`Mississippi Bend`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Mississippi Bend`)))),
                                                                              tableOutput("MB_table")
                                                                              ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("MB", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     tabPanel("Northwest",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Northwest: Out of 33 high schools, 15 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Northwest High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("N_table")
                                                                              #tableHTML(Northwest, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(Northwest)))),
                                                                             ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("N", height = '700px')
                                                              )
                                                              
                                                     ),
                                                     
                                                     tabPanel("Prairie Lakes",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Prairie Lakes: Out of 28 high schools, 17 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Prairie Lakes High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(`Prairie Lakes`, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(`Prairie Lakes`)))),
                                                                              tableOutput("PL_table")
                                                                       ),
                                                                      column(1)
                                                              ),
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("PL", height = '700px')
                                                              )
                                                     )
                                        )
                               ),
                               
                               
                               br(),
                               fluidRow(width = 12,
                                        column(2),
                                        column(10, align = 'center', 
                                               h3(strong('Relative Percent Difference of Completed FAFSA by County for 2019-20')))
                                        #column(1)
                               ),
                               fluidRow(column(2),
                                        column(10, align = "center", leafletOutput("IA_rel_fafsa", width = "90%", height = "500px"))
                                        #column(1)
                               )
                               
                               
              ),
                               
              
              conditionalPanel("input.faf_state == 'Virginia'",  
                               
                               
                               fluidRow(width = 12, style = "margin: 20px", 
                                        
                                        navlistPanel(widths = c(2, 10),
                                                     
                                                     tabPanel("Region 1",
                                                              
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10, h3(strong( "Region 1: Out of 45 high schools, 32 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 1 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              tableOutput("R1_table")
                                                                              #tableHTML(region1, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region1)))),
                                                                       ),
                                                                      column(1)), 
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region1", height = '700px'))
                                                     ),
                                                     
                                                     
                                                     tabPanel("Region 2",
                                                              
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10, h3(strong( "Region 2: Out of 57 high schools, 44 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 2 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region2, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region2)))),
                                                                              tableOutput("R2_table")),
                                                                        column(1)), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region2",height = '700px'))),
                                                     
                                                     tabPanel("Region 3",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 3: Out of 23 high schools, 15 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 3 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region3, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region3)))),
                                                                              tableOutput("R3_table")),
                                                                      column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region3", height = '700px'))
                                                              
                                                     ),
                                                     tabPanel("Region 4",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 4: Out of 81 high schools, 38 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 4 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region4, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region4)))),
                                                                              tableOutput("R4_table")),
                                                                       column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region4", height = '700px'))
                                                              
                                                     ),
                                                     tabPanel("Region 5",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 5: Out of 35 high schools, 23 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 5 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region5, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region5)))),
                                                                              tableOutput("R5_table")),
                                                                       column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region5", height = '700px'))
                                                              
                                                     ),
                                                     tabPanel("Region 6",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 6: Out of 29 high schools, 20 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 6 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region6, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region6)))),
                                                                              tableOutput("R6_table")),
                                                                       column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region6", height = '700px'))
                                                              
                                                     ),
                                                     tabPanel("Region 7",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 7: Out of 39 high schools, 23 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 7 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region7, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region7)))),
                                                                              tableOutput("R7_table")),
                                                                       column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region7", height = '700px'))
                                                              
                                                     ),
                                                     tabPanel("Region 8",
                                                              fluidRow(width =12,
                                                                       column(1),
                                                                       column(10,
                                                                              h3(strong( "Region 8: Out of 13 high schools, 10 had a decrease in completed FAFSA applications from 2019-20 to 2020-21; orange lines identify those high schools with a decrease of >25%.")),
                                                                              hr(),
                                                                              strong("Region 8 High Schools with greater than a 25% reduction in completed FAFSA applications from 2019-20 to 2020-21"),
                                                                              p(),
                                                                              #tableHTML(region8, border = 0, widths = c(250, 250, 250, 250), rownames = FALSE, headers = gsub("\\.", " ", colnames(region8)))),
                                                                              tableOutput("R8_table")),
                                                                       column(1)
                                                              ), 
                                                              
                                                              fluidRow(width = 12, style = "margin: 20px",
                                                                       plotlyOutput("region8", height = '700px'))
                                                              
                                                     )
                                                     
                                        )
                                        
                               ), 
                               
                               br(),
                               fluidRow(width = 12,
                                        column(2),
                                        column(10, align = 'center', 
                                               h3(strong('Relative Percent Difference of Completed FAFSA by School Division for 2019-20'))),
                                        #column(1)
                                        ),
                               fluidRow(column(2),
                                        column(10, align = "center", leafletOutput("division", width = "90%", height = "500px")),
                                        #column(1)
                               )
              )
     ),
    
    
    # data sources tab ---------------------------------------------------
    
    tabPanel(h4("Data Sources"),
             fluidRow(width = 12, style = "margin: 20px", h3(strong('Federal')), align = 'center'),
             fluidRow(width = 12, style = "margin: 20px",
                      column(3,align = "center", tags$a(href = "https://data.census.gov/cedsci/table?g=0400000US51.050000&y=2018&tid=ACSDT5Y2018.B01001&t=Age%20and%20Sex&hidePreview=true&vintage=2018&layer=VT_2018_050_00_PY_D1&cid=B01001_001E", tags$img(src = "census.jpg", align = "top", width = '60%'))),
                      column(9, wellPanel(p("The", tags$a(href = "https://data.census.gov/cedsci/table?g=0400000US51.050000&y=2018&tid=ACSDT5Y2018.B01001&t=Age%20and%20Sex&hidePreview=true&vintage=2018&layer=VT_2018_050_00_PY_D1&cid=B01001_001E", tags$i("American Community Survey")), "(ACS) is a continuous survey sponsored by the U.S. Census Bureau. 
                                            A new sample of 250,000 nationally representative households is sampled every month and the publicly 
                                            available data products are cumulations of monthly data for 1-year and 5-year periods. 
                                            The questions on the ACS address: ancestry, educational attainment, income, language proficiency, 
                                            migration, disability, employment, housing characteristics, and more."), 
                                          p("The Sex By Age (B01001) table from the ACS 2014/18 5-Year Estimates Detailed Tables was downloaded to compute percentage of 18-24 year olds within each county/city out of all 18-24 year olds 
                                            in Virginia.")))), 
             fluidRow(width = 12, style = "margin: 20px",
                      column(3, align = "center", tags$a(href = "https://studentaid.gov/data-center/student/application-volume/fafsa-completion-high-school" ,tags$img(src = "fsa.jpg", align = "top", width = '100%'))),
                      column(9, wellPanel(p("Federal Student Aid (FSA) is an office of the U.S. Department of Education and the largest provider of student 
                                            financial aid in the U.S. FSA provides student financial assistance in the form of grants, loans, and work-study
                                            funds.", tags$a(href = "https://studentaid.gov/data-center/student/application-volume/fafsa-completion-high-school", tags$i("The Free Application for Federal Student Aid")), "(FAFSA) is a form completed by current and prospective college
                                            students in the United States to determine their eligibility for student financial aid. "), 
                                          p("Applicants can submit FAFSAs over an 18-month period for each school year. For example,
                                            the FAFSA for the 2019-2020 award year is available from January 1, 2019 through June 30, 2020.  As a result, the 
                                            application reports include six quarters of data for each award year. The data included reflect the number of submitted and
                                            completed FAFSAs among first-time filing applicants no older than 19 at the cutoff date who will have received their high school
                                            diploma by the start of the school year to which they are applying for aid. FAFSA Virginia data for the applications 
                                            processed by April 30th for the years 2015-16, 2016-2017, 2017-18, 2018-19, and 2019-20 were used in the dashboard, 
                                            variables include school name, location, application submitted and applications completed.")))
                      ),
             
             fluidRow(width = 12, style = "margin: 20px",
                      column(3, align = "center", tags$a(href = "https://nces.ed.gov/ipeds/use-the-data" ,tags$img(src = "IA/ipeds.png", align = "top", width = '100%'))),
                      column(9, wellPanel(p("...", tags$a(href = "https://nces.ed.gov/ipeds/use-the-data", tags$i("The Integrated Postsecondary Education Data System")), 
                                            "... "), 
                                          p("...")))
             ),
             
             
             fluidRow(width = 12, style = "margin: 20px", h3(strong('State')), align = "center"),
             fluidRow(width = 12, style = "margin: 20px",
                      column(3, align = "center", tags$a(href = "https://p1pe.doe.virginia.gov/apex/f?p=180:1:12869447590039:::::", tags$img(
                        src = "vdoe.png",
                        align = "top", width = '100%'))),
                      column(9, wellPanel(p("The Virginia Department of Education (VDOE) annually collects statistics on the number of students enrolled in 
                                            public school on September 30. This report, known as", tags$a(href = "https://p1pe.doe.virginia.gov/apex/f?p=180:1:12869447590039:::::", tags$i("Fall Membership")), ", is submitted by each school in Virginia that 
                                            officially enrolls students (i.e. student records are maintained on a Virginia teacher's register or automated system). 
                                            Data are collected at the student-level and are limited to one active record per student within the state."), 
                                          p("The Fall Membership Build-A-Table was used to download the percentage of economically disadvantaged seniors for 
                                            the 2019-20 school year for each high school. Economically disadvantaged seniors are those who have met of one if the 
                                            following criteria : 1) is eligible for Free/Reduced Meals, or 2) receives TANF, or 3) is eligible for Medicaid, or 4) 
                                            identified as either Migrant or experiencing Homelessness. English learners are those seniors who once received English 
                                            learner services and finished an English learner program within the last four school years.")))),
             fluidRow(width = 12, style = "margin: 20px",
                      column(3, tags$a(href = "https://schoolquality.virginia.gov/download-data", tags$img(src = "vdoe_sqp.jpg", align = "top", width = '100%'))),
                      column(9, wellPanel(p(tags$a(href = "https://schoolquality.virginia.gov/download-data", tags$i("Virginia’s School Quality Profiles")), "provide information about student achievement, college and career readiness, 
                                            program completion, school safety, teacher quality and other topics of interest to parents and the general public. 
                                            Report cards are available for schools, school divisions and for the commonwealth."), 
                                          p("The school quality profile dashboard was used to download the number of seniors for that last three academic years by high schools.")))),
             fluidRow(width = 12, style = "margin: 20px",
                      column(3,tags$a(href = "https://research.schev.edu/info/Reports.Guide-to-the-Data-for-Localities-Reports", tags$img(src = "schev.jpg", width = '100%'))),
                      column(9, wellPanel(p('The', tags$a(href = "https://research.schev.edu/info/Reports.Guide-to-the-Data-for-Localities-Reports", tags$i('State Council of Higher Education for Virginia (SCHEV)')), 'is the Commonwealth\'s coordinating body for higher education. SCHEV was established by the Governor and General Assembly in 1956. 
                                            Their mission is "to advocate and promote the development and operation of an educationally and economically sound, vigorous, 
                                            progressive, and coordinated system of higher education in the Commonwealth of Virginia and to lead state-level strategic planning
                                            and policy development and implementation based on research and analysis."'),
                                          p('Total college and university enrollment, based on the reported locality (county or city) 
                                            for all students at time of admission, were downloaded from table LD03. Percentage of fall undergraduates who received PELL, based on the reported locality (county or city), 
                                            were downloaded from table FA31C. Fall undergraduate enrollment status (full-time or part-time), race/ethnicity, and gender 
                                            were downloaded from table E02.')))
                                          ),
             
             fluidRow(width = 12, style = "margin: 20px",
                      column(3, align = "center", tags$a(href = "https://educateiowa.gov/data-and-reporting/education-statistics", tags$img(src = "IA/IA_doe.jpg", width = '40%'))),
                      column(9, wellPanel(p('The', tags$a(href = "https://educateiowa.gov/data-and-reporting/education-statistics", tags$i('Iowa Department of Education')), '...."'),
                                          p('...')))
             )
             
             ),
    tabPanel(h4("Data Downloads"),
             fluidRow(width = 12, style = "margin: 20px",
                      
                      fluidRow(width = 12,
                               column(width=4, align="left",
                                      selectInput("download_state", h4(strong("Select a State")),
                                                  choices = list("Iowa",
                                                                 "Virginia"
                                                  ),
                                                  selected = "Iowa")
                               )
                      ),
                      
                      conditionalPanel("input.download_state == 'Virginia'",
                      
                          fluidRow(
                            column(12,
                                   p("Use the CSV or Excel button below to export the integrated data set."),
                                   tableOutput("virginia_dictionary")

                            )),
                          br(),
                          DT::dataTableOutput("data_downloads")
                      )
             )),
    tabPanel(h4("Resources"),
             
             fluidRow(width = 12, style = "margin: 20px", 
                      p("Resources are provided to help",br(), 
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp"), "· with the financial aid appeals process and", br(),
                        HTML("&nbsp;&nbsp;&nbsp;&nbsp"), "· understand the impact of COVID-19 on high school and college students."),
                      br(), 
                      fluidRow(width = 12, 
                               column(3, align = "center", tags$a(href = "https://www.newamerica.org/education-policy/reports/highered-polling-dashboard/?utm_medium=email&utm_campaign=EdCentral%20555&utm_content=EdCentral%20555+CID_b49307ca85778bf8c66db275b7075f88&utm_source=Campaign%20Monitor%20Newsletters&utm_term=HigherEd%20Polling%20Dashboard", 
                                                                  tags$img(src = "newamerica.png", align = "top", width = '75%'))),
                               
                               column(9, wellPanel(strong("New America HigherEd Polling Dashboard"),
                                                   br(),
                                                   p("The", tags$a(href = "https://www.newamerica.org/education-policy/reports/highered-polling-dashboard/?utm_medium=email&utm_campaign=EdCentral%20555&utm_content=EdCentral%20555+CID_b49307ca85778bf8c66db275b7075f88&utm_source=Campaign%20Monitor%20Newsletters&utm_term=HigherEd%20Polling%20Dashboard", tags$i("HigherEd Polling Dashboard")), "provides links to public opinion surveys 
                                                     on higher education that have been conducted in the U.S. since 2010. 
                                                     Current surveys topics include the impact of COVID-19 on high school and college bound high students.")))),
                      br(),
                      fluidRow(width = 12, 
                               column(3, align = 'center',
                                      tags$a(href= "https://formswift.com/swift-student", 
                                             tags$img(align = "top", width = "30%", src = "shsf.png"))),
                               column(9, wellPanel(strong("Help with Financial Aid Appeals"),
                                                   br(),
                                                   p("New America has partnered with the Seldin/Haring-Smith Foundation to launch a free digital 
                                                     tool for college students to learn about the financial aid appeals process.", tags$a(href= "https://formswift.com/swift-student",tags$i("SwiftStudent")), "walks 
                                                     students through common financial aid appeal scenarios, such as the need for a childcare allowance, new financial 
                                                     issues not included in the FAFSA, and disability-related expenses. Through SwiftStudent, students can learn about the 
                                                     financial aid appeal process, review eligibility requirements, and customize free template appeal letters to submit to their college financial aid office.")))),
                      br(),
                      fluidRow(width = 12,
                               column(3,align = "center", tags$a(href = "https://muse.jhu.edu/article/754381",
                                                                 tags$img(align = "top", width = "75%", src = "sacsa.png"))),
                               column(9, wellPanel(strong("First-Generation College Student Financial Aid: Results From A National Financial Aid Jargon Survey"),
                                                   p(),
                                                   p("Taylor, Z.W. and I. Bicak. (Spring, 2020)", tags$a(href = "https://muse.jhu.edu/article/754381", tags$i("First-Generation College Student Financial Aid: Results From A National Financial Aid Jargon Survey.")), 
                                                     "College Student Affairs Journal, Volume 38, Number 1, Spring 2020."),
                                                   
                                                   p("“This study assessed prospective first-generation college students' knowledge of federal student aid. 
                                                     The research team surveyed 752 prospective first-generation college students to assess what financial aid jargon terms were unfamiliar. 
                                                     Students often reported FAFSA, master promissory note, entrance counseling, data retrieval tool, and non-filer's statement as unfamiliar. 
                                                     Controlling for demographics, non-binary conforming first-generation college students reported financial aid jargon terms at a higher rate than peers (p=0.05, t=2.42). 
                                                     Implications for student affairs and financial aid praxis are addressed.”")))))),
    tabPanel(h4("Who We Are"),
             fluidRow(width = 12, style = "margin: 20px",
                      column(12,  
                             p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the 
                               Biocomplexity Institute and Initiative at the University of Virginia. SDAD combines expertise in statistics 
                               and social and behavioral sciences to develop evidence-based research and quantitative methods to inform policy 
                               decision-making and evaluation. The researchers at SDAD span many disciplines including 
                               statistics, economics, sociology, psychology, political science, policy, health IT, public health, program evaluation, and data science. 
                               The SDAD office is located near our nation's capital in Arlington, VA. You can 
                               learn more about us at", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "https://biocomplexity.virginia.edu/social-decision-analytics"), "."), 
                             
                             p("Questions about the dashboard can be directed to :"),
                             
                             p(a(href = "mailto:val7zv@virginia.edu"," Vicki Lancaster"), ", Principal Scientist"),
                             
                             p(a(href = "mailto:kjl5t@virginia.edu"," Kathryn Linehan"), ", Research Scientist"),
                             
                             p(a(href = "mailto:sm9dv@virginia.edu", "Sarah McDonald"),  ", Data Science for the Public Good Fellow")))
             )
    ),
  hr(),
  
  fluidRow(style = "margin: 20px",
           width = 12, 
           column(12, align = 'center',
                  em('Last updated: September 2020'))
  )
                                                   )


# server-----------------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  
  output$data_downloads <- DT::renderDataTable({
    DT::datatable(data_download[,-c(1,4, 7, 31, 32)] ,
                  
                  colnames = c("District Number" = "Distract.Number",
                               "School Number" = "School.Number",
                               "Division Name" = "Division.Name"),
                  
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  options = list(  pageLength = 10, scrollX = T,
                                  buttons = list(list(extend = 'csv', filename= 'data_download'),
                                                 list(extend = 'excel', filename = 'data_download')), dom="BlfrtipS", iDisplayLength=-1)
    )
  })
  

  # VA high school map server fcn ---------------------------------------------------
  
  hs_map <- reactive({
    if (input$showPoints){
    leaflet(data = superintendent, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>% 
        setView(zoom = 6.5, lat = 38.032560, lng = -79.422777
        ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = virginia, color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7, fillColor = ~VA_fpal(virginia$CO_Per), fillOpacity = .8) %>%
        addPolylines(color = "#5A5766", weight = 3, fill = "transparent", fillOpacity = 0, opacity = 1) %>% 
        addPolygons(data = virginia, color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7, fillOpacity = 0, label =VA_co_labels, labelOptions = labelOptions(direction = "top",
                                                                                                                                                                                        style = list("font-size" = "12px",
                                                                                                                                                                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                                                                                                                                                                     "text-align" = "left",
                                                                                                                                                                                                     direction = "auto"
                                                                                                                                                                                        )))%>%
        addCircleMarkers(data = hs, radius = 5, fillColor = "orange", fillOpacity = 0.6, stroke = TRUE, color = "orange", opacity = 0.6, weight = 1,  popup= labels)%>%
        setMapWidgetStyle(list(background= "transparent"))  %>%
        # addLegend("bottomleft", pal = VA_fpal, values = ~virginia$bins,
        #           title = "% High School Seniors that have Completed FAFSA Forms", opacity = 1)
      addLegend("bottomleft",
                pal = VA_fpal,
                values =  ~round(virginia$CO_Per, 0),
                title = "% High School Seniors that have Completed FAFSA Forms",  #by<br>Quintile Group",
                opacity = 0.7,
                na.label = "Not Available",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
      
    } else {
      leaflet(data = superintendent, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>% 
        setView(zoom = 6.5, lat = 38.032560, lng = -79.422777
        ) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = virginia, color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7, fillColor = ~VA_fpal(virginia$CO_Per), fillOpacity = .8) %>%
        addPolylines(color = "#5A5766", weight = 3, fill = "transparent", fillOpacity = 0, opacity = 1) %>% 
        addPolygons(data = virginia, color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7, fillOpacity = 0, label = VA_co_labels, labelOptions = labelOptions(direction = "top",
                                                                                                                                                                                         style = list("font-size" = "12px",
                                                                                                                                                                                                      "border-color" = "rgba(0,0,0,0.5)",
                                                                                                                                                                                                      "text-align" = "left",
                                                                                                                                                                                                      direction = "auto"
                                                                                                                                                                                         )))%>%
        setMapWidgetStyle(list(background= "transparent"))  %>%
        # addLegend("bottomleft", pal = VA_fpal, values = ~virginia$bins,
        #           title = "% High School Seniors that have Completed FAFSA Forms", opacity = 1)
        addLegend("bottomleft",
                pal = VA_fpal,
                values =  ~round(virginia$CO_Per, 0),
                title = "% High School Seniors that have Completed FAFSA Forms",  #by<br>Quintile Group",
                opacity = 0.7,
                na.label = "Not Available",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
     
    }
  })
 
  output$mymap1 <- renderLeaflet({
    
    hs_map()
    })
  
  
  # IA hs map server code -----------------------------------------------------
  
  IA_hs_map <- reactive({
    if (input$IAshowPoints){
      leaflet(data = IA_fafsa_by_county, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(color = "#5A5766", weight = 0.9, stroke = TRUE, smoothFactor = 0.7,
                    fillColor = ~IA_fpal(perc_complete_20), fillOpacity = .8) %>%
        addPolylines(data = IA_aea, color = "#5A5766", weight = 3, fill = "transparent", fillOpacity = 0, 
                     opacity = 1) %>%
        addPolygons(color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7,
                    fillOpacity = 0, label = IA_co_labels,
                    labelOptions = labelOptions(direction = "top",
                                                style = list("font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)",
                                                              "text-align" = "left",
                                                             direction = "auto"
                                               ))
        )%>%
        addCircleMarkers(data = IA_hs_fafsa, radius = 5, fillColor = "orange", fillOpacity = 0.6, 
                         stroke = TRUE, color = "orange", opacity = 0.6, weight = 1,
                         popup= IA_hs_labels) %>%
        setMapWidgetStyle(list(background= "transparent"))  %>%
        addLegend("bottomleft",
                pal = IA_fpal,
                values =  ~round(perc_complete_20, 0),
                title = "% High School Seniors that have Completed FAFSA Forms",  #by<br>Quintile Group",
                opacity = 0.7,
                na.label = "Not Available",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
      
      
    } else {
      leaflet(data = IA_fafsa_by_county, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(color = "#5A5766", weight = 0.9, stroke = TRUE, smoothFactor = 0.7,
                    fillColor = ~IA_fpal(perc_complete_20), fillOpacity = .8) %>%
        addPolylines(data = IA_aea, color = "#5A5766", weight = 3, fill = "transparent", fillOpacity = 0, 
                     opacity = 1) %>%
        addPolygons(color = "#5A5766", opacity = 1, weight = 0.9, stroke = TRUE, smoothFactor = 0.7,
                    fillOpacity = 0, label = IA_co_labels,
                    labelOptions = labelOptions(direction = "top",
                                                style = list("font-size" = "12px",
                                                             "border-color" = "rgba(0,0,0,0.5)",
                                                             "text-align" = "left",
                                                             direction = "auto"
                                                ))
        )%>%
        # addCircleMarkers(data = IA_hs_fafsa, radius = 5, fillColor = "orange", fillOpacity = 0.6, 
        #                  stroke = TRUE, color = "orange", opacity = 0.6, weight = 1,
        #                  popup= IA_hs_labels) %>%
        setMapWidgetStyle(list(background= "transparent"))  %>%
        addLegend("bottomleft",
                  pal = IA_fpal,
                  values =  ~round(perc_complete_20, 0),
                  title = "% High School Seniors that have Completed FAFSA Forms",  #by<br>Quintile Group",
                  opacity = 0.7,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      
    }
  })
  
  
  
  output$IA_hs_tab_map <- renderLeaflet({
    
    IA_hs_map()
    
  })
  
  
  # relative fafsa maps ----------------------------------------------
  
  
  output$division <- renderLeaflet({
    
    test <- colorRampPalette( c("#D94801","#F9F1CB", "#084594"))(9)
    
    test <- test[-9]
    
    pal <- colorFactor(palette = test, domain = division$bin, na.color = "transparent")
    show_col(test)
    
    
    leaflet(division, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
      setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor =  ~pal(bin), color = "black", opacity = 1, weight = 1, fillOpacity = .8,  label = label_division, 
                  labelOptions = labelOptions(direction = "top",           
                                              style = list(
                                                "font-size" = "12px",
                                                "text-align" = "left",
                                                direction = "auto"))) %>%
      setMapWidgetStyle(list(background= "transparent"))  %>%
      addLegend("bottomleft", pal = pal, values = ~bin,
                title = "Relative Percent Difference 2019-20", opacity = 1)
    
    
  })
  
  output$IA_rel_fafsa <- renderLeaflet({
    
    test <- colorRampPalette( c("#D94801","#F9F1CB", "#084594"))(9)
    
    test <- test[-9]
    
    pal <- colorBin(palette = test, domain = IA_fafsa_by_county$pct_rel_diff, bins = 8)
    show_col(test)
    
    
    leaflet(IA_fafsa_by_county, options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor =  ~pal(pct_rel_diff), color = "black", opacity = 1, weight = 1, 
                  stroke = TRUE, smoothFactor = 0.7,
                  fillOpacity = .8,  label = IA_relfaf_labels, 
                  labelOptions = labelOptions(direction = "top",           
                                              style = list(
                                                "font-size" = "12px",
                                                "text-align" = "left",
                                                direction = "auto"))) %>%
      setMapWidgetStyle(list(background= "transparent"))  %>%
      addLegend("bottomleft", pal = pal, values = ~pct_rel_diff,
                title = "Relative Percent Difference 2019-20", opacity = 1)
    
    
  })
  
  
  
  
  output$region1 <- renderPlotly({
    ly1
    
  })
  output$region2 <- renderPlotly({
    ly2
    
  })
  output$region3 <- renderPlotly({
    ly3
    
  })
  output$region4 <- renderPlotly({
    ly4
    
  })
  output$region5 <- renderPlotly({
    ly5
    
  })
  output$region6 <- renderPlotly({
    ly6
    
  })
  output$region7 <- renderPlotly({
    ly7
    
  })
  
  output$region8 <- renderPlotly({
    ly8
    
  })
  
  # VA college map ------------------------------------------------------------------
  
  output$leafplot <- renderLeaflet({
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19", "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16", "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    
    selected_type <- switch(input$whichtype,
                            "Grand Total, All Reporting Institutions" = selected_year$ALL,
                            "Total Private, Non-Profit, Four-Year Institutions" = selected_year$PR_4YR,
                            "Total Public Four-Year Institutions" = selected_year$PUB_4YR,
                            "Total Public Two-Year Institutions" = selected_year$PUB_2YR )
    
    
    
    selected_year_pell <- switch(input$whichyear,
                                 "2019-2020" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                                      "19_20 ALL Perc PELL",  "19_20 PR4 Perc PELL",
                                                      "19_20 PUB4 Perc PELL", "19_20 PUB2 Perc PELL")],
                                 "2018-2019" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                                      "18_19 ALL Perc PELL",  "18_19 PR4 Perc PELL",
                                                      "18_19 PUB4 Perc PELL", "18_19 PUB2 Perc PELL")],
                                 "2017-2018" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                                      "17_18 ALL Perc PELL",  "17_18 PR4 Perc PELL",
                                                      "17_18 PUB4 Perc PELL", "17_18 PUB2 Perc PELL")],
                                 "2016-2017" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                                      "16_17 ALL Perc PELL",  "16_17 PR4 Perc PELL",
                                                      "16_17 PUB4 Perc PELL", "16_17 PUB2 Perc PELL")],
                                 "2015-2016" = pell[c("STATEFP", "COUNTYFP", "NAME",
                                                      "15_16 ALL Perc PELL",  "15_16 PR4 Perc PELL",
                                                      "15_16 PUB4 Perc PELL", "15_16 PUB2 Perc PELL")])
    
    old_names <- colnames(selected_year_pell)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL_pell", "PR4YR_pell", "PUB4YR_pell", "PUB2YR_pell", "geometry")
    selected_year_pell <- selected_year_pell %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    
    selected_pell <- switch(input$whichtype,
                            "Grand Total, All Reporting Institutions" = selected_year_pell$ALL_pell,
                            "Total Private, Non-Profit, Four-Year Institutions" = selected_year_pell$PR4YR_pell,
                            "Total Public Four-Year Institutions" = selected_year_pell$PUB4YR_pell,
                            "Total Public Two-Year Institutions" = selected_year_pell$PUB2YR_pell )
    
    
    
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            selected_year$NAME,
            "<br />",
            "<strong># of Students: </strong>",
            comma(selected_type, accuracy = 1),
            "<br />",
            "<strong>% of Fall Students Who Received PELL: </strong>",
            substr(selected_pell, start=1, stop=2),
            "%"),
      htmltools::HTML
    )
    
    label3 <- lapply(
      paste("<strong>College Name: </strong>",
            colleges$Cllg_Nm,
            "<br />",
            "<strong>Website: </strong>", 
            colleges$Website
            
      ),
      htmltools::HTML
    )
    
    
    collegeColor <- colorFactor(palette = c("#E69F00", "#009E73", "#CC79A7"), levels = c(1,2,4))
    
    pal <- colorQuantile(palette ="Blues", domain = selected_type, probs = seq(0, 1, length = 6), 
                             na.color = 'gray', right = FALSE)
    
    leaflet(data = selected_year,  options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
      setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(selected_type), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "#5A5766",
                  smoothFactor = 0.7,
                  layerId = ~COUNTYFP,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addCircleMarkers(data = colleges, radius = 5, fillColor = ~collegeColor(colleges$Type), fillOpacity = 0.7, stroke = TRUE, color =  ~collegeColor(colleges$Type), opacity = 0.7, weight = 1,  popup= label3)%>%
      addLegend(
        pal = pal, 
        position = "bottomleft",
                values = ~(round(selected_type, 0)),
                #colors = brewer.pal(8, "Blues"), #c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                #labels = c("0-250", "251-500", "501-1,000", "1,001-2,500", "2,501-5,000",
                #           "5,001-10,000", "10,001-20,000", "20,001+"),
                title = "Number of Students",
                opacity = 1,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
    
  })
  
  
  output$enroll_table <- renderTable({
    
    # get data for the input year
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19", "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16", "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    # build table
    
    total_all <- sum(selected_year$ALL)
    total_pub4yr <- sum(selected_year$PUB_4YR)
    total_pr4yr <- sum(selected_year$PR_4YR)
    total_pub2yr <- sum(selected_year$PUB_2YR)
    
    Enrollment <- c(total_pub4yr, total_pr4yr, total_pub2yr, total_all)
    Type <- c("Four-Year Public", "Four-Year Private", "Two-Year Public", "Total")
    data <- data.frame(Type, Enrollment)
    data$Enrollment <- comma(as.integer(data$Enrollment), format='d')
    
    data
  })
  
  
  output$VA_enr_by_type <- renderPlot({
    
    # get data for the input year
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2019_20", "PR_4YR_2019_20", 
                                                  "PUB_4YR_2019_20", "PUB_2YR_2019_20", "geometry")],
                            "2018-2019" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2018_19", "PR_4YR_2018_19", 
                                                  "PUB_4YR_2018_19", "PUB_2YR_2018_19", "geometry")],
                            "2017-2018" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2017_18", "PR_4YR_2017_18", 
                                                  "PUB_4YR_2017_18", "PUB_2YR_2017_18", "geometry")],
                            "2016-2017" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2016_17", "PR_4YR_2016_17", 
                                                  "PUB_4YR_2016_17", "PUB_2YR_2016_17", "geometry")],
                            "2015-2016" = hi_ed[c("STATEFP", "COUNTYFP", "NAME", 
                                                  "ALL_2015_16", "PR_4YR_2015_16", 
                                                  "PUB_4YR_2015_16", "PUB_2YR_2015_16", "geometry")])
    
    selected_year <- st_transform(selected_year, 4326)  # ask Teja what this does - some sort of GEO
    
    # rename columns so each year has the same column names
    
    old_names <- colnames(selected_year)
    new_names <- c("STATEFP", "COUNTYFP", "NAME", "ALL", "PR_4YR", "PUB_4YR", "PUB_2YR", "geometry")
    selected_year <- selected_year %>% rename_at(vars(all_of(old_names)), ~ new_names)
    
    # build plot
    
    total_all <- sum(selected_year$ALL)
    total_pub4yr <- sum(selected_year$PUB_4YR)
    total_pr4yr <- sum(selected_year$PR_4YR)
    total_pub2yr <- sum(selected_year$PUB_2YR)
    
    enroll <- c(100*total_pub4yr/total_all, 100*total_pr4yr/total_all, 100*total_pub2yr/total_all)
    type <- c("Four-Year Public", "Four-Year Private", "Two-Year Public")
    
    data <- data.frame(type, enroll)
    data <- data %>% arrange(desc(type)) %>%
      mutate(lab.ypos = cumsum(enroll) - 0.5*enroll) %>%
      mutate(lab = paste0(type, "\n" ,round(enroll,2), "%"))
    
    
    par(mar = c(1,1,1,1))
    pie(data$enroll, labels = paste(data$type, "   ","\n", round(data$enroll, 2), "%", "  ", "\n"),col = c(cbPalette[8], cbPalette[2], cbPalette[4]),border = "white", main= "")
    
  })
  
  output$year <- renderText({
    
    s <- input$whichyear
    s
    
  })
  
  
  output$race_eth <- renderPlot({
    dat <- demo_df[demo_df$Description == "Foreign/International" | 
                     demo_df$Description == "African American or Black (NH)" |
                     demo_df$Description == "American Indian/Native American (NH)" |
                     demo_df$Description == "Asian/Pacific Islander (NH)" |
                     demo_df$Description == "Hispanic" |
                     demo_df$Description == "White, Caucasian American (NH)" |
                     demo_df$Description == "Multi-Race (NH)" |
                     demo_df$Description == "Unknown/Unreported (NH)", ]
    
    selected_type <- switch(input$hi_ed_type,
                            "Grand Total, All Reporting Institutions" = 
                              dat[dat$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              dat[dat$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              dat[dat$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              dat[dat$Institution == "Total Public Two-Year Institutions", ])
    
    
    ggplot(selected_type, aes(x = `Fall Term`, y = `In-State Percentage`, color = Description)) +
      scale_color_manual(values = cbPalette)+
      scale_y_continuous(breaks = c(0, 10, 20, 30 , 40,50 ,60,70), limits = c(0, 70))+
      geom_line(size = 1) +
      geom_point(size = 3) + 
      theme_SDAD()+
      labs(title = "Race/Ethnicity") +
      xlab("Fall Term") +
      ylab("Percentage of In-State Undergraduates")+
      theme(
        plot.title = element_text(size=18, hjust=0.5),
        axis.text.x=element_text(size=16), 
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title = element_blank(),
        legend.text=element_text(size=12)) + 
      guides(col = guide_legend(ncol =2))
  })
  
  
  output$gender <- renderPlot({
    dat <- demo_df[demo_df$Description == "Men" | 
                     demo_df$Description == "Women" |
                     demo_df$Description == "Gender Unreported", ]
    selected_type <- switch(input$hi_ed_type,
                            "Grand Total, All Reporting Institutions" = 
                              dat[dat$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              dat[dat$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              dat[dat$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              dat[dat$Institution == "Total Public Two-Year Institutions", ]
    )
    
    
    ggplot(selected_type, aes(x = `Fall Term`, y = `In-State Percentage`, color = Description)) +
      scale_color_manual(values = c(cbPalette[2],cbPalette[3],cbPalette[4]))+
      scale_y_continuous(breaks = c(0, 10, 20, 30 , 40,50 ,60, 70), limits = c(0, 70))+
      geom_line(size = 1) +
      geom_point(size =3) +
      theme_SDAD()+
      labs(title = "Gender") +
      xlab("Fall Term") +
      ylab("Percentage of In-State Undergraduates")+
      theme(
        plot.title = element_text(size=18, hjust=0.5),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title = element_blank(),
        legend.text=element_text(size=12))
  })
  
  
  output$FT_PT <- renderPlot({
    
    selected_year <- switch(input$whichyear,
                            "2019-2020" = demo_df[demo_df$`Fall Term` == 2019 &
                                                    (demo_df$Description == "Full-Time" |
                                                       demo_df$Description=="Part-Time"), ],
                            "2018-2019" = demo_df[demo_df$`Fall Term` == 2018 &
                                                    (demo_df$Description == "Full-Time" |
                                                       demo_df$Description=="Part-Time"), ],
                            "2017-2018" = demo_df[demo_df$`Fall Term` == 2017 &
                                                    (demo_df$Description == "Full-Time" |
                                                       demo_df$Description=="Part-Time"), ],
                            "2016-2017" = demo_df[demo_df$`Fall Term` == 2016 &
                                                    (demo_df$Description == "Full-Time" |
                                                       demo_df$Description=="Part-Time"), ],
                            "2015-2016" = demo_df[demo_df$`Fall Term` == 2015 &
                                                    (demo_df$Description == "Full-Time" |
                                                       demo_df$Description=="Part-Time"), ])
    
    
    selected_type <- switch(input$whichtype,
                            "Grand Total, All Reporting Institutions" = 
                              selected_year[selected_year$Institution == "Grand Total, All Reporting Institutions", ],
                            "Total Private, Non-Profit, Four-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Private, Nonprofit, Four-Year Institutions", ],
                            "Total Public Four-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Public Four-Year Institutions", ],
                            "Total Public Two-Year Institutions" = 
                              selected_year[selected_year$Institution == "Total Public Two-Year Institutions", ]
    )
    
    
    type_title <- switch(input$whichtype,
                         "Grand Total, All Reporting Institutions" = 
                           "All Reporting Institutions",
                         "Total Private, Non-Profit, Four-Year Institutions" = 
                           "Private, Nonprofit, Four-Year Institutions", 
                         "Total Public Four-Year Institutions" = 
                           "Public Four-Year Institutions", 
                         "Total Public Two-Year Institutions" = 
                           "Public Two-Year Institutions"
    )
    
    
    
    ggplot(selected_type, aes(y = as.factor(Description), x = `In-State Percentage`, fill = Description)) +
      scale_fill_manual(values = c(cbPalette[5], cbPalette[7]))+
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 20, 40, 60, 80, 100))+
      geom_bar(stat="identity", width = 0.3) +
      labs(title = paste("Fall Enrollment Status", "\n", type_title)) +
      xlab("Percentage of In-State Undergraduates") +
      ylab("") +
      theme_SDAD()+
      theme(plot.title = element_text(size=14, hjust=0.5),
            axis.text.x=element_text(size=12), 
            axis.text.y=element_text(size=12),
            legend.position="none", 
            legend.title = element_blank(),
            aspect.ratio = 1/3) 
    
    
  })
  
  output$CR <- renderPlotly({
    IA_ly1
    
  })
  output$GW <- renderPlotly({
    IA_ly2
    
  })
  output$GP <- renderPlotly({
    IA_ly3
    
  })
  output$GH <- renderPlotly({
    IA_ly4
    
  })
  output$H <- renderPlotly({
    IA_ly5
    
  })
  output$K <- renderPlotly({
    IA_ly6
    
  })
  output$MB <- renderPlotly({
    IA_ly7
    
  })
  
  output$N <- renderPlotly({
    IA_ly8
    
  })
  
  output$PL <- renderPlotly({
    IA_ly9
    
  })
  
  # IA college tab map ----------------------------
  
  output$IAcolleges_map <- renderLeaflet({
    
    selected_year <- switch(input$IAwhichyear,
                            "2018-2019" = IA_grad_plans %>% 
                              filter(year == "2018-2019") %>%
                              select(county_name, priv4, pub4, cc, total, geometry),
                            "2017-2018" = IA_grad_plans %>% 
                              filter(year == "2017-2018") %>%
                              select(county_name, priv4, pub4, cc, total, geometry),
                            "2016-2017" = IA_grad_plans %>% 
                              filter(year == "2016-2017") %>%
                              select(county_name, priv4, pub4, cc, total, geometry),
                            "2015-2016" = IA_grad_plans %>% 
                              filter(year == "2015-2016") %>%
                              select(county_name, priv4, pub4, cc, total, geometry))
    
    #selected_year <- st_transform(selected_year, 4326)  
    
    
    selected_type <- switch(input$IAwhichtype,
                            "All Institutions" = selected_year$total,
                            "Private, Non-Profit, Four-Year Institutions" = selected_year$priv4,
                            "Public Four-Year Institutions" = selected_year$pub4,
                            "Community Colleges" = selected_year$cc)
    
    
    pal <- colorQuantile(palette ="Blues", domain = selected_type, probs = seq(0, 1, length = 6), 
                         na.color = 'gray', right = FALSE)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            selected_year$county_name,
            "<br />",
            "<strong># of Students: </strong>",
            comma(selected_type, accuracy = 1)
      ),
      htmltools::HTML
    )
    
    # label2 <- lapply(paste("<strong>Community College Name: </strong>",
    #                        vccs$Instt_N,
    #                        "<br />",
    #                        "<strong>Website: </strong>",
    #                        paste("<a href = ","https://", paste(vccs$Website), " >", sep = ""),
    #                        vccs$Website, paste("</a>")), htmltools::HTML)
    
    
    
    label3 <- lapply(
      paste("<strong>College Name: </strong>",
            IA_colleges$Name,
            "<br />",
            "<strong>Website: </strong>", 
            paste("<a href = ","https://", paste(IA_colleges$Website), " >", sep = ""),
            IA_colleges$Website, paste("</a>")), 
      htmltools::HTML
    )
    
    
    collegeColor <- colorFactor(palette = c("#E69F00", "#009E73", "#CC79A7"), levels = c(1,2,4))
    
    
    leaflet(data = selected_year) %>%  #,  options = leafletOptions(minZoom = 7, maxZoom = 10)) %>%
      #setView(zoom = 6, lat = 38.032560, lng = -79.422777) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(selected_type), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "#5A5766",
                  smoothFactor = 0.7,
                  #layerId = ~county_name,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addCircleMarkers(data = IA_colleges, radius = 5, fillColor = ~collegeColor(IA_colleges$Sector), 
                       fillOpacity = 0.7, stroke = TRUE, color =  ~collegeColor(IA_colleges$Sector), 
                       opacity = 0.7, weight = 1,  popup= label3)%>%
      addLegend(
        pal = pal, 
        position = "bottomleft",
        values = ~(round(selected_type, 0)),
        title = "Number of Students",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          n = length(cuts)
          paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
        })
    
  })
  
  
  output$IAenroll_table <- renderTable({
    
    # get data for the input year
    
    selected_year <- switch(input$IAwhichyear,
                            "2018-2019" = IA_grad_plans %>% 
                              filter(year == "2018-2019") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2017-2018" = IA_grad_plans %>% 
                              filter(year == "2017-2018") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2016-2017" = IA_grad_plans %>% 
                              filter(year == "2016-2017") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2015-2016" = IA_grad_plans %>% 
                              filter(year == "2015-2016") %>%
                              select(county_name, priv4, pub4, cc, total)) #geometry))
    
    
    #selected_year <- st_transform(selected_year, 4326)  
    
    # build table
    
    total_all <- sum(selected_year$total, na.rm = TRUE)
    total_pub4yr <- sum(selected_year$pub4, na.rm = TRUE)
    total_pr4yr <- sum(selected_year$priv4, na.rm = TRUE)
    total_pub2yr <- sum(selected_year$cc, na.rm = TRUE)
    
    Enrollment <- c(total_pub4yr, total_pr4yr, total_pub2yr, total_all)
    Type <- c("Four-Year Public", "Four-Year Private", "Community College", "Total")
    data <- data.frame(Type, Enrollment)
    data$Enrollment <- comma(as.integer(data$Enrollment), format='d')
    
    data
  })
  
  
  output$IAenroll_plot <- renderPlot({
    
    # get data for the input year
    
    selected_year <- switch(input$IAwhichyear,
                            "2018-2019" = IA_grad_plans %>% 
                              filter(year == "2018-2019") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2017-2018" = IA_grad_plans %>% 
                              filter(year == "2017-2018") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2016-2017" = IA_grad_plans %>% 
                              filter(year == "2016-2017") %>%
                              select(county_name, priv4, pub4, cc, total), #geometry),
                            "2015-2016" = IA_grad_plans %>% 
                              filter(year == "2015-2016") %>%
                              select(county_name, priv4, pub4, cc, total)) #geometry))
    
    #selected_year <- st_transform(selected_year, 4326)  
    
    # build plot
    
    total_all <- sum(selected_year$total, na.rm = TRUE)
    total_pub4yr <- sum(selected_year$pub4,na.rm = TRUE)
    total_pr4yr <- sum(selected_year$priv4, na.rm = TRUE)
    total_pub2yr <- sum(selected_year$cc, na.rm = TRUE)
    
    enroll <- c(100*total_pub4yr/total_all, 100*total_pr4yr/total_all, 100*total_pub2yr/total_all)
    type <- c("Four-Year Public", "Four-Year Private", "Community College")
    
    data <- data.frame(type, enroll)
    data <- data %>% arrange(desc(type)) %>%
      mutate(lab.ypos = cumsum(enroll) - 0.5*enroll) %>%
      mutate(lab = paste0(type, "\n" ,round(enroll,2), "%"))
    
    
    par(mar = c(1,1,1,1))
    pie(data$enroll, labels = paste("\n\n  ", data$type, " \n    ", round(data$enroll, 2), "%", " ", "\n"),col = c(cbPalette[8], cbPalette[2], cbPalette[4]),border = "white", main= "")
    
  })
  
  output$IAyear <- renderText({
    
    s <- input$IAwhichyear
    s
    
  })
  
  output$CR_table <- renderTable({
     
    `Central Rivers`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
   )
  
  output$GW_table <- renderTable({
    
    `Grant Wood`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$GP_table <- renderTable({
    
    `Great Prairie`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$GH_table <- renderTable({
    
    `Green Hills`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$H_table <- renderTable({
    
    `Heartland`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$K_table <- renderTable({
    
    `Keystone`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$MB_table <- renderTable({
    
    `Mississippi Bend`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$N_table <- renderTable({
    
    `Northwest`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$PL_table <- renderTable({
    
    `Prairie Lakes`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R1_table <- renderTable({
    
    `region1`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R2_table <- renderTable({
    
    `region2`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R3_table <- renderTable({
    
    `region3`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R4_table <- renderTable({
    
    `region4`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R5_table <- renderTable({
    
    `region5`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R6_table <- renderTable({
    
    `region6`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R7_table <- renderTable({
    
    `region7`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  output$R8_table <- renderTable({
    
    `region8`}, align = c('lccc'), rownames = FALSE, bordered = TRUE, spacing = "xs"
  )
  
  
  output$virginia_dictionary <- renderTable({
    
    va_dict <- data.frame(Variable = c("Region", "EL12", "MinorityStudents12", "AA12", "DIS12", "POP12", "SUBMIT", "COMPLETE"),
                          Definition = c("The Supervisor Region the high school is located in.",
                                         "The number of students who once received EL services and finished an EL program within the last four school years.",
                                         "The number of students who are American Indian or Alaska Native, Asian, Black (not of Hispanic origin), Hispanic, Native Hawaiian or Pacific Islander, or Non-Hispanic (two or more races).",
                                         "The number of students who are Black, not of Hispanic origin.",
                                         "The number of disadvantaged seniors who are economically diasadvantaged and have met of one if the following criteria : 1) is eligible for Free/Reduced Meals, or 2) receives TANF, or 3) is eligible for Medicaid, or 4) identified as either Migrant or experiencing Homelessness.",
                                         "The the number of graduating seniors in a given year.",
                                         "The number of FAFSA applications submitted from the beginning of the 18 month cycle until April 30th of the following year (i.e. SUBMIT19 is for the 2019-20 cycle that began January 1, 2019 and includes applications through the end of April 30, 2020).",
                                         "The number of FAFSA completed applications from the beginning of the 18 month cycle until April 30th of the following year (i.e. COMPLETE19 is for the 2019-20 cycle that began January 1, 2019 and includes applications through the end of April 30, 2020)."))
    va_dict
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)