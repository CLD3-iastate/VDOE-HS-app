#
# Vicki's parallel coordinates code ----------------------------------------
#

IA_parcoord.val<- function (data, columns = 1:ncol(data), groupColumn = NULL, scale = "std",
                         scaleSummary = "mean", centerObsID = 1, missing = "exclude",
                         order = columns, showPoints = FALSE, splineFactor = FALSE,
                         alphaLines = 1, boxplot = FALSE, shadeBox = NULL, mapping = NULL,
                         title = "") 
{
  
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
  #fact.vars <- c("Region","Division.Name", "LOCATION")
  fact.vars <- c("DECR")  
  if (length(fact.vars) >= 1) {
    for (fact.var in fact.vars) {
      if(fact.var != "DECR")
      {
        data[[fact.var]] <- as.numeric(data[[fact.var]])
      }
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
  #View(data.m)
  
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
                           colour = groupColumn, text = "School.Name.x") #text = "SCHOOL")
  } else {
    mapping2 <- aes_string(x = "variable", y = "value", group = ".ID", text = "School.Name.x")
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