# rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
# cat("\014")

# ds <- readRDS("./data/simulated/dsX.rds")
# mdl <- glm(formula=iq ~ 1 + ses + parent_edu + house_cost, data=ds)
# mdl
# summod <- summary(mdl)
#
# # parameter estimates
# summod$coefficients
# EF_value=120
# dfF_value=5
# ER_value=336
# dfR_value=6

create_data <- function(EF_value=120, dfF_value=5, ER_value=336, dfR_value=6){
  # sample_size <- 6
  # degrees of freedom of the models
  (dfF <- dfF_value)                     # FULL            ( df ERROR)
  (dfR <- dfR_value)                  # RESTRICTED      ( df TOTAL)
  (dfD <- dfR - dfF)                   # DIFFERENCE      ( df RESIDUAL) - gain in complexity / loss of simplicity

  # misfit of the models
  (SSE <- EF_value);EF <- SSE          # FULL            (SS Error) - (EF)
  (SST <- ER_value); ER <- SST      # RESTRICTED      (SS Total) - (ER)
  (SSR <- SST - SSE)                   # DIFFERENCE      (SS Resisudal) - gain in accurcy / loss of misfit

  # accuracy vs parsimony
  (MSE <- EF / dfF)                     # FULL            (Mean Square Error)
  (MST <- ER / dfR)                     # RESTRICTED      (Mean Square Total)
  (MSR <- (ER - EF) / (dfR - dfF))      # DIFFERENCE      (Mean Square Residual); (MSR <-SSR/dfR)


  # collect squared error in a vector
  (SS_value <- c( EF, ER, ER - EF)) # Full , Restricted, Difference
  # collect degrees of freedom in a vector
  (df_value <- c(dfF, dfR, dfD)) # Full , Restricted, Difference
  # collected average squared error in a vector
  (MS_value <- c(MSE, MST, MSR)) # squared discrepancy per degree of freedom

  # Counterparts
  model_label <- c("Full",    "Restricted",   "Difference")
  SS_Label    <- c("Error",   "Total",        "Residual")
  SS_label    <- c("SSE",     "SST",          "SSR")
  MS_label    <- c("MSE",     "MST",          "MSR")
  df_label    <- c("df(F)",   "df(R)",        "df(D)")

  # misfit as area analogy: creating the squares
  area <- SS_value
  side <- sqrt(area) # sides of the SS square
  origin <- rep(1,3) # the bottom left corner of each square
  sideMS <- sqrt(MS_value) # side of the MS square

  # Create dataset for graphing
  (d <- data.frame(model_label, SS_Label, SS_label, SS_value,
                   df_label, df_value, MS_label, MS_value, origin, area, side, sideMS))
  # create an auxilary column that numbers the models
  (d$position <-  c(3, 1, 2))
  # sort the dataframe in the descending order of model number
  (d <- d[order(d$position),])
  d$SS_value <- round(d$SS_value,2)
  d$MS_value <- round(d$MS_value,2)

  d$dfD <- d[d$MS_label=="MSR", "df_value"]
  d$dfF <- d[d$MS_label=="MSE", "df_value"]
  d$Ftest <- d[d$MS_label=="MSR", "MS_value"] / d[d$MS_label=="MSE", "MS_value"]
  d$Fcrit <- qf(.95, df1=d[d$MS_label=="MSR", "df_value"], df2=d[d$MS_label=="MSE", "df_value"]) # find F critical
  d$label1 <- paste0("F (",d$dfD,",",d$dfF,") = ", round(d$Ftest,2),"  (observed)")
  d$label2 <- paste0("F (",d$dfD,",",d$dfF,") = ", round(d$Fcrit,2), "  (95% crit)")
  d$unit_step <- max(d$side)/10
  d$max_value <- max(d$side) + d$unit_step

  return(d)
}
# d <- create_data()


library(ggplot2)
base::source("./scripts/graphing/graph-presets.R")
areaFcolors <- c( "Full"="blue", "Restricted" = "black","Difference" = "red")


SS_graph <- function(d, scalemax){

  g <- ggplot2::ggplot()
  g <- g + ggtitle("Total misfit of the models")
  g <- g + geom_rect(data=d,
                     mapping=aes(xmin=origin, xmax=side, ymin=origin, ymax=side,
                                 color=model_label, fill=model_label),
                     alpha=.1)
  g <- g + scale_x_continuous( limits=c(0, scalemax))
  g <- g + scale_y_continuous( limits=c(0, scalemax))
  g <- g + scale_fill_manual(values = areaFcolors, guide=FALSE)
  g <- g + scale_color_manual(values = areaFcolors, name="Model", guide=guide_legend(reverse=TRUE))
  g <- g + main_theme
  g <- g + theme(
    axis.title = element_blank(),
    legend.text =  element_text(),
    legend.position="bottom")
  g
  return(g)
}
# SS_graph(d, max=200)
# SS_graph(d)


F_text <- function(d){

  g <- ggplot2::ggplot(data=d)
  g <- g + scale_x_continuous(limits=c(0,4))
  # g <- g + scale_y_continuous(limits=c(-1,4))
  g <- g + geom_text(mapping=aes(x = 0, y = rev(position)+1, color = model_label,
                                 label = paste0(
                                   SS_label, " = ",  SS_value, "  ",
                                   df_label, " = ",  df_value, "  ",
                                   MS_label, " = ",  MS_value)),
                     hjust=0)
  g <- g + geom_text(aes(x=0, y = 1, label = label1 ),hjust=0)
  g <- g + geom_text(aes(x=0, y = 0, label = label2 ),hjust=0)
  g <- g + scale_color_manual(values=areaFcolors)
  g <- g + main_theme
  g <- g + theme(axis.text.y =  element_blank(),
                 axis.text.x =  element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 legend.title = element_blank(),
                 legend.text =  element_text(),
                 legend.position="none",
                 panel.grid = element_blank(),
                 panel.border = element_blank(),
                 axis.ticks = element_blank())
  g
  return(g)
}
# F_text(d)


MS_graph <- function(d, scalemax){

  g <- ggplot2::ggplot()
  g <- g + ggtitle("Misfit per degree of freedom")
  g <- g + geom_rect(data=d,
                     mapping=aes(xmin=origin, xmax=sideMS,
                                 ymin=origin, ymax=sideMS, color=model_label, fill=model_label),
                     alpha=.1)
  g <- g + scale_x_continuous( limits=c(0, scalemax))
  g <- g + scale_y_continuous( limits=c(0, scalemax))
  g <- g + scale_fill_manual(values = areaFcolors, guide=FALSE)
  g <- g + scale_color_manual(values = areaFcolors, name="Model", guide=guide_legend(reverse=TRUE))
  g <- g + main_theme
  g <- g + theme(
    legend.title = element_blank(),
    legend.text =  element_text(),
    legend.position="bottom")
  return(g)
}
# MS_graph(d, max=200)
# MS_graph()


vpLayout <- function(rowIndex, columnIndex) { return( viewport(layout.pos.row=rowIndex, layout.pos.col=columnIndex) ) }
areaF <- function(misfit_full, df_full, misfit_reduced, df_reduced, scalemax=max(d$side)){

  d <- create_data(EF_value=misfit_full, dfF_value=df_full, ER_value=misfit_reduced, dfR_value=df_reduced)
  print(d)
  a <- SS_graph(d, scalemax)
  b <- MS_graph(d, scalemax)
  c <- F_text(d)

  grid.newpage()
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid.layout(nrow=3, ncol=3,
                        widths=unit(c(.36, .28, .36) ,c("null", "null","null")),
                        heights=unit(c(.38, .32, .2), c("null", "null", "null"))
  )
  pushViewport(viewport(layout=layout))
  #   print(a, vp=vpLayout(1, 1))
  #   print(b, vp=vpLayout(1, 2))
  #   print(c, vp=viewport(layout.pos.row=2))
  print(a, vp=viewport(layout.pos.col=1))
  print(c, vp=vpLayout(2, 2))
  print(b, vp=viewport(layout.pos.col=3))

  popViewport(0)
  # return(c)
}

# areaF(6136.292, 26, 6525, 29, 20 )
