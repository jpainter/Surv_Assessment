# Forest plot with table from https://github.com/nzcoops/blog_code/blob/master/forest_plot.Rmd

library(ggplot2)
library(gridExtra)

forrest_table = function( dat = NULL ,
                          labs_x = "Adjusted Odds Ratio" 
                          ){ 
  
  if (is.null( dat )) {
    dat <- data.frame(group = factor(c("A","B","C","D","E","F","G"), 
                                     levels=c("F","E","D","C","B","A","G")),
                      cen = c(3.1,2.0,1.6,3.2,3.6,7.6,NA),
                      low = c(2,0.9,0.8,1.5,2,4.2,NA),
                      high = c(6,4,2,6,5,14.5,NA))
  }
  
  
  theme_set(theme_bw())
  theme_update(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = unit(c(0,0,0,0), "lines")
  )
  
  p <- ggplot(dat, aes(cen, group)) + 
    geom_point(size=5, shape=18) +
    geom_errorbarh(aes(xmax = high, xmin = low), height = 0.15) +
    geom_vline(xintercept = 1, linetype = "longdash") +
    scale_x_continuous(breaks = seq(0,14,1), labels = seq(0,14,1)) +
    labs( x = labs_x , y="")
  
  lab <- data.frame(V0 = factor(c("A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G","A","B","C","D","E","F","G"),, levels=c("G","F","E","D","C","B","A")),
                    V05 = rep(c(1,2,3,4),each=7),
                    V1 = c("Occuption","Active","","Inactive","","Inactive","","Recreation","Inactive","","Active","","Inactive","","Gender","Men","Women","Men","Women","Men","Women","OR",3.1,2.0,1.6,3.2,3.6,7.6)
  )
  
  data_table <- ggplot( lab, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
    geom_text(size = 4, hjust=0, vjust=0.5) + theme_bw() +
    geom_hline( aes( yintercept = 6.5 ) ) + 
    geom_hline( aes( yintercept = 7.5 ) ) + 
    theme(panel.grid.major = element_blank(), 
          legend.position = "none",
          panel.border = element_blank(), 
          axis.text.x = element_text(colour="white"),#element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks = element_line(colour="white"),#element_blank(),
          plot.margin = unit(c(0,0,0,0), "lines")) +
    labs( x="", y="") +
    coord_cartesian(xlim=c(1,4.5))
  
  grid.arrange(data_table, p, ncol=2)
}