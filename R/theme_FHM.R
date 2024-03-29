#' @title theme_FHM: custom ggplot2 theme for forestNETN
#'
#' @import ggplot2
#'
#' @description This function customizes a theme for plotting NETN forest data, including removing the
#' default panel grids from ggplot2 figures.
#'
#' @return This function must be used in conjunction with a ggplot object
#'
#' @examples
#' \dontrun{
#' importData()
#'  size_dist <- ggplot(mean_dens, aes(x = size_class, y = stems_ha_mean))+
#'      geom_bar(stat = 'identity', fill = 'CadetBlue', position = position_dodge())+
#'      geom_errorbar(aes(ymin=stems_ha_mean-stems_ha_se, ymax=stems_ha_mean+stems_ha_se, x=size_class),
#'                    color='DimGrey', width=0.2,position=position_dodge(0.9))+
#'      facet_wrap(vars(cycle_fac), ncol=3, labeller=as_labeller(cycle_names))+
#'      labs(x='Regeneration Size Distribution', y='stems/ha')+
#'      scale_x_discrete(labels=c('15-30cm', '30-100cm', '100-150cm', '>150cm','1-10cm DBH'))+
#'      theme_FHM()
#'}
#' @export


theme_FHM <- function(){theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              panel.background = element_rect(color = '#696969', fill = 'white', size = 0.4),
                              plot.background = element_blank(),
                              strip.background = element_rect(color = '#696969', fill = 'grey90', size = 0.4),
                              legend.key = element_blank(),
                              axis.line.x = element_line(color = "#696969", size = 0.4),
                              axis.line.y = element_line(color = "#696969", size = 0.4),
                              axis.ticks = element_line(color = "#696969", size = 0.4)
)}
