#' @include treeMapPrep.R
#' @title treeMap: creates plot of trees by status and size
#'
#' @description This function converts tree distance and azimuth values to coordinates and plots the coordinates of live or
#' dead trees. Trees are color coded by status, and size is relative to DBH. Works best with data.frame derived
#' from joinLocEvent and joinTreeData, then cleaned up with treeMapPrep(). Requires a data.frame with the following fields:
#'  Plot_Name, Tree_Number_NETN, Distance, Azimuth, Orientation, Status_ID (n=7), DBH, x, and y.
#'
#' @return Returns a map of trees on a given plot
#'
#' @export
#'
#------------------------
# Plots tree map by status and size
#------------------------
treeMap<-function(df){

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Package 'ggplot2' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("ggrepel", quietly = TRUE)){
    stop("Package 'ggrepel' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("cowplot", quietly = TRUE)){
    stop("Package 'cowplot' needed for this function to work. Please install it.", call. = FALSE)
  }

  df<-treeMapPrep(df)

  status_cols<-c("#d9f008","#a6db12","#73c71c","#009900", "#00f2ff", "#0066ff", "#3300CC")
  names(status_cols)<-as.character(c('AB','AF','AL','AS','DB','DL','DS'))
  park=unique(df$Unit_Code)
  orient<-paste0(unique(df$Plot_Name),' Orientation: ',unique(df$Orientation))
  if(park=='ACAD'){
    p<-ggplot(data=df,aes(x=x, y=y, group=Status_ID, fill=Status_ID, size=DBH, label=Tree_Number_NETN))+
      geom_hline(yintercept=0, lwd=1, color='DimGrey')+
      geom_vline(xintercept=0, lwd=1, color='DimGrey')+
      geom_jitter(aes(fill=Status_ID),shape=21, width=0.2)+ xlim(-8.7,8.7)+ylim(-8.7,8.7)+
      scale_fill_manual(values=status_cols)+
      theme_bw()+
      theme(panel.background=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.text=element_blank(),
          plot.margin=unit(c(3,2,2,2), 'lines'), legend.position='none',
          legend.spacing.y=unit(0.05,'cm'), legend.text=element_text(size=10))+
      guides(shape=T, size=F)+
      scale_size_continuous(range=c(2,10))+
      geom_text_repel(aes(x=x,y=y,label=Tree_Number_NETN), direction='both', size=5, nudge_x=0.1,nudge_y=0.1)+
      coord_cartesian(xlim=c(-8.7,8.7), clip='off')+
      labs(x=NULL,y=NULL, fill='Status')+
      geom_text(x=0, y=9.8, size=5, label='UP')+ geom_text(x=9.75,y=9.75,size=5,label='UR')+
      geom_text(x=9.75,y=-9.75,size=5,label='BR')+ geom_text(x=-9.75,y=-9.75,size=5,label='BL')+
      geom_text(x=-9.75,y=9.75, size=5, label='UL')+
      geom_text(x=0, y=10.8, label=orient, size=5, col='red')

  leg<-get_legend(ggplot(data=df, aes(x=x, y=y, group=Status_ID, fill=Status_ID))+
    geom_point(aes(fill=Status_ID),shape=21, size=6)+labs(fill='Status')+
    scale_fill_manual(values=status_cols)+
    guides(shape=T))

  print(plot_grid(p,leg,rel_widths=c(1,0.2)))
  } else if (park!='ACAD') {
    p<-ggplot(data=df,aes(x=x, y=y, group=Status_ID, fill=Status_ID, size=DBH, label=Tree_Number_NETN))+
      geom_hline(yintercept=0, lwd=1, color='DimGrey')+
      geom_vline(xintercept=0, lwd=1, color='DimGrey')+
      geom_jitter(aes(fill=Status_ID),shape=21, width=0.2)+ xlim(-11.5,11.5)+ylim(-11.5,11.5)+
      scale_fill_manual(values=status_cols)+
      theme_bw()+
      theme(panel.background=element_blank(), panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(), axis.text=element_blank(),
            plot.margin=unit(c(3,2,2,2), 'lines'), legend.position='none',
            legend.spacing.y=unit(0.05,'cm'), legend.text=element_text(size=10))+
      guides(shape=T, size=F)+
      scale_size_continuous(range=c(2,10))+
      geom_text_repel(aes(x=x,y=y,label=Tree_Number_NETN), direction='both', size=5, nudge_x=0.2,nudge_y=0.2)+
      coord_cartesian(xlim=c(-11.5,11.5), clip='off')+
      labs(x=NULL,y=NULL, fill='Status')+
      geom_text(x=0, y=13.1, size=5, label='UP')+ geom_text(x=13.1,y=13,size=5,label='UR')+
      geom_text(x=13.1,y=-13,size=5,label='BR')+ geom_text(x=-13,y=-13,size=5,label='BL')+
      geom_text(x=-13,y=13, size=5, label='UL')+
      geom_text(x=0, y=14.5, label=orient, size=5, col='red')

    leg<-get_legend(ggplot(data=df, aes(x=x, y=y, group=Status_ID, fill=Status_ID))+
                      geom_point(aes(fill=Status_ID),shape=21, size=6)+labs(fill='Status')+
                      scale_fill_manual(values=status_cols)+
                      guides(shape=T))

    print(plot_grid(p,leg,rel_widths=c(1,0.2)))
  }

  } # end of function

