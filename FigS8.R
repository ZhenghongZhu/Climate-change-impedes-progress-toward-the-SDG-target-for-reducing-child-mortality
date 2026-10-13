# Figure S8: original four-panel layout, colours, fonts and annotations.
# Run this script with the working directory set to its folder.
# All values come from figure_source_data in the corrected FigS8.RData.
library(ggplot2)
load('FigS8.RData')
d <- figure_source_data$FigS8$block1
colours <- c(No='#F8766D', Yes='#00BFC4')
font <- 'Times New Roman'
width_px <- 1269
height_px <- 1513
width_mm <- 183
height_mm <- width_mm * height_px / width_px
px_per_mm <- width_px / width_mm
text_mm <- function(px) px/px_per_mm
poly <- list(); labels <- list(); lines <- list(); insets <- list(); numbers <- list()
panels <- data.frame(
  outcome=c('U5MR','U5MR','NMR','NMR'),
  scenario=rep(c('SSP2-4.5','Temperature-unrelated scenario'),2),
  cx=c(417,1041,393,1003), cy=c(369,369,1185,1185),
  r0=c(81,81,88,88), unit=c(2.30,2.30,3.50,3.50),
  label_r=c(185,185,210,210),
  inset_left=c(123,742,99,711), inset_top=c(135,135,950,950),
  inset_width=275, inset_height=189,
  threshold=c(25,25,12,12)
)
for (p in seq_len(nrow(panels))) {
  s <- panels[p,]
  bars <- d[d$outcome==s$outcome & d$scenario==s$scenario & d$plot_type=='country_bar',]
  bars <- bars[order(bars$value),]
  # Seventy-two radial slots, with eighteen blank slots as in the original.
  for(i in seq_len(nrow(bars))) {
    theta <- ((i-1)*5 + c(seq(0.10,4.90,length.out=15),seq(4.90,0.10,length.out=15)))*pi/180
    r <- c(rep(s$r0+s$unit*bars$value[i],15),rep(s$r0,15))
    poly[[length(poly)+1]] <- data.frame(x=s$cx+r*sin(theta),y=s$cy-r*cos(theta),id=paste(p,i),status=bars$target_status[i])
    mid <- (i-0.5)*5*pi/180
    lr <- max(s$label_r, s$r0+s$unit*bars$value[i]+10)
    labels[[length(labels)+1]] <- data.frame(x=s$cx+lr*sin(mid),y=s$cy-lr*cos(mid),label=bars$country_code[i],angle=90-(i-0.5)*5)
  }
  theta <- seq(2.5,267.5,length.out=600)*pi/180
  rr <- s$r0+s$unit*s$threshold
  lines[[p]] <- data.frame(x=s$cx+rr*sin(theta),y=s$cy-rr*cos(theta),id=p,outcome=s$outcome)
  sums <- d[d$outcome==s$outcome & d$scenario==s$scenario & d$plot_type=='regional_inset',]
  regions <- c('South Asia','Southeast Africa','West Africa')
  for (j in seq_along(regions)) {
    z <- sums[sums$region==regions[j],]
    yes <- z$value[z$target_status=='Yes']; no <- z$value[z$target_status=='No']
    xleft <- s$inset_left+(j-1)*95
    split <- s$inset_top+s$inset_height*no/100
    insets[[length(insets)+1]] <- data.frame(xmin=xleft,xmax=xleft+85,ymin=c(s$inset_top,split),ymax=c(split,s$inset_top+s$inset_height),status=c('No','Yes'))
    if(no>0) numbers[[length(numbers)+1]] <- data.frame(x=xleft+42.5,y=(s$inset_top+split)/2,label=sprintf('%.1f',no))
    if(yes>0) numbers[[length(numbers)+1]] <- data.frame(x=xleft+42.5,y=(split+s$inset_top+s$inset_height)/2,label=sprintf('%.1f',yes))
  }
}
poly <- do.call(rbind,poly); labels <- do.call(rbind,labels)
lines <- do.call(rbind,lines); insets <- do.call(rbind,insets); numbers <- do.call(rbind,numbers)
fig <- ggplot() +
  geom_polygon(data=poly,aes(x,y,group=id,fill=status),colour=NA)+
  geom_path(data=lines,aes(x,y,group=id,linetype=outcome),linewidth=0.28,colour='black')+
  scale_linetype_manual(values=c(U5MR='22',NMR='dotted'))+
  geom_text(data=labels,aes(x,y,label=label,angle=angle),hjust=0,family=font,size=text_mm(20))+
  scale_fill_manual(values=colours,drop=FALSE)+
  scale_y_reverse(limits=c(height_px,0),expand=expansion(mult=0))+
  coord_fixed(xlim=c(0,width_px),expand=FALSE,clip='off')+
  theme_void()+theme(legend.position='none',plot.margin=grid::unit(c(0,0,0,0),'mm'),plot.background=element_rect(fill='white',colour=NA))
# Inset background grids, category labels and headings match the original.
for(p in seq_len(nrow(panels))) {
  s <- panels[p,]
  gx <- s$inset_left-14
  fig <- fig+
    annotate('rect',xmin=gx,xmax=s$inset_left+289,ymin=s$inset_top-9,ymax=s$inset_top+s$inset_height+9,fill='white',colour=NA)+
    annotate('segment',x=gx,xend=s$inset_left+289,y=seq(s$inset_top,s$inset_top+s$inset_height,length.out=5),yend=seq(s$inset_top,s$inset_top+s$inset_height,length.out=5),colour='grey85',linewidth=0.15)+
    annotate('text',x=s$inset_left+137,y=s$inset_top-28,label='Proportion (%)',family=font,fontface='bold',size=text_mm(33))+
    annotate('text',x=s$inset_left+c(60,155,250),y=s$inset_top+s$inset_height+24,label=c('SA','SEA','WA'),angle=45,family=font,size=text_mm(23))
}
fig <- fig+
  geom_rect(data=insets,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=status),colour=NA)+
  geom_text(data=numbers,aes(x,y,label=label),family=font,size=text_mm(30))+
  annotate('text',x=c(2,2),y=c(27,846),label=c('a','b'),hjust=0,family=font,fontface='bold',size=text_mm(48))+
  annotate('text',x=c(343,982,337,983),y=c(36,33,852,849),label=rep(c('SSP2-4.5','Temperature-unrelated scenario'),2),family=font,fontface='bold',size=text_mm(35))
# Row-specific target-line and No/Yes legends, as in the source image.
for(o in c('U5MR','NMR')) {
  y <- if(o=='U5MR') 669 else 1489
  x0 <- if(o=='U5MR') 140 else 149
  x1 <- if(o=='U5MR') 256 else 237
  tx <- if(o=='U5MR') 270 else 251
  bx <- if(o=='U5MR') 811 else 771
  caption <- if(o=='U5MR') 'Target of SDG 3.2.1 (U5MR, 25 per 1000 live births)' else 'Target of SDG 3.2.2 (NMR, 12 per 1000 live births)'
  goal <- if(o=='U5MR') 'SDG3.2.1 Goal Achievement' else 'SDG3.2.2 Goal Achievement'
  fig <- fig+
    annotate('segment',x=x0,xend=x1,y=y,yend=y,linetype=if(o=='U5MR')'dashed' else 'dotted',linewidth=0.42)+
    annotate('text',x=tx,y=y,label=caption,hjust=0,family=font,fontface='bold',size=text_mm(23))+
    annotate('rect',xmin=bx,xmax=bx+45,ymin=y-20,ymax=y+22,fill=colours['No'])+
    annotate('rect',xmin=bx+45,xmax=bx+91,ymin=y-20,ymax=y+22,fill=colours['Yes'])+
    annotate('text',x=bx+c(22.5,68),y=y,label=c('No','Yes'),family=font,fontface='bold',size=text_mm(23))+
    annotate('text',x=bx+108,y=y,label=goal,hjust=0,family=font,fontface='bold',size=text_mm(23))
}
ggsave('FigS8.pdf',fig,device=cairo_pdf,width=width_mm,height=height_mm,units='mm',family=font,bg='white')
ggsave('FigS8.png',fig,device=ragg::agg_png,width=width_mm,height=height_mm,units='mm',dpi=600,bg='white')
ggsave('FigS8_preview.png',fig,device=ragg::agg_png,width=width_px,height=height_px,units='px',dpi=width_px/(width_mm/25.4),bg='white')
