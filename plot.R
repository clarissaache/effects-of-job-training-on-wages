library(gifski)
library(ggplot2)
library(gganimate)


wages <- read.table("./Data//lalondedata.txt",sep=",",header=TRUE)
colorindex <- seq(1:nrow(wages))

colors <- rainbow(nrow(wages))
newdata <- data.frame(index= seq(1:nrow(wages)),year= 1974,wage = wages$re74,treat= wages$treat)#,color_i=colors)
newdata75 <-  data.frame(index= seq(1:nrow(wages)),year= 1975,wage = wages$re75, treat= wages$treat)#color_i=colors)
newdata78 <-  data.frame(index= seq(1:nrow(wages)),year= 1978,wage = wages$re78,treat= wages$treat)#color_i=colors)
newdataall <- rbind(newdata,newdata75,newdata78)
newdataall$colors <- ifelse(newdataall$treat ==0,"blue","red")


ggplot(
  newdataall, 
  aes(x = index, y=wage,color=colors)) +
  geom_point(show.legend = TRUE, alpha = 0.7) +
  scale_size(range = c(2, 12)) +
  labs(x = "index", y = "wage")+
  scale_color_manual(labels=c("not trained","trained"),values = c("blue","red"))+transition_time(year)+ labs(title = "Year: {frame_time}")
anim_save("incomeOveryear.gif")
