---
title: "Untitled"
author: "bolero"
date: "2021/4/8"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r read data, include=FALSE}
library(readxl)
library(plotly)
a01 <-read_excel("a01.xlsx")
attach(a01)
a01$class = as.factor(a01$class)
a01$gender = as.factor(a01$gender)
a01$points = as.numeric(a01$points)
a01$scores = as.numeric(a01$scores)
a01$chinese = as.numeric(a01$chinese)
a01$english = as.numeric(a01$english)
a01$math = as.numeric(a01$math)
a01$nature = as.numeric(a01$nature)
a01$society = as.numeric(a01$society)
a01$writing = as.numeric(a01$writing)
library(tidyverse)
library(gridExtra)
```

## 109年度三年級第一次模擬考成績

# 109年度三年級第一次模擬考成績


```{r 各科箱型圖code, include=FALSE}
p1<- ggplot(a01) +
  aes(y =points) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$points, na.rm = TRUE),col="red") +
  annotate("text",x=0, y=52,label="全校平均值",color="red") +
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("總積點") + 
  ggtitle("第一次模擬考箱形圖")


p2<- ggplot(a01) +
  aes(y =scores) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$scores, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=20.5,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("總積分")  
#ggtitle("第一次模擬考全校總積分箱形圖")


p3<- ggplot(a01) +
  aes(y =chinese) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$chinese, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=10,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("國文積點") +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))

#ggtitle("第一次模擬考全校國文積點箱形圖")

p4<- ggplot(a01) +
  aes(y =english) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$english, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=10,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("英文積點") +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))

#ggtitle("第一次模擬考全校英文積點箱形圖")

p5<- ggplot(a01) +
  aes(y =math) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$math, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=9,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("數學積點")  +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))

#ggtitle("第一次模擬考全校數學積點箱形圖")

p6<- ggplot(a01) +
  aes(y =nature) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$nature, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=9.5,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("自然積點") +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))

#ggtitle("第一次模擬考全校自然積點箱形圖")
p7<- ggplot(a01) +
  aes(y =society) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$society, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=10,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("社會積點")  +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))

#ggtitle("第一次模擬考全校社會積點箱形圖")

p8<- ggplot(a01) +
  aes(y =writing) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$writing, na.rm = TRUE),col="red")+
  annotate("text",x=0, y=3.3,label="全校平均值",color="red")+
  theme_minimal() +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.text.x = element_text(color="NA")) +
  xlab("作文積點") 

#ggtitle("第一次模擬考全校作文積點箱形圖")

```

```{r 結果輸出, echo=FALSE}
grid.arrange(p1,p3,p5,p7,p2,p4,p6,p8,nrow=2)
```

```{r 第一次模擬考各班總積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = points, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE) +
  geom_hline(yintercept = mean(a01$points, na.rm = TRUE),col="red") +
  annotate("text",x=9, y=52,label="全校平均值",color="red") +
  theme_minimal() +
  #theme(axis.title.y = element_text(angle=0)) +
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank()) +
  theme(axis.title.x.bottom = element_blank()) +
  #guides(fill=guide_legend(title = "班級")) +
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(0, 120,20))+
  ggtitle("第一次模擬考各班總積點箱形圖")
)
```

```{r 第一次模擬考各班總積分箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = scores, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$scores, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=21,label="全校平均值",color="red")+
  theme_minimal()+
  #theme(axis.title.y = element_text(angle=0))+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積分\n(分)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(0, 30,5))+
  ggtitle("第一次模擬考各班總積分箱形圖")
)
```

```{r 第一次模擬考各班國文積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01)+
  aes(x = class, y = chinese, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$chinese, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=10,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("3(C)","6(B)","9(B+)","12(B++)","15(A)","18(A+)","21(A++)"))+
  ggtitle("第一次模擬考各班國文積點箱形圖")
  #facet_grid(.~class)
)
```

```{r 第一次模擬考各班英文積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = english, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$english, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=10,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("3(C)","6(B)","9(B+)","12(B++)","15(A)","18(A+)","21(A++)"))+
  ggtitle("第一次模擬考各班英文積點箱形圖")
)
```

```{r 第一次模擬考各班數學積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y =math, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$math, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=9,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("3(C)","6(B)","9(B+)","12(B++)","15(A)","18(A+)","21(A++)"))+
  ggtitle("第一次模擬考各班數學積點箱形圖")
)
```

```{r 第一次模擬考各班自然積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = nature, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$nature, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=9,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("3(C)","6(B)","9(B+)","12(B++)","15(A)","18(A+)","21(A++)"))+
  ggtitle("第一次模擬考各班自然積點箱形圖")
)
```

```{r 第一次模擬考各班社會積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = society, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$society, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=10,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(3, 21,3), labels = c("3(C)","6(B)","9(B+)","12(B++)","15(A)","18(A+)","21(A++)"))+
  ggtitle("第一次模擬考各班社會積點箱形圖")
)
```

```{r 第一次模擬考各班作文積點箱形圖, echo=FALSE}

ggplotly(
  ggplot(a01) +
  aes(x = class, y = writing, fill = class) +
  geom_boxplot(outlier.size = 1.5, outlier.shape = 21, na.rm = TRUE) +
  stat_summary(fun="mean", geom = "point",shape=23, size=2, fill="white", na.rm = TRUE)+
  geom_hline(yintercept = mean(a01$writing, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=3.5,label="全校平均值",color="red")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  #guides(fill=guide_legend(title = "班級"))+
  xlab("班級(班)") + 
  ylab("總積點\n(點)") + 
  scale_fill_discrete(guide=FALSE) +
  scale_y_continuous(breaks = seq(0, 6, 1), labels = c(0,1,2,3,4,5,6))+
  ggtitle("第一次模擬考各班作文積點箱形圖")
)
```

###################################################################################
```{r 第一次模擬考全校總積點人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=points))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$points, na.rm = TRUE),col="red")+
  annotate("text",x=49, y=21,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$points, na.rm = TRUE),col="blue")+
  annotate("text",x=40, y=23,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  theme(axis.text.y.left = element_blank()) +
  #xlab("總積點") + 
  #ylab("人數")+
  scale_x_continuous(breaks = seq(0, 120,20))+
  ggtitle("第一次模擬考全校總積點人數圖")
)
```

```{r 第一次模擬考全校總積分人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=scores))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$scores, na.rm = TRUE),col="red")+
  annotate("text",x=20, y=100,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$scores, na.rm = TRUE),col="blue")+
  annotate("text",x=20, y=90,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+  
  theme(axis.text.y.left = element_blank()) +
  #xlab("總積分") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 100,20))+
  ggtitle("第一次模擬考全校總積分人數圖") 
  #facet_grid(class ~.)
)
```

```{r 第一次模擬考全校國文積點人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=chinese))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$chinese, na.rm = TRUE),col="red")+
  annotate("text",x=9.3, y=130,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$chinese, na.rm = TRUE),col="blue")+
  annotate("text",x=9, y=150,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))+
  #xlab("國文總積點") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 200,20))+
  ggtitle("第一次模擬考全校國文積點人數圖") 
)
```

```{r 第一次模擬考全校英文積點人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=english))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$english, na.rm = TRUE),col="red")+
  annotate("text",x=9, y=130,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$english, na.rm = TRUE),col="blue")+
  annotate("text",x=6, y=150,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+ 
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))+
  #xlab("英文總積點") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 200,20))+
  ggtitle("第一次模擬考全校英文積點人數圖") 
)
```

```{r 第一次模擬考全校數學積點人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=math))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$math, na.rm = TRUE),col="red")+
  annotate("text",x=8, y=130,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$math, na.rm = TRUE),col="blue")+
  annotate("text",x=6, y=150,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+ 
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))+
  #xlab("數學總積點") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 200,20))+
  ggtitle("第一次模擬考全校數學積點人數圖") 
)
```

```{r 第一次模擬考全校自然積點人數圖, echo=FALSE}

ggplotly(
  ggplot(a01, aes(x=nature))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$nature, na.rm = TRUE),col="red")+
  annotate("text",x=8.5, y=130,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$nature, na.rm = TRUE),col="blue")+
  annotate("text",x=6, y=150,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+ 
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))+
  #xlab("自然總積點") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 200,20))+
  ggtitle("第一次模擬考全校自然積點人數圖") 
)
```

```{r 第一次模擬考全校社會積點人數圖, echo=FALSE}
ggplotly(
  ggplot(a01, aes(x=society))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE)+
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$society, na.rm = TRUE),col="red")+
  annotate("text",x=10, y=130,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$society, na.rm = TRUE),col="blue")+
  annotate("text",x=9, y=150,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+ 
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(3, 21,3), labels = c("C","B","B+","B++","A","A+","A++"))+
  #xlab("社會總積點") + 
  #ylab("人數")+
  #scale_y_continuous(breaks = seq(0, 200,20))+
  ggtitle("第一次模擬考全校社會積點人數圖") 
)
```

```{r 第一次模擬考全校作文積點人數圖, echo=FALSE}
ggplotly(
  ggplot(a01, aes(x=writing))+ 
  geom_bar(fill="white", color="black", na.rm = TRUE) +
  geom_text(stat = "count", aes(label = (..count..)), vjust = -0.5) +
  geom_vline(xintercept = mean(a01$writing, na.rm = TRUE),col="red")+
  annotate("text",x=3, y=240,label="平均數",color="red")+
  geom_vline(xintercept = median(a01$writing, na.rm = TRUE),col="blue")+
  annotate("text",x=3, y=250,label="中位數",color="blue")+
  theme_minimal()+
  theme(panel.grid = element_line(color="NA"))+
  #theme(axis.line = element_line(color="black"))+
  #theme(axis.title.y = element_text(angle=0))+
  theme(axis.title.y.left = element_blank())+
  theme(axis.title.x.bottom = element_blank())+ 
  theme(axis.text.y.left = element_blank()) +
  scale_x_continuous(breaks = seq(0, 6, 1), labels = c(0,1,2,3,4,5,6))+
  #scale_y_continuous(breaks = seq(0, 300, 50))+
  #xlab("作文總積點") + 
  #ylab("人數")+
  ggtitle("第一次模擬考全校作文積點人數圖")
)
```

```{r 第一次模擬考各班5C-5A人code, include=FALSE}
a01.5a<- a01 %>% filter(scores>=30)
a01.5c<- a01 %>% filter(scores<=10)
```

```{r 第一次模擬考各班5C人數圖, echo=FALSE}

#以符號標示, 5c
ggplotly(
  ggplot(a01.5c, aes(class, points)) +
  geom_point(aes(shape= gender, size= scores)) + 
  #geom_text(aes(label=name), size=3, hjust=1) +
  theme_minimal() +
  theme(panel.grid = element_line(color="NA")) +
  theme(axis.line = element_line(color="black")) +
  theme(axis.title.y.left = element_blank()) +
  theme(axis.title.x.bottom = element_blank()) + 
  #scale_x_continuous(breaks = seq(0, 6, 1), labels = c(0,1,2,3,4,5,6)) +
  #scale_y_continuous(breaks = seq(0, 300, 50)) +
  #xlab("作文總積點") + 
  #ylab("人數")+
  ggtitle("第一次模擬考各班5C人數圖") 
)
```

```{r 第一次模擬考各班5A人數圖, echo=FALSE}

#以姓名標示, 5a
ggplotly(
  ggplot(a01.5a, aes(class, points)) +
  geom_text(aes(label=name), size=4, hjust=0.5, vjust=-1) +
  geom_hline(yintercept = 95, col="red") +
  annotate("text",x=8, y=96,label="109學年度台中一中最低入取95積點",color="red") +
  theme_minimal() +
  theme(panel.grid = element_line(color="NA")) +
  theme(axis.line = element_line(color="black")) +
  theme(axis.title.y.left = element_blank()) +
  theme(axis.title.x.bottom = element_blank()) + 
  #scale_x_continuous(breaks = seq(0, 6, 1), labels = c(0,1,2,3,4,5,6)) +
  #scale_y_continuous(breaks = seq(0, 120, 20)) +
  #xlab("作文總積點") + 
  #ylab("人數")+
  ylim(80,110) +
  ggtitle("第一次模擬考各班5A人數圖")
)
```
###################################################################################


