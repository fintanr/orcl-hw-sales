library(ggplot2)
library(reshape2)

df <- read.csv("ORCL-HWSales-q4fy15.csv")

df$HW.Margin <- round(((df$HW.Sales - df$HW.Costs)/df$HW.Sales * 100),1)
df$HW.Support.Margin <- round(((df$HW.Support - df$HW.Support.Costs)/df$HW.Support * 100),1)
df$HW.Percent.Revenue <- round((df$HW.Sales/(df$HW.Sales + df$HW.Support) * 100),1)
df$HW.Support.Percent.Revenue <- round((df$HW.Support/(df$HW.Sales + df$HW.Support) * 100), 1)
df$HW.Percent.Total.Revenue <- round((((df$HW.Sales + df$HW.Support)/df$Total.Revenue) * 100),1)
df$Date <- as.Date(df$Date)

SalesComparisonDf <- melt(subset(df, 
                                 select = c(Date,HW.Sales,HW.Support)), 
                          id="Date")
colnames(SalesComparisonDf) <- c("Date", "Source", "value")

HWvSupportGrossMarginsDf <- melt(subset(df, 
                                        select = c(Date,HW.Margin,HW.Support.Margin)), 
                                 id="Date")
colnames(HWvSupportGrossMarginsDf) <- c("Date", "Source", "value")


g <- ggplot(data = SalesComparisonDf, aes(x=Date, y=value, color=Source)) 
g <- g + geom_line()
g <- g + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
g <- g + xlab("Year") + ylab("Revenue ($M)")
g <- g + ggtitle("Oracle Hardware and Hardware Support Sales\nQ1FY10 to Q4FY15")
# add in regression lines
g <- g + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

ggsave("ora-hw_v_support-sales-q4fy15.png", width=10)

g <- ggplot(data = HWvSupportGrossMarginsDf, aes(x=Date, y=value, color=Source)) 
g <- g + geom_line()
g <- g + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
g <- g + xlab("Year") + ylab("Margin (%)")
g <- g + ggtitle("Oracle Hardware v Hardware Support Margins\nQ1FY10 to Q4FY15")
g <- g + stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

ggsave("ora-hw_v_support-margins-q4fy15.png", width=10)

g <- ggplot(df, aes(Date, HW.Percent.Total.Revenue, group = 1)) + geom_line()
g <- g + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1))
g <- g + xlab("Year") + ylab("% Revenue")
g <- g + ggtitle("Oracle Hardware Sales as % of Total Revenue\nQ1FY10 to Q4FY15")

ggsave("ora-hw-sales-percent-revenue-q4fy15.png", width=10)

