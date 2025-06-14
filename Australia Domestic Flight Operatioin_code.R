
library(readxl)
library("readxl")
dataset20.23=read_excel("DatasetAssignment2.xlsx")
datasaet2019=read_excel("DatasetAssignment2.xlsx", sheet=3)
datasaet2018=read_excel("DatasetAssignment2.xlsx", sheet=4)
datasaet2017=read_excel("DatasetAssignment2.xlsx", sheet=5)
datasaet2016=read_excel("DatasetAssignment2.xlsx", sheet=6)
datasaet2015=read_excel("DatasetAssignment2.xlsx", sheet=7)
datasaet2014=read_excel("DatasetAssignment2.xlsx", sheet=8)
datasaet2013=read_excel("DatasetAssignment2.xlsx", sheet=9)
datasaet2012=read_excel("DatasetAssignment2.xlsx", sheet=10)
datasaet2011=read_excel("DatasetAssignment2.xlsx", sheet=11)
datasaet2010=read_excel("DatasetAssignment2.xlsx", sheet=12)


combine.data=rbind(datasaet2010, datasaet2011, datasaet2012, datasaet2013, datasaet2014, datasaet2015, datasaet2016, datasaet2017, datasaet2018, datasaet2019, dataset20.23)
colnames(combine.data)[colnames(combine.data)=="Month"]="Date"
combine.data$Date=as.Date(paste0(combine.data$Date, "-01"), format = "%Y-%m-%d")
class(combine.data$Date)
combine.data$Year=format(combine.data$Date, "%Y")
combine.data$Month=format(combine.data$Date, "%B")
combine.data$Day=format(combine.data$Date, "%d")
write.csv(combine.data, "assignment2.clean.data.csv", row.names=FALSE)

colnames(combine.data)
colnames(combine.data)[colnames(combine.data)=="Sectors Scheduled"]="Sector.Scheduled"
colnames(combine.data)[colnames(combine.data)=="Departing Port"]="Departing.Port"
colnames(combine.data)[colnames(combine.data)=="Arriving Port"]="Arriving.Port"
colnames(combine.data)[colnames(combine.data)=="Sectors Flown"]="Sectors.Flown"
colnames(combine.data)[colnames(combine.data)=="Arrivals On Time"]="Arrivals.On.Time"
colnames(combine.data)[colnames(combine.data)=="Departures On Time"]="Departures.On.Time"
colnames(combine.data)[colnames(combine.data)=="Departures Delayed"]="Departures.Delayed"
colnames(combine.data)[colnames(combine.data)=="Arrivals Delayed"]="Arrivals.Delayed"
colnames(combine.data)[colnames(combine.data)=="OnTime Departures \r\n(%)"]="OnTime.Departures"
colnames(combine.data)[colnames(combine.data)=="OnTime Arrivals \r\n(%)"]="OnTime.Arrivals"
colnames(combine.data)[colnames(combine.data)=="Cancellations \r\n\r\n(%)"]="Cancellatioins"
airline.df=as.data.frame(table(combine.data$Airline))
colnames(airline.df)[colnames(airline.df)=="Var1"]="Airline"
combine.data$Airline[combine.data$Airline=="virgin Australia"]="Virgin Australia"
table(combine.data$Airline)
combine.data$Year <- year(combine.data$Date)
combine.data$Month <- month(combine.data$Date, label = TRUE, abbr = TRUE)

library(tidyverse)
library(ggplot2)
library(lubridate)


year=combine.data%>%
  select(Year)

year.df=as.data.frame(table(year))

monthly.trend=combine.data%>%
  summarise(Scheduled=sum(Sector.Scheduled, na.rm=TRUE),
            Flown=sum(Sectors.Flown, na.rm=TRUE))

ggplot(monthly.trend, aes(x = interaction(Year, Month, sep = "-"))) +
  geom_line(aes(y = Scheduled, group = 1, color = "Scheduled")) +
  geom_line(aes(y = Flown, group = 1, color = "Flown")) +
  labs(title = "Monthly Trend of Scheduled vs Flown Flights",
       x = "Year-Month", y = "Number of Flights", color = "Legend") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



plot(Freq ~ Year, data=year.df, 
     type="l")
ggplot(data=year.df)+
  geom_line(mapping=aes(x=Year))

### top routes by the number of sector flow
top.routes=combine.data%>%
  filter(Route != "All Ports-All Ports")%>%
  group_by(Route)%>%
  summarise(total.sector.flow=sum(Sectors.Flown, na.rm=TRUE))%>%
  arrange(desc(total.sector.flow))%>%
  slice_head(n=10)

ggplot(data=top.routes, aes(x=reorder(Route,total.sector.flow), y=total.sector.flow))+
  geom_col(fill="light blue", col="dark blue")+
  labs(title="Top 10 routes by sector flown", x="Route", y="Total Sector Flown", size=14)+
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),   # <-- Increase y-axis label font size (airlines)
    axis.text.x = element_text(size = 12),   # <-- Increase x-axis label font size (numbers)
    plot.title = element_text(size = 16, face = "bold")  # Optional: make title larger and bold
  )

### top 10 airlines by number of sector flown
table(combine.data$Airline)
top.airline=combine.data%>%
  filter(Airline !="All Airlines")%>%
  group_by(Airline)%>%
  summarise(total.sector.flow=sum(Sectors.Flown, na.rm = TRUE))%>%
  arrange(desc(total.sector.flow))%>%
  slice_head(n=10)

ggplot(data=top.airline, aes(x=reorder(Airline, total.sector.flow), y=total.sector.flow), size=10)+
  geom_col(col="darkgreen",fill="light green",)+
  labs(title="Top 10 Airlines by Sector Flown", x="Airline",y="Total Sector Flown")+
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),   
    axis.text.x = element_text(size = 10),   
    plot.title = element_text(size = 16, face = "bold")  
  )

            
### plot airlines during the pandemic
pandemic=combine.data%>%
  filter(YearMonth %in% c(2019, 2020,2021))
annual.flight.pandemic=pandemic%>%
  group_by(Year)%>%
  summarise(total.sector.flown=sum(Sectors.Flown, na.rm=TRUE))

ggplot(data=annual.flight.pandemic, aes(x=factor(Year), y=total.sector.flown, fill=factor(Year)))+
  geom_col(show.legend = TRUE)+
  labs(title="Effect of COVID-19 on Sector Flown", 
       x="Year", y="Total Sector Flown",
       fill="Year")+
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

## top airlines based on on-time departure
data.new = combine.data %>%
  filter(Airline != "All Airlines", Route != "All Ports-All Ports")

# 10 On-Time Performance by Airline


on.time.airline=data.new%>%
  group_by(Airline)%>%
  summarise(avg.ontime.dept=mean(OnTime.Departures, na.rm=TRUE),
avg.ontime.arr=mean(OnTime.Arrivals, na.rm=TRUE))%>%
  arrange(desc(avg.ontime.dept))

on.time.route=data.new%>%
  group_by(Route)%>%
  summarise(avg.ontime.dept=mean(OnTime.Departures, na.rm=TRUE),
            avg.ontime.arr=mean(OnTime.Arrivals, na.rm=TRUE))%>%
  arrange(desc(avg.ontime.dept))

top10.airline=on.time.airline%>%slice_max(avg.ontime.dept, n=10)
ggplot(data=top10.airline, aes(x=reorder(Airline, avg.ontime.dept), y=avg.ontime.dept))+
  geom_col(fill="magenta")+
  labs(title = "Top 10 Airlines by Avg On-Time Departure Rate",
       x = "Airline", y = "Avg On-Time Departure (%)")+
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10),  
    plot.title = element_text(size = 16, face = "bold") 
  )


top10.route=on.time.route%>%slice_max(avg.ontime.arr, n=10)

ggplot(data=top10.route, aes(x=reorder(Route, avg.ontime.arr), y=avg.ontime.arr))+
  geom_col(fill="deeppink")+
  labs(title = "Top 10 Route by Avg On-Time Arrival Rate",
       x = "Route", y = "Avg On-Time Arrival (%)")+
  coord_flip()+
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),  
    axis.text.x = element_text(size = 10),  
    plot.title = element_text(size = 16, face = "bold") 
  )


# Create heatmap


heatmap.data=combine.data%>%
  group_by(Airline, Month, Year)%>%
  summarise(avg.ontime.dept = mean(OnTime.Departures, na.rm=TRUE))%>%
  ungroup()

ggplot(data=heatmap.data, aes(x= Month, y=reorder(Airline, desc(Airline)), fill=avg.ontime.dept))+
  geom_tile(color="black")+
  scale_fill_gradient(low="lightyellow", high="darkgreen", na.value = "grey90")+
  labs(title = "Average On-Time Departure Rate by Airline and Month",
       x = "Month", y = "Airline", fill = "On-Time %") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))

## Dashboard


top3.perf=combine.data%>%
  filter(Airline %in% c("Virgin Australia", "Qantas", "QantasLink"))

sum.metrix=top3.perf%>%
  group_by(Airline)%>%
  summarise(avg.sector.flown=mean(Sectors.Flown, na.rm=TRUE),
            avg.ontime.dept=mean(OnTime.Departures, na.rm=TRUE),
            avg.cancel=mean(Cancellations, na.rm=TRUE))%>%
  pivot_longer(cols=-Airline, names_to = "Metrix", values_to = "Value")

sum.metrix$Metrix=recode(sum.metrix$Metrix,
                         avg)  
  

ggplot(data=sum.metrix, aes(x=Metrix, y=Value, fill=Airline))+
  geom_col(position="dodge")+
  labs(title = "Airline Performance Benchmarking",
       x = "Metric", y = "Average Value", fill = "Airline") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11, angle = 20),
        plot.title = element_text(size = 14, face = "bold"))

# Radra plot
library(fmsb)

radar.df=top3.perf%>%
  group_by(Airline)%>%
  summarise("Avarage Sector flown"=mean(Sectors.Flown, na.rm=TRUE),
           "Avarage On Time Depatures"=mean(OnTime.Departures, na.rm=TRUE),
            "Avarage Cancellations"=mean(Cancellations, na.rm=TRUE))

max.val=apply(radar.df[,-1],2,max)*1.1
min.val=rep(0, length(max.val))
radar.data=rbind(max.val, min.val, radar.df[,-1])
rownames(radar.data)=c("Max", "Min", radar.df$Airline)

# Plot
radarchart(radar.data,
           pcol = c("darkgreen", "blue", "purple"),
           pfcol = c("lightblue", "#FFDC91AA", "#E7B8D1AA"),
           plwd = 2,
           title = "Airline Performance Radar Chart",
           cglcol = "grey", cglty = 1, axislabcol = "black")
legend("topright", legend = radar.df$Airline, fill = c("lightblue", "#FFDC91AA", "#E7B8D1AA"))

# Hypothesis testing between on-time airline performance
va= combine.data%>%
  filter(Airline=="Virgin Australia") %>%
  pull(OnTime.Departures)%>%
  na.omit()

qs=combine.data%>%
  filter(Airline=="Qantas")%>%
  pull(OnTime.Departures)%>%
  na.omit()

t.test(va, qs, alternative="two.sided") 
# p value is small, so there is a significant different on the on-time arrival between VA and Qantas


## Predicting
combine.data$YearMonth=floor_date(combine.data$Date, "month")
flight.trend=combine.data%>%
  filter(year(Date)>=2019)%>%
  group_by(YearMonth)%>%
  summarise(Sectors.Flown=sum(Sectors.Flown, na.rm=TRUE))

model=lm(Sectors.Flown~as.numeric(YearMonth), data=flight.trend)
future=data.frame(YearMonth=seq(as.Date("2024-01-01"), as.Date("2024-12-01"),by="month"))
future$Sector.Flown.Predicted=predict(model, newdata = future)

ggplot(flight.trend, aes(x=YearMonth, y=Sectors.Flown))+
  geom_line(color = "blue", size=1) +
  geom_line(data = future, aes(x = YearMonth, y = Sector.Flown.Predicted), color = "red", linetype = "dashed", size=1) +
  labs(title = "Forecast of Monthly Sectors Flown",
       subtitle = "Blue = Actual, Red Dashed = Forecast",
       x = "Month", y = "Sectors Flown") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))


ggplot(flight.trend, aes(x=YearMonth, y=Sectors.Flown))+
  geom_line(color = "goldenrod", size=1)+
  labs(title="Sector Flown per year", x="Year", y="Total Sector Flown")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold"))
