

pacman::p_load(forecast,tidyverse,data.table)




df.ts <- df |> 
  mutate(year=as.character(year(date)),
         month=as.character(month(date)),
         day= as.character(day(date))) |> 
  group_by(date,year,month,day) |> 
  dplyr::summarise(n=n()) |> 
  mutate(month = str_pad(month,width = 2, side="left",pad = 0)) |> 
  dplyr::mutate(date_char = paste0("01/",month,"/",year)) |> 
  dplyr::group_by(date_char) |> 
  dplyr::summarise(n = sum(n)) |> 
  mutate(date = as.Date(date_char,  format = "%d/%m/%Y"),
         month = month(date),
         year = year(date)) |> 
  select(date_char,date,year,month,n)



df18 <- df.ts |> filter(year == 2018)
df19 <- df.ts |> filter(year == 2019)
df20 <- df.ts |> filter(year == 2020)
df21 <- df.ts |> filter(year == 2021)

df.ts <- rbind(df18,df19,df20,df21)

df_ts <- ts(data = df.ts |> select(n),
            start = c(2018,1),
            end = c(2021,12),
            frequency = 12)


ggseasonplot(df_ts)


ggseasonplot(df_ts, polar = TRUE)



train <- window(df_ts,start=2018, end=2021)
test <- window(df_ts,start=2021)


naive_model = naive(train,h=12)

summary(naive_model)

autoplot(naive_model)+
  autolayer(test,series = "test")

checkresiduals(naive_model)



holt_model=holt(train,h=12,damped = TRUE)

summary(holt_model)
autoplot(holt_model)

autoplot(holt_model)+
  autolayer(test,series = "test")

checkresiduals(holt_model)


hw_model=hw(train,h=25)
autoplot(hw_model)

autoplot(hw_model)+
  autolayer(test,series = "test")

summary(hw_model)
checkresiduals(hw_model)





df_a=as.data.frame(accuracy(holt_model))


df_a = df_a |> mutate(model = "holt_model")

df_b=as.data.frame(accuracy(hw_model))
df_b <- df_b |> mutate(model = "hw")

df_c=as.data.frame(accuracy(naive_model))

df_c=df_c |> mutate(model = "naive")

measures = rbind(df_a,df_b,df_c)

p1 <- measures |> 
  ggplot(aes(x = MAPE,
             y = model))+
  geom_col(aes(color = model, fill = model), alpha = c(0.3))+
  theme_minimal()+labs(title ="MAPE",x="",y="")+
  theme(legend.position = "none")


p2 <- measures |> 
  ggplot(aes(x = MASE,
             y = model))+
  geom_col(aes(color = model, fill = model), alpha = c(0.3))+
  theme_minimal()+labs(title ="MASE",x="",y="")+
  theme(legend.position = "none")



p3 <- measures |> 
  ggplot(aes(x = RMSE,
             y = model))+
  geom_col(aes(color = model, fill = model), alpha = c(0.3))+
  theme_minimal()+labs(title ="RMSE",x="",y="")+
  theme(legend.position = "none")



p4 <- measures |> 
  ggplot(aes(x = MAE,
             y = model))+
  geom_col(aes(color = model, fill = model), alpha = c(0.3))+
  theme_minimal()+labs(title ="MAE",x="",y="")+
  theme(legend.position = "none")


gridExtra::grid.arrange(p1,p2,p3,p4)




checkresiduals(hw_model)

rm(predictions)
predictions <- as.data.frame(hw_model);predictions



predictions <- predictions |> mutate(date=row.names(predictions))


predictions <- predictions |> filter(grepl("2022",date)) |> select(1) |> as_tibble();predictions

df.ts

predictions <- predictions |> 
  mutate(date = c("01/01/2022",
                  "01/02/2022",
                  "01/03/2022",
                  "01/04/2022",
                  "01/05/2022",
                  "01/06/2022",
                  "01/07/2022",
                  "01/08/2022",
                  "01/09/2022",
                  "01/10/2022",
                  "01/11/2022",
                  "01/12/2022")) |> 
  select(date,1) |> 
  dplyr::rename(forecast=`Point Forecast`) |> 
  as_tibble()


df.ts2 <- rbind(df.ts |> 
                  select(date,n) |> 
                  dplyr::rename(forecast = n),
                predictions |> mutate(date = as.Date(date, format = "%d/%m/%Y"))) |> 
  mutate(type = ifelse(year(date) == "2022","forecast","real"))


df.ts2 <- df.ts2 |> mutate(forecast=round(forecast))



write.csv2(df.ts2,"data/processed/forecast.csv",
           row.names=FALSE)
