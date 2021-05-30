#S&P 500 Dataset
sp500_data = read.csv('S&P 500 Dataset.csv')
sp500_data = sp500_data[-1]
sp500_data$Date = as.Date(sp500_data$Date, '%Y-%m-%d')
sp500_data$Date = yearmonth(sp500_data$Date)
sp500_data = as_tsibble(sp500_data)

#Plots
sp500_data %>%
  ggplot(aes(x = Apple.Inc., y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

sp500_data %>%
  ggplot(aes(x = Microsoft, y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

sp500_data %>%
  ggplot(aes(x = Amazon, y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

sp500_data %>%
  ggplot(aes(x = Google, y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

sp500_data %>%
  ggplot(aes(x = Tesla, y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

sp500_data %>%
  ggplot(aes(x = VIX, y = S.P.500)) + geom_point() + geom_smooth(method = 'lm', se = F)

#Time series
sp500_data %>%
  autoplot(S.P.500)

sp500_data %>%
  autoplot(Microsoft)

sp500_data %>%
  autoplot(Amazon)

sp500_data %>%
  autoplot(Google)

sp500_data %>%
  autoplot(Tesla)

sp500_data %>%
  autoplot(Apple.Inc.)

sp500_data %>%
  autoplot(VIX)

#seasonality and trend detection
sp500_data %>%
  model(STL(S.P.500 ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(Microsoft ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(Apple.Inc. ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(Amazon ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(Google ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(Tesla ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

sp500_data %>%
  model(STL(VIX ~ trend() + season(window = 'periodic'))) %>%
  components() %>%
  autoplot()

#Models
sp500_data_reg = sp500_data %>%
  model(tslm = TSLM(S.P.500 ~ Microsoft + Apple.Inc. + Amazon + Google + Tesla + VIX),
        tslm1 = TSLM(S.P.500 ~ Apple.Inc. + Google + Tesla + VIX),
        tslm2 = TSLM(S.P.500 ~ Microsoft + Apple.Inc. + Google + Tesla + VIX),
        tslm3 = TSLM(S.P.500 ~ Apple.Inc. + Amazon + Google + Tesla + VIX))

glance(sp500_data_reg)

sp500_data_reg %>%
  select(tslm1) %>%
  report()

sp500_data_reg %>%
  select(tslm) %>%
  report()

##Residual diagnostics
sp500_data_reg %>%
  select(tslm1) %>%
  gg_tsresiduals()


#Autocorrelation detection
sp500_data %>%
  ACF(S.P.500) %>%
  autoplot()  # Autocorrelation detected  for S&P 500

sp500_data %>%
  ACF(Apple.Inc.) %>%
  autoplot()  # Autocorrelation detected for Apple

sp500_data %>%
  ACF(Google) %>%
  autoplot()  # Autocorrelation detected for Google

sp500_data %>%
  ACF(Tesla) %>%
  autoplot()  # Autocorrelation detected for Tesla

sp500_data %>%
  ACF(VIX) %>%
  autoplot()  # Autocorrelation detected for VIX

##ARIMA model
#Differencing of S&P 500
sp500_data %>%
  gg_tsdisplay(difference(S.P.500,12),
               plot_type='partial',lag = 36) 

sp500_data %>%
  gg_tsdisplay(difference(S.P.500 , 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

#Differencing of Apple
sp500_data %>%
  gg_tsdisplay(difference(Apple.Inc. , 12),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

sp500_data %>%
  gg_tsdisplay(difference(Apple.Inc. , 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")  

#Differencing of Google
sp500_data %>%
  gg_tsdisplay(difference(Google, 12),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

sp500_data %>%
  gg_tsdisplay(difference(Google, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

#Differencing of Tesla
sp500_data %>%
  gg_tsdisplay(difference(Tesla, 12),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

sp500_data %>%
  gg_tsdisplay(difference(Tesla, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

#Differencing of VIX
sp500_data %>%
  gg_tsdisplay(difference(VIX, 12),
               plot_type='partial', lag=36) +
  labs(title="2x Difference", y="")

sp500_arima = sp500_data %>%
  model(ARIMA(S.P.500 ~ Apple.Inc. + Google + Tesla + VIX + 0 + pdq(0,2,5) + PDQ(0,2,1)))
report(sp500_arima)


sp500_arima %>%
  gg_tsresiduals() +
  labs(title = 'Residual Diagnostics')

##Forecasts of predictor variables

#Apple
sp500_apple_future = sp500_data %>%
  model(ARIMA(Apple.Inc.)) %>%
  forecast(h = 60) 

confidence = data.frame(matrix(nrow = nrow(sp500_apple_future), ncol = 4))
colnames(confidence) = c('Lower Bound 80', 'Upper Bound 80', 'Lower Bound 95', 'Upper Bound 95')
sp500_apple_future = cbind(sp500_apple_future, confidence)

for(i in 1:nrow(sp500_apple_future)){
  sp500_apple_future[i, 'Lower Bound 80'] = sp500_apple_future[[3]][[i]][['mu']] -  1.28*sp500_apple_future[[3]][[i]][['sigma']]
  sp500_apple_future[i, 'Upper Bound 80'] = sp500_apple_future[[3]][[i]][['mu']] +  1.28*sp500_apple_future[[3]][[i]][['sigma']]
  sp500_apple_future[i, 'Lower Bound 95'] = sp500_apple_future[[3]][[i]][['mu']] -  1.96*sp500_apple_future[[3]][[i]][['sigma']]
  sp500_apple_future[i, 'Upper Bound 95'] = sp500_apple_future[[3]][[i]][['mu']] +  1.96*sp500_apple_future[[3]][[i]][['sigma']]
}

appleForecast = subset(sp500_apple_future, select = -c(.model, Apple.Inc.))
colnames(appleForecast)[2] = 'Forecasted Apple.Inc.'

#Google
sp500_google_future = sp500_data %>%
  model(ARIMA(Google)) %>%
  forecast(h = 60) 


sp500_google_future = cbind(sp500_google_future, confidence)

for(i in 1:nrow(sp500_google_future)){
  sp500_google_future[i, 'Lower Bound 80'] = sp500_google_future[[3]][[i]][['mu']] -  1.28*sp500_google_future[[3]][[i]][['sigma']]
  sp500_google_future[i, 'Upper Bound 80'] = sp500_google_future[[3]][[i]][['mu']] +  1.28*sp500_google_future[[3]][[i]][['sigma']]
  sp500_google_future[i, 'Lower Bound 95'] = sp500_google_future[[3]][[i]][['mu']] -  1.96*sp500_google_future[[3]][[i]][['sigma']]
  sp500_google_future[i, 'Upper Bound 95'] = sp500_google_future[[3]][[i]][['mu']] +  1.96*sp500_google_future[[3]][[i]][['sigma']]
}

googleForecast = subset(sp500_google_future, select = -c(.model, Google))
colnames(googleForecast)[2] = 'Forecasted Google'

#Tesla
sp500_tesla_future = sp500_data %>%
  model(ARIMA(Tesla)) %>%
  forecast(h = 60) 

sp500_tesla_future = cbind(sp500_tesla_future, confidence)

for(i in 1:nrow(sp500_tesla_future)){
  sp500_tesla_future[i, 'Lower Bound 80'] = sp500_tesla_future[[3]][[i]][['mu']] -  1.28*sp500_tesla_future[[3]][[i]][['sigma']]
  sp500_tesla_future[i, 'Upper Bound 80'] = sp500_tesla_future[[3]][[i]][['mu']] +  1.28*sp500_tesla_future[[3]][[i]][['sigma']]
  sp500_tesla_future[i, 'Lower Bound 95'] = sp500_tesla_future[[3]][[i]][['mu']] -  1.96*sp500_tesla_future[[3]][[i]][['sigma']]
  sp500_tesla_future[i, 'Upper Bound 95'] = sp500_tesla_future[[3]][[i]][['mu']] +  1.96*sp500_tesla_future[[3]][[i]][['sigma']]
}

teslaForecast = subset(sp500_tesla_future, select = -c(.model, Tesla))
colnames(teslaForecast)[2] = 'Forecasted Tesla'

#VIX
sp500_vix_future = sp500_data %>%
  model(ARIMA(VIX)) %>%
  forecast(h = 60) 


sp500_vix_future = cbind(sp500_vix_future, confidence)


for(i in 1:nrow(sp500_vix_future)){
  sp500_vix_future[i, 'Lower Bound 80'] = sp500_vix_future[[3]][[i]][['mu']] -  1.28*sp500_vix_future[[3]][[i]][['sigma']]
  sp500_vix_future[i, 'Upper Bound 80'] = sp500_vix_future[[3]][[i]][['mu']] +  1.28*sp500_vix_future[[3]][[i]][['sigma']]
  sp500_vix_future[i, 'Lower Bound 95'] = sp500_vix_future[[3]][[i]][['mu']] -  1.96*sp500_vix_future[[3]][[i]][['sigma']]
  sp500_vix_future[i, 'Upper Bound 95'] = sp500_vix_future[[3]][[i]][['mu']] +  1.96*sp500_vix_future[[3]][[i]][['sigma']]
}

vixForecast = subset(sp500_vix_future, select = -c(.model, VIX))
colnames(vixForecast)[2] = 'Forecasted VIX'


#Final forecast

sp500_future = new_data(sp500_data, 60) %>%
  mutate(Apple.Inc. = mean(sp500_apple_future$Apple.Inc.),
         Google = mean(sp500_google_future$Google),
         Tesla = mean(sp500_tesla_future$Tesla),
         VIX = mean(sp500_vix_future$VIX))

forecast(sp500_arima, new_data = sp500_future) %>%
  autoplot(sp500_data) +
  labs(title = 'Forecasted S&P 500')

sp500_future_data = forecast(sp500_arima, new_data = sp500_future)
colnames(sp500_future_data)[4] = 'Forecasted S.P.500'
sp500_future_data = cbind(sp500_future_data, confidence)

for(i in 1:nrow(sp500_future_data)){
  sp500_future_data[i, 'Lower Bound 80'] = sp500_future_data[[3]][[i]][['mu']] -  1.28*sp500_future_data[[3]][[i]][['sigma']]
  sp500_future_data[i, 'Upper Bound 80'] = sp500_future_data[[3]][[i]][['mu']] +  1.28*sp500_future_data[[3]][[i]][['sigma']]
  sp500_future_data[i, 'Lower Bound 95'] = sp500_future_data[[3]][[i]][['mu']] -  1.96*sp500_future_data[[3]][[i]][['sigma']]
  sp500_future_data[i, 'Upper Bound 95'] = sp500_future_data[[3]][[i]][['mu']] +  1.96*sp500_future_data[[3]][[i]][['sigma']]
}

sp500forecast = subset(sp500_future_data, select = -c(.model, Apple.Inc., S.P.500, Google, Tesla, VIX))




