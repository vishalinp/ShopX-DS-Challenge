library(tidyverse)
library(lubridate)

sales <- read.csv('historic_sales_data.csv', header=T, sep=',')
items <- read.csv('items.csv', header=T, sep=',')

sales <- sales %>%
  left_join(items, by='item_id')

sales <- sales %>%
  mutate(date_key = paste0(year, sprintf('%02d', month), sprintf('%02d', day))) %>%
  mutate(date = as_date(date_key)) %>%
  mutate(date_month = floor_date(ymd(date), 'month'))

sales <- sales %>%
  mutate(item_cnt_day = abs(item_cnt_day))


#----------------------------DATA EXPLORATION-----------------------------------
#Wrangling

shops <- sales %>%
  group_by(shop_id) %>%
  summarise(sales_volume = sum(item_cnt_day),
            sales_value = sum(item_cnt_day * item_price),
            average_item_price = sum(item_cnt_day * item_price)/sum(item_cnt_day),
            active_days = n_distinct(date_key),
            active_months = n_distinct(date_month),
            daily_volume = sum(item_cnt_day)/n_distinct(date_key),
            daily_value = sum(item_cnt_day * item_price)/n_distinct(date_key)) %>%
  ungroup()

shops <- sales %>%
  group_by(shop_id) %>%
  summarise() %>%
  ungroup()

shops_monthly <- sales %>%
  group_by(shop_id, date_month) %>%
  summarise(sales_volume = sum(item_cnt_day),
            sales_value = sum(item_cnt_day * item_price),
            average_item_price = sum(item_cnt_day * item_price)/sum(item_cnt_day),
            active_days = n_distinct(date_key),
            daily_volume = sum(item_cnt_day)/n_distinct(date_key),
            daily_value = sum(item_cnt_day * item_price)/n_distinct(date_key)) %>%
  ungroup()


items <- sales %>%
  group_by(item_category_id, item_id) %>%
  summarise(avg_price = mean(item_price)) %>%
  ungroup() %>%
  group_by(item_category_id) %>%
  summarise(avg_cat_price = mean(avg_price)) %>%
  mutate(category_price_class = case_when(avg_cat_price > 4000 ~ 'v.high',
                                          avg_cat_price >= 1000 ~ 'high',
                                          T ~ 'normal')) %>%
  ungroup()


shop_item_obs <- sales %>%
  group_by(shop_id, item_id) %>%
  summarise(obs = n_distinct(date_month))


#Visualization

ggplot(shops_monthly) +
  geom_line(aes(date_month, average_item_price, group=shop_id, colour=as.character(shop_id) )) +
  coord_cartesian(ylim=c(0,1000))

#monthly obs
ggplot(shop_item_obs) +
  geom_histogram(aes(obs))

ggplot(sales) +
  geom_histogram(aes(item_cnt_day), binwidth=1) +
  coord_cartesian(xlim=c(-2, 10), ylim=c(0,10000) )

ggplot(items) +
  geom_histogram(aes(avg_price))

#------------------------SALES FORECASTING--------------------------------------

library(forecast)
library(prophet)


#test

#find store-product with highest volume
shop_item <- sales %>%
  group_by(shop_id, item_id) %>%
  summarise(volume = n_distinct(date_month)) %>%
  arrange(desc(volume))

#shop = 62
#item = 1020949

sales_test <- sales %>%
  filter(item_id == 1005894, shop_id == 50) %>%
  group_by(date_month) %>%
  summarise(y = sum(item_cnt_day)) %>%
  arrange(date_month)

df <- data.frame(ds = seq(as.Date('2018-01-01'), as.Date('2020-05-01'), by='months'))

df <- df %>%
  left_join(sales_test, by=c('ds'='date_month')) %>%
  mutate(y = if_else(is.na(y), 0, y))

#df <- column_to_rownames(sales_test, var='date')

ggplot(df) +
  geom_line(aes(ds, y), colour='steelblue', size=1)

ggplot(df) +
  geom_histogram(aes(y ))

#box-cox transform
lam = BoxCox.lambda((df$y + 1), method='loglik')

df$y <- BoxCox((df$y + 1), lam)



m_p <- prophet(df)

future <- make_future_dataframe(m_p, periods=1, freq='month')

forecast <- predict(m_p, future)

plot(m_p, forecast)

inverse_forecast <- forecast

#inverse_forecast <- column_to_rownames(inverse_forecast, var='ds')

inverse_forecast <- inverse_forecast %>%
  mutate(yhat = InvBoxCox(yhat, lam),
         yhat_upper = InvBoxCox(yhat_upper, lam),
         yhat_lower = InvBoxCox(yhat_lower, lam))

plot(m_p, inverse_forecast)

ggplot(inverse_forecast) +
  geom_line(aes(as.Date(ds), yhat-1), colour='steelblue') +
  geom_point(data = sales_test, aes(date_month, y))


  geom_ribbon(aes(yhat_lower, yhat_upper), fill='steelblue', alpha=0.3)


  
#FORECAST ALL

#generate shop-item combinations
shop_item <- sales %>%
  group_by(shop_id, item_id) %>%
  summarise(volume = n_distinct(date_month)) %>%
  arrange(desc(volume))

#output dataframe
june_forecast <- data.frame(shop_id = c(),
                            item_id = c(),
                            predicted = c(),
                            pred_upper = c(),
                            pred_lower = c())
  
#iterate through shop-item combinations and forecast for each  
for(i in 1:nrow(shop_item))
{
  sid <- shop_item$shop_id[i]
  itm <- shop_item$item_id[i]
  
  series <- sales %>%
    filter(item_id == itm, shop_id == sid) %>%
    group_by(date_month) %>%
    summarise(y = sum(item_cnt_day)) %>%
    arrange(date_month)
  
  #create smooth dates
  df <- data.frame(ds = seq(as.Date('2018-01-01'), as.Date('2020-05-01'), by='months'))
  
  df <- df %>%
    left_join(series, by=c('ds'='date_month')) %>%
    mutate(y = if_else(is.na(y), 0, y))
  
  #check if we have more than 50% observations in last 12 months
  obs_12m <- series %>%
    filter(date_month > '2019-05-31') %>%
    nrow()
  
  #use prophet if we have enough obs
  if(obs_12m >= 6)
  {
    #apply reasonable box cox transform
    lam = BoxCox.lambda((df$y + 1), method='loglik')
    
    df$y <- BoxCox((df$y + 1), lam)
    
    
    #prophet
    m_p <- prophet(df)
    
    future <- make_future_dataframe(m_p, periods=1, freq='month')
    
    forecast <- predict(m_p, future)
    
    #convert forecast values back to pre-transform
    orig_forecast <- forecast
    
    orig_forecast <- orig_forecast %>%
      mutate(yhat = InvBoxCox(yhat, lam) - 1, 0,
             yhat_upper = InvBoxCox(yhat_upper, lam) - 1,
             yhat_lower = InvBoxCox(yhat_lower, lam) - 1) %>%
      mutate(yhat = if_else(yhat < 0, 0, yhat),
             yhat_upper = if_else(yhat_upper < 0, 0, yhat_upper),
             yhat_lower = if_else(yhat_lower < 0, 0, yhat_lower))
    
    
    pred <- data.frame(shop_id = sid,
                       item_id = itm,
                       predicted = orig_forecast$yhat[nrow(orig_forecast)],
                       pred_upper = orig_forecast$yhat_upper[nrow(orig_forecast)],
                       pred_lower = orig_forecast$yhat_lower[nrow(orig_forecast)])
  }
  
  #take average of June obs if data very sparse
  else
  {
    june_obs <- series %>%
      filter(month(date_month) == 6)
    
    june_pred <- ifelse(is.na(mean(june_obs$y)), 0, mean(june_obs$y))
    
    pred <- data.frame(shop_id = sid,
                       item_id = itm,
                       predicted = june_pred,
                       pred_upper = june_pred,
                       pred_lower = june_pred)
  }
  
  june_pred <- june_pred %>%
    rbind(pred)
}
 

ggplot(df) +
  geom_point(aes(ds, y)) +
  geom_line(data = orig_forecast, aes(as.Date(ds), yhat))
  
plot(m_p, forecast)
  
  
#--------------------------SHOP SEGMENTATION------------------------------------

library(mclust)
library(factoextra)

shops_clust <- shops %>%
  select(sales_volume, sales_value, active_days, daily_volume, daily_value, average_item_price) %>%
  mutate_all(scale)


#Dimension Reduction
pca <- prcomp(shops_clust, scale = F)

plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))

fviz_eig(pca)

fviz_pca_var(pca,
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T)

mod <- Mclust(shops_clust)


plot(mod, what = "classification")

plot(mod, what = 'BIC')

saveRDS(object = mod1, file = 'segm_model.rds')

adv_mclust <- adv_mclust %>%
  mutate(classification = mod1$classification,
         uncertainty = mod1$uncertainty)


adv_mclust_summary <- adv_mclust %>%
  group_by(classification) %>%
  summarise_all(list(mean, median, min, max))



#Additional feature engineering



smooth_dates <- data.frame(date_month = seq(as.Date('2018-01-01'), as.Date('2020-05-01'), by='months'))


#Fitting a linear model for each shop sale trend

shop_trend <- data.frame(shop_id = c(), trend = c())

for(i in 1:length(shops$shop_id))
{
  sid <- shops$shop_id[i]
  
  lm_test <- sales %>%
    filter(date_month >= as.Date('2019-06-01'), shop_id == sid) %>%
    group_by(date_month) %>%
    summarise(sales_volume = sum(item_cnt_day, na.rm=T)) %>%
    ungroup() %>%
    full_join(smooth_dates, by=c('date_month'='date_month')) %>%
    mutate(sales_volume = if_else(is.na(sales_volume), 0, sales_volume)) %>%
    mutate(sales_volume = sales_volume/mean(sales_volume)) %>%
    arrange(date_month) %>%
    mutate(var = row_number())
  
  if(nrow(filter(lm_test, !is.na(sales_volume)) ) < 2)
  {
    coef = 0
  }
  
  else
  {
    linmod <- lm(sales_volume ~ var, data=lm_test)
    coef = linmod$coefficients[2]
  }
  
  shop_trend <- shop_trend %>%
    rbind(data.frame(shop_id = c(sid), trend = coef))
}

  shops_12m <- shops_12m %>%
    mutate(days_since_active = difftime(as.Date('2020-05-31'), date, units='days'))


ggplot(filter(shops_monthly, shop_id == 26)) +
  geom_point(aes(date_month, sales_volume))
  
  
  
#12m stats

shops_12m <- sales %>%
  filter(date_month >= as.Date('2019-06-01')) %>%
  inner_join(items, by='item_category_id') %>%
  group_by(item_id, date_month) %>%
  mutate(median_price = median(item_price)) %>%
  ungroup() %>%
  mutate(pricing = case_when(item_price > median_price * 1.02 ~ 'premium',
                             item_price > median_price * 0.98 ~ 'normal',
                             T ~ 'discount')) %>%
  group_by(shop_id, date_month) %>%
  summarise(days_since_active = min(as.numeric(difftime(as.Date('2020-05-31'), date, units='days'))),
            active_days = n_distinct(date),
            volume_12m = sum(item_cnt_day),
            revenue_12m = sum(item_cnt_day * item_price),
            avg_item_price = sum(item_cnt_day * item_price)/sum(item_cnt_day),
            discount_volume = sum(if_else(pricing == 'discount', item_cnt_day, 0)/sum(item_cnt_day)),
            premium_volume = sum(if_else(pricing == 'premium', item_cnt_day, 0)/sum(item_cnt_day)),
            unique_items = n_distinct(item_id),
            unique_cat = n_distinct(item_category_id),
            unique_items_vhigh = n_distinct(if_else(category_price_class == 'v.high', item_id, -1)) - 1,
            unique_items_high = n_distinct(if_else(category_price_class == 'high', item_id, -1)) - 1,
            unique_items_normal = n_distinct(if_else(category_price_class == 'normal', item_id, -1)) - 1) %>%
  ungroup() %>%
  group_by(shop_id) %>%
  summarise(days_since_active = min(days_since_active),
            m_active_days = mean(active_days),
            m_volume = mean(volume_12m),
            m_revenue = mean(revenue_12m),
            m_avg_item_price = mean(avg_item_price),
            m_discount_volume = mean(discount_volume),
            m_premium_volume = mean(premium_volume),
            m_unique_items = mean(unique_items),
            m_unique_cat = mean(unique_cat),
            m_unique_items_vhigh = mean(unique_items_vhigh),
            m_unique_items_high = mean(unique_items_high),
            m_unique_items_normal = mean(unique_items_normal))


shops_12m <- shops_12m %>%
  left_join(shop_trend, by='shop_id')


#scale
shops_12m_clust <- shops_12m %>%
  select(-shop_id, -m_revenue, -m_unique_items_normal, -m_unique_items_high, -m_unique_items_vhigh, -m_unique_items, -m_premium_volume) %>%
  mutate_all(scale)

#PCA
pca <- prcomp(shops_12m_clust, scale = F)

plot(cumsum(pca$sdev^2/sum(pca$sdev^2)))

fviz_eig(pca)

fviz_pca_var(pca,
             col.var = 'contrib',
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


#choose first 6 PCA (explains ~95% var)
shops_12m_mclust <- pca$x %>%
  as.data.frame() %>%
  select(1:6)

#GMM
mod <- Mclust(shops_12m_clust)


plot(mod, what = "classification")

plot(mod, what = 'BIC')

shops_12m <- shops_12m %>%
  mutate(cluster = mod$classification)

shops_12m_summary <- shops_12m %>%
  group_by(cluster) %>%
  summarise_all(list(mean, median, min, max))

cluster_summary <- shop)_summary %>%
  group_by(cluster) %>%
  summarise_all(list(mean, median, min, max))

