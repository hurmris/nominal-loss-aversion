# Record function used in 06_flag_weirds_clean_panel.R


record_function <- function(filter_name, data = panel){
  record %>%
    add_row(n = nrow(record)+1,
            filter = paste0(filter_name),
            
            n_obs = nrow(data), #Rows
            d_obs = NA_real_,
            
            n_apartments = n_distinct(data %>% select(srnro,huonnro),na.rm=TRUE), #Apartmentos
            d_apartments = NA_real_,
            
            n_sales = nrow(data %>% select(srnro,huonnro,omypvm) %>% #Sales (new apartments excluded)
                             distinct() %>% 
                             filter(!is.na(omypvm))),
            d_sales = NA_real_,
          
            
            n_purchases = nrow(data %>% select(srnro,huonnro,ospvm,omypvm,vuosi) %>% #Purchase (new apartments included)
                                 distinct() %>% 
                                 filter(lubridate::year(ospvm)==vuosi & is.na(omypvm))),
            d_purchases = NA_real_) %>%
    mutate(d_obs = n_obs-lag(n_obs) ,
           d_apartments = n_apartments-lag(n_apartments),
           d_sales = n_sales-lag(n_sales),
           d_purchases = n_purchases-lag(n_purchases))
  
}


#data.table

record_function_dt <- function(filter_name, data = panel){
  record %>%
    add_row(n = nrow(record)+1,
            filter = paste0(filter_name),
            
            n_obs = nrow(data), #Rows
            d_obs = NA_real_,
            
            n_apartments = n_distinct(data %>% select(srnro,huonnro),na.rm=TRUE), #Apartmentos
            d_apartments = NA_real_,
            
            n_sales = nrow(data %>% select(srnro,huonnro,omypvm) %>% #Sales (new apartments excluded)
                             distinct() %>% 
                             filter(!is.na(omypvm))),
            d_sales = NA_real_,
            
            
            n_purchases = nrow(data %>% select(srnro,huonnro,ospvm,omypvm,vuosi) %>% #Purchase (new apartments included)
                                 distinct() %>% 
                                 filter(lubridate::year(ospvm)==vuosi & is.na(omypvm))),
            d_purchases = NA_real_) %>%
    mutate(d_obs = n_obs-lag(n_obs) ,
           d_apartments = n_apartments-lag(n_apartments),
           d_sales = n_sales-lag(n_sales),
           d_purchases = n_purchases-lag(n_purchases))
  
}







#Add hokki
record_function2 <- function(filter_name, data = panel){
  record %>%
    add_row(n = nrow(record)+1,
            filter = paste0(filter_name),
            
            n_obs = nrow(data), #Rows
            d_obs = NA_real_,
            
            n_apartments = n_distinct(data %>% select(srnro,huonnro),na.rm=TRUE), #Apartmentos
            d_apartments = NA_real_,
            
            n_sales = 0,
            d_sales = NA_real_,
            
            
            n_purchases = nrow(data %>% select(srnro,huonnro,ospvm,vuosi) %>% #Purchase (new apartments included)
                                 distinct() %>% 
                                 filter(lubridate::year(ospvm)==vuosi)),
            d_purchases = NA_real_) %>%
    mutate(d_obs = n_obs-lag(n_obs) ,
           d_apartments = n_apartments-lag(n_apartments),
           d_sales = n_sales-lag(n_sales),
           d_purchases = n_purchases-lag(n_purchases))


}