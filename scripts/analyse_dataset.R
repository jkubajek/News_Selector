# dataset_dir <- "D:/KPRM/Linux/Projekty_all/Scrapy/Data/Articles/"

# DF_1 <- fromJSON(file = paste0(dataset_dir, "page_RMF.json")) %>% clean_RMF(.)
# DF_2 <- fromJSON(file = paste0(dataset_dir, "page_Gazeta.json")) %>% clean_Gazeta(.)
# DF_3 <- fromJSON(file = paste0(dataset_dir, "page_Interia.json")) %>% clean_Interia(.)
# DF_4 <- fromJSON(file = paste0(dataset_dir, "page_Dziennik.json")) %>% clean_Dziennik(.)
# DF_5 <- fromJSON(file = paste0(dataset_dir, "page_RadioZET.json")) %>% clean_RadioZET(.)
# DF_6 <- fromJSON(file = paste0(dataset_dir, "page_PAP.json")) %>% clean_PAP(.)
# DF_7 <- fromJSON(file = paste0(dataset_dir, "page_TVN24.json")) %>% clean_TVN24(.)
# DF_8 <- fromJSON(file = paste0(dataset_dir, "page_TVN24bis.json")) %>% clean_TVN24bis(.)

load("D:/KPRM/Linux/Projekty_all/Projekty/Data/Data_unnested.RData")

data_grouped <- joint_articles %>%
    group_by(date, word) %>%
    summarise(counts = n()) %>%
    ungroup() %>%
    mutate(week_day = lubridate::wday(date, week_start = 1),
           month = lubridate::month(date),
           year = lubridate::year(date),
           week = paste0(lubridate::week(date), "_", year))

quantile <- data_grouped %>%
    filter(week_day < 6 &  year >= 2018) %>%
    # filter(week_day > 5 &  year >= 2018) %>%
    # group_by(week, word) %>%
    # summarise(counts = sum(counts), date = min(date), days_num = n()) %>%
    # filter(days_num > 1) %>%
    group_by(date) %>%
    summarise(p_95 = quantile(counts, probs = 0.95),
              p_94 = quantile(counts, probs = 0.94),
              p_93 = quantile(counts, probs = 0.93),
              p_92 = quantile(counts, probs = 0.92),
              p_91 = quantile(counts, probs = 0.91),
              p_90 = quantile(counts, probs = 0.9)) %>%
    ungroup() %>%
    tidyr::gather(key = "prob", value = "value", -date)

ggplot(data = quantile)+
    geom_density(aes(x = value, y = ..density..))+
    facet_wrap(~prob)+
    theme_bw()

quantile %>%
    group_by(prob) %>%
    summarise(mean_val = mean(value), median_val = median(value))
