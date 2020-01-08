# This file should be sourced
options(stringsAsFactors = F)

sites <- tibble(site = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                    site_name = c("RMF 24", "Gazeta.pl", "Interia", "Radio ZET", "Dziennik.pl", 
                                  "PAP", "TVN24", "TVN24 bis", "TVP Info",
                                  "Polskie Radio", "Polsat News", "Wprost"))

clean_RMF <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        filter(! str_detect(url, pattern = "twojezdrowie.rmf24.pl")) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = ">>>>", fixed = T)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, autor, source, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "01") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_Gazeta <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = "(>>>|ZOBACZ TEŻ)", fixed = F)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, autor, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "02") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_Interia <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! str_detect(text, pattern = "^ Zdjęcie")) %>%
        filter(! grepl(text, pattern = "(>>>|ZOBACZ TEŻ)", fixed = F)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, source) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "03") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                             ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
        
    
    return(data)
}

clean_RadioZET <- function(data){
    # Radio ZET to jedyna z wybranych stron, ktora nie pokazuje wewnetrznego id artykulu
    data <- do.call("rbind", lapply(data, function(x) try(as.data.frame(x))))
    data <- data %>% 
        distinct(title, .keep_all = T) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%d-%m-%Y") %>% format("%Y-%m-%d") %>% as.Date()) %>%
        mutate(site = "04") %>% 
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! str_detect(text, pattern = "^ Zdjęcie")) %>%
        filter(! grepl(text, pattern = "(>>>|ZOBACZ TEŻ|SYLWETKA)", fixed = F)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, source, tags, site) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        arrange(desc(id))
    
    return(data)
}

clean_Dziennik <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%d-%m-%Y") %>% format("%Y-%m-%d") %>% as.Date()) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! str_detect(text, pattern = "^ Zdjęcie")) %>%
        filter(! grepl(text, pattern = "(>>>|ZOBACZ TEŻ|SYLWETKA)", fixed = F)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, source) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "05") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_PAP <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, autor, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "06") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_TVN24 <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        distinct(id, .keep_all = T) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        filter(! str_detect(text, pattern = "czytaj dalej")) %>%
        group_by(id, url, date, time, title, lead, autor, source) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "07") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_TVN24bis <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        mutate(text = str_replace_all(text, pattern = "(\\r|\\n)", replacement = " "),
               text = str_replace_all(text, pattern = "czytaj więcej  ", replacement = " "),
               text = str_replace_all(text, pattern = "^<U\\+\\w+>a(?=.)", replacement = " "),
               text = str_replace_all(text, pattern = "(\\s{2,20})", replacement = " ")) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        filter(autor != "Artykuł sponsorowany" & source != "Artykuł sponsorowany") %>%
        filter(! str_detect(text, pattern = "ARTYKUŁ SPONSOROWANY")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = " ", fixed = T)) %>%
        filter(! grepl(text, patter = "twitter.com", fixed = T)) %>%
        filter((str_count(text, "[A-Z]") / nchar(text)) < 0.3) %>%
        filter(nchar(text) > 100) %>%
        filter(! str_detect(text, pattern = "^ Zdjęcie")) %>%
        filter(! grepl(text, pattern = "(>>>|ZOBACZ TEŻ|SYLWETKA)", fixed = F)) %>%
        filter(! grepl(text, pattern = "==null)")) %>%
        filter(! grepl(text, pattern = "pvideo=false")) %>%
        group_by(id, url, date, time, title, lead, autor, source) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "08") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_TVP_INFO <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(date = date %>% as.Date(format = "%d-%m-%Y") %>% 
                   format("%Y-%m-%d") %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(nchar(text) > 100) %>%
        group_by(id, url, date, time, title, lead, autor, source, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "09") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_Polskie_Radio <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(date = date %>% as.Date(format = "%d-%m-%Y") %>% 
                   format("%Y-%m-%d") %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = "(Polskiego Radia 24|PR24|audycji|audycja|Polskim Radiu)", fixed=F)) %>%
        group_by(id, url, date, time, title, lead, autor, source, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "10") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_Polsat_News <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(nchar(text) > 100) %>%
        filter(! grepl(text, pattern = "Polsat News")) %>%
        group_by(id, url, date, time, title, lead, autor, source, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "11") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}

clean_Wprost <- function(data){
    data <- do.call("rbind", lapply(data, as.data.frame))
    data <- data %>% 
        distinct(id, .keep_all = T) %>%
        mutate(date = date %>% as.Date(format = "%Y-%m-%d")) %>%
        unnest_tokens(text, text, token = "regex", to_lower = F, pattern = " \\|\\| ") %>%
        mutate(text = gsub(text, pattern = "|| ", replacement = "", fixed = T)) %>%
        filter(nchar(text) > 100) %>%
        group_by(id, url, date, time, title, lead, autor, source, tags) %>%
        summarise(text = paste0(text, collapse = " || ")) %>%
        ungroup() %>%
        mutate(site = "12") %>% 
        rename(id_site = id) %>% # id na stronie
        group_by(date) %>%
        arrange(time) %>%
        mutate(num = seq(1, n()),
               id = paste0(str_remove_all(date, "-"), site, ifelse(num < 10, paste0("00", num),
                                                                   ifelse(num < 100, paste0("0", num), num)))) %>% # data 0 kolejnosc
        ungroup() %>%
        dplyr::select(-num) %>%
        arrange(desc(id))
    
    return(data)
}