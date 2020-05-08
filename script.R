
library(magrittr)

##### Total deaths per week/day in Spain from 1941 to 2020

# DATA --------------------------------------------------------------------

# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177008&menu=ultiDatos&idp=1254735573002
df_A <- read.csv("6562.csv", sep=";", stringsAsFactors = F, dec = ".")
df_B <- read.csv("6564.csv", sep=";", stringsAsFactors = F)

# https://momo.isciii.es/public/momo/dashboard/momo_dashboard.html#datos
df_C <- read.csv("data.csv", sep=",", stringsAsFactors = F)

# DATA A ------------------------------------------------------------------

noise <- 0

df_A_clean <- df_A %>% 
  `colnames<-`(c("ambito", "fecha", "total")) %>% 
  dplyr::filter(ambito != "Total") %>% 
  dplyr::mutate(date = as.Date(paste0(fecha, "01"), "%YM%m%d")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(total = sum(total, na.rm = T)) %>% 
  dplyr::ungroup()

df_A_days <- data.frame(date = seq(lubridate::ymd(min(df_A_clean$date)),
                                   lubridate::ymd(max(df_A_clean$date)), 
                                   by = 'day')) %>% 
  merge(df_A_clean, by="date",all.x = T) %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::mutate(total_tmp = sum(total, na.rm=T),
                ndays = lubridate::days_in_month(date),
                total = round(total_tmp / ndays)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = total + round(rnorm(1, 0, noise))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, total) %>% 
  dplyr::filter(date < as.Date("2018-04-15"))


# DATA B ------------------------------------------------------------------

df_B_clean <- df_B %>% 
  `colnames<-`(c("ambito", "fecha", "total")) %>% 
  dplyr::filter(ambito != "Total Nacional") %>% 
  dplyr::mutate(date = as.Date(paste0(fecha, "01"), "%YM%m%d")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(total = sum(total, na.rm = T)) %>% 
  dplyr::ungroup()

df_B_days <- data.frame(date = seq(lubridate::ymd(min(df_B_clean$date)),
                                   lubridate::ymd(max(df_B_clean$date)), 
                                   by = 'day')) %>% 
  merge(df_B_clean, by="date",all.x = T) %>% 
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::mutate(total_tmp = sum(total, na.rm=T),
                ndays = lubridate::days_in_month(date),
                total = round(total_tmp / ndays)) %>% 
  dplyr::ungroup() %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = total + round(rnorm(1, 0, noise))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, total)



# DATA C ------------------------------------------------------------------

df_C_days <- df_C %>% 
  dplyr::filter(ambito == "nacional", cod_sexo == "all", cod_gedad == "all") %>% 
  dplyr::select(fecha=fecha_defuncion, total=defunciones_observadas) %>% 
  dplyr::mutate(date = as.Date(fecha)) %>% 
  dplyr::select(date, total)



# EXPLORE FUSIONED DATA ---------------------------------------------------

df <- rbind(df_A_days, df_B_days, df_C_days) %>% 
  dplyr::mutate(date_posix = as.POSIXct(date),
                year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                yday = lubridate::yday(date),
                week = lubridate::week(date),
  ) %>% 
  dplyr::mutate(t = 1:dplyr::n(),
                total_smooth_1 = predict(loess(total ~ t, data = ., span = 0.003)),
                total_smooth_2 = predict(loess(total ~ t, data = ., span = 0.0008)),
                total_smooth = ifelse(date < as.Date("2018-04-15"), total_smooth_1, total_smooth_2))

p <- df %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=date_posix, y=total), size=0.25)+
  ggplot2::scale_y_continuous(name = "Deaths", limits = c(0, NA))+
  ggplot2::scale_x_datetime(name="", date_labels = "%Y", date_breaks = "5 years", limits=range(df$date_posix))+
  hrbrthemes::theme_ipsum_rc(axis_title_size = 12)

p
plotly::ggplotly(p)



# animation?

plot_evolution <- function(df, year_, month_, day_){
  date_year_start <- as.Date(paste0(c(year_, "01", "01"), collapse = "-"))
  date_year_end <- as.Date(paste0(c(year_, "12", "31"), collapse = "-"))
  date_ <- as.Date(paste0(c(year_, month_, day_), collapse = "-"))
  
  title_base <- "Deaths per day in Spain from 1941 to 2020"
  file_name <- paste0("img_", year_, stringr::str_pad(month_, 2, pad = "0"), stringr::str_pad(day_, 2, pad = "0"), ".png")
  family_base <- "Roboto Condensed"
  df %>% 
    dplyr::filter(year <= year_) %>% 
    # tidyr::nest(-year) %>%
    # dplyr::mutate(m = purrr::map(data, loess, formula = total ~ yday, span = 0.1),
    #               total_smooth = purrr::map(m, `[[`, "fitted")) %>%
    # dplyr::select(-m) %>%
    # tidyr::unnest(cols = c(data, total_smooth)) %>%
    ggplot2::ggplot()+
    ggplot2::geom_line(data = . %>% dplyr::filter(year < year_),
                       ggplot2::aes(x=yday, y=total_smooth, group = year, color=year),
                       size = 0.5, alpha = 0.2, na.rm=T)+
    ggplot2::geom_line(data = . %>% dplyr::filter(year == year_),
                       ggplot2::aes(x=yday, y=total_smooth, color=year, alpha = date < date_), 
                       size=1.5, na.rm=T, show.legend = F)+
    ggplot2::scale_alpha_manual(values=c(0,1))+
    
    # ggplot2::annotate("label", x=lubridate::yday(date_)+5, y=df %>% dplyr::filter(date == date_) %>% dplyr::pull(total),
                      # label=date_, hjust=0, color="black", size=4, family = family_base)+
    ggplot2::annotate("label", x=0-15, y=df %>% dplyr::filter(date == date_year_start) %>% dplyr::pull(total), 
                      label=paste0("Year\n",year_), hjust=0.5, color="black", size=4, family = family_base)+
    # ggplot2::annotate("label", x=366+5, y=df %>% dplyr::filter(date == date_year_start) %>% dplyr::pull(total), 
    #                   label=year_, hjust=0, color="black", size=4, family = family_base)+
    ggplot2::scale_x_continuous(name = "Day", expand = c(0, 0), 
                                breaks = seq(0,360,by=30), limits = c(-40, 380))+
    ggplot2::scale_y_continuous(name = "Deaths", expand = c(0,0), n.breaks = 11, limits = c(0, 2500))+
    ggplot2::scale_color_viridis_c(name="Year", option = "D", limits = c(1940, 2020), n.breaks=9)+
    ggplot2::guides(color = ggplot2::guide_colourbar(barwidth = 1, barheight = 22, ticks.linewidth = 2,
                                                     title.theme = ggplot2::element_text(size=14, family = family_base),
                                                     label.theme = ggplot2::element_text(size=12, family = family_base)) )+
    # ggplot2::ggtitle(title_base)+
    hrbrthemes::theme_ipsum_rc(plot_title_size = 14, axis_title_size = 14)+
    ggplot2::ggsave(file_name, device="png", width = 10, height = 6, dpi=100)
}


# population <- c(26014278,28117873,30582936,33956047,37742561,39433942,40499791,44708964,46063511)
# year <- c(1940,1950,1960,1970,1981,1991,2001,2006,2008)

population <- c(seq(26014278, 28117873, length.out = 10),
                seq(28117873, 30582936, length.out = 10),
                seq(30582936, 33956047, length.out = 10),
                seq(33956047, 37742561, length.out = 11),
                seq(37742561, 39433942, length.out = 10),
                seq(39433942, 40499791, length.out = 10),
                seq(40499791, 44708964, length.out = 5),
                seq(44708964, 46063511, length.out = 2),
                seq(46063511, 46751816, length.out = 12))
year <- seq(1941, 2020)

df_capita <- rbind(df_A_days, df_B_days, df_C_days) %>% 
  dplyr::mutate(date_posix = as.POSIXct(date),
                year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                yday = lubridate::yday(date),
                week = lubridate::week(date),
  ) %>% 
  # tidyr::nest(-year) %>%
  # dplyr::mutate(m = purrr::map(data, loess, formula = total ~ yday, span = 0.3),
  #               total_smooth = purrr::map(m, `[[`, "fitted")) %>%
  # dplyr::select(-m) %>%
  # tidyr::unnest(cols = c(data, total_smooth)) %>%
  dplyr::mutate(t = 1:dplyr::n(),
                total_smooth_1 = predict(loess(total ~ t, data = . , span = 0.003)),
                total_smooth_2 = predict(loess(total ~ t, data = . , span = 0.0008)),
                total_smooth = ifelse(date < as.Date("2018-04-15"), total_smooth_1, total_smooth_2)) %>%
  merge(data.frame(year, population), by="year") %>%
  dplyr::mutate(total_smooth = total_smooth / population * 1e6)

plot_evolution_per_capita <- function(df_capita, year_, month_, day_){
  date_year_start <- as.Date(paste0(c(year_, "01", "01"), collapse = "-"))
  date_year_end <- as.Date(paste0(c(year_, "12", "31"), collapse = "-"))
  date_ <- as.Date(paste0(c(year_, month_, day_), collapse = "-"))
  
  title_base <- "Deaths per day in Spain from 1941 to 2020"
  file_name <- paste0("img_capita_", year_, stringr::str_pad(month_, 2, pad = "0"), stringr::str_pad(day_, 2, pad = "0"), ".png")
  family_base <- "Roboto Condensed"
  df_capita %>% 
    dplyr::filter(year <= year_) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_line(data = . %>% dplyr::filter(year < year_),
                       ggplot2::aes(x=yday, y=total_smooth, group = year, color=year),
                       size = 0.5, alpha = 0.2, na.rm=T)+
    ggplot2::geom_line(data = . %>% dplyr::filter(year == year_),
                       ggplot2::aes(x=yday, y=total_smooth, color=year, alpha = date < date_), 
                       size=1.5, na.rm=T, show.legend = F)+
    ggplot2::scale_alpha_manual(values=c(0,1))+
    
    # ggplot2::annotate("label", x=lubridate::yday(date_)+5, y=df %>% dplyr::filter(date == date_) %>% dplyr::pull(total),
    # label=date_, hjust=0, color="black", size=4, family = family_base)+
    ggplot2::annotate("label", x=0-15, y=df_capita %>% dplyr::filter(date == date_year_start) %>% dplyr::pull(total_smooth),
                      label=paste0("Year\n",year_), hjust=0.5, color="black", size=4, family = family_base)+
    # ggplot2::annotate("label", x=366+5, y=df %>% dplyr::filter(date == date_year_start) %>% dplyr::pull(total), 
    #                   label=year_, hjust=0, color="black", size=4, family = family_base)+
    ggplot2::scale_x_continuous(name = "Day", expand = c(0, 0), 
                                breaks = seq(0,360,by=30), limits = c(-40, 380))+
    ggplot2::scale_y_continuous(name = "Deaths per million habitants", expand = c(0,0), n.breaks = 11, limits = c(0, 70))+
    ggplot2::scale_color_viridis_c(name="Year", option = "D", limits = c(1940, 2020), n.breaks=9)+
    ggplot2::guides(color = ggplot2::guide_colourbar(barwidth = 1, barheight = 22, ticks.linewidth = 2,
                                                     title.theme = ggplot2::element_text(size=14, family = family_base),
                                                     label.theme = ggplot2::element_text(size=12, family = family_base)) )+
    # ggplot2::ggtitle(title_base)+
    hrbrthemes::theme_ipsum_rc(plot_title_size = 14, axis_title_size = 14)+
    ggplot2::ggsave(file_name, device="png", width = 10, height = 6, dpi=100)
}


year_ <- 2020
month_ <- 5
day_ <- 2
plot_evolution(df, year_, month_, day_)
plot_evolution_per_capita(df_capita, year_, month_, day_)

date_start <- lubridate::ymd("1941-01-01")
date_end <- lubridate::ymd("2020-05-02")
date_array <- seq(date_start, date_end, by="2 weeks")
pbapply::pblapply(date_array, function(date_){
  year_ <- lubridate::year(date_)
  month_ <- lubridate::month(date_)
  day_ <- lubridate::day(date_)
  plot_evolution_per_capita(df_capita, year_, month_, day_)
  return(0)
}, cl = 4)



## FFMPEG
# ffmpeg -r 60 -f image2 -s 1000x600 -pattern_type glob -i '*.png' -vcodec libx264 -crf 25 -pix_fmt yuv420p test.mp4

# ffmpeg -i test.mp4 -filter_complex "[0]trim=0:N[hold];[0][hold]concat[extended];[extended][0]overlay" out.mp4

# ffmpeg -i out.mp4 -vf drawtext="fontfile=./Roboto_Condensed/RobotoCondensed-Regular.ttf: text='Deaths per day in Spain from 1941 to 2020': fontcolor=black: fontsize=20: box=1: boxcolor=black@0.0: boxborderw=0: x=(w-text_w)/2: y=20" -codec:a copy output_text.mp4
