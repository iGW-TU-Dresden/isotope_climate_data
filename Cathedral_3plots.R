# focus on three plots
library(readxl)
# library(writexl)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)
library(tidyverse)
# library(stringr) 
# library(Hmisc)
library(corrplot)
setwd("D:/Master4_TUD/TA_karst")

##  PLOT 1. rainfall, infil, d18O ----
####  get data ----
df_cave <- read_xlsx("./analyze/Cathedral cave/ttest_Cathedral.xlsx")
df_cave <- filter(df_cave, year < 2015) 
names(df_cave)

#### 1.1 ADD P_SISAL from the bottom ----
df_SISAL <- df_cave[, c("year","month","P_SISAL", "infil_SISAL")] 
df_SISAL$date <- as.Date(paste(df_SISAL$year, df_SISAL$month, "01", sep = "-"))
  
p_SISAL <- ggplot(df_SISAL, aes(x = date)) +
  scale_x_date(limits = as.Date(c("2009-01-02", "2013-12-31")), expand = c(0, 0),
               date_labels = "%Y", date_breaks = "1 year") +
  # geom_vline(aes(xintercept = as.numeric(as.Date(paste(year(date), "01-01", sep = "-")))), color = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  theme(axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 9)
        ,axis.title.x = element_text(vjust = -0.5)) +  
  ylab("SISAL") + 
  #change
  geom_bar(aes(y = P_SISAL), stat = "identity", fill = "grey87", width=31) +
  geom_bar(aes(y = infil_SISAL), stat = "identity", fill = "grey50", width=31) +
  scale_y_continuous(limits = c(0, 250), expand = c(0, 0)) +
  theme(plot.margin = margin(10,2, 10, 20, "pt"),
        axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid")) 
p_SISAL

ggsave("./analyze/Cathedral cave/pic/SISAL_water.png", plot = p_SISAL, width = 5, height = 2.4, dpi = 800, bg = "transparent")


#### 1.2 ADD isotope  ----
df_d18O <- read.csv("./analyze/Cathedral cave/Cathedralcave_d18O.csv")
unique(df_d18O$entity)
df_d2H <- read.csv("./analyze/Cathedral cave/Cathedralcave_d2H.csv")
unique(df_d2H$entity)
 
entities_to_delete <- c("sin","Brisabane","Cobar","Sydney")
df_SISAL_d18O <- df_d18O[!df_d18O$entity %in% entities_to_delete, ]
df_SISAL_d2H <- df_d2H[!df_d2H$entity %in% entities_to_delete, ]
# convert to date format
df_SISAL_d18O$date <- as.Date(df_SISAL_d18O$date)
df_SISAL_d2H$date <- as.Date(df_SISAL_d2H$date)

# 1.2.1 d18O
p_SISAL_d18O <- ggplot(df_SISAL_d18O, aes(x = date)) +
  # change
  scale_x_date(limits = as.Date(c("2009-01-02", "2013-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(-10, 2), expand = c(0, 0),
                     sec.axis = sec_axis(trans=~., name = expression("d18O ‰"[paste(VSMOW)]))
  ) +
  geom_point(aes(y = value, color = entity), data = df_SISAL_d18O) +
  geom_line(aes(y = value,color = entity), data = df_SISAL_d18O) +

  # geom_vline(aes(xintercept = as.numeric(as.Date(paste(year(date), "01-01", sep = "-")))), color = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  theme(axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 9),
        legend.position = "none",
        plot.margin = margin(10, 0, 10, 20, "pt")
        ) +
  scale_color_manual(values = c("cathedral_cave_drip279" = "springgreen", "cathedral_cave_drip280" = "seagreen3",
                                "cathedral_cave_drip319" = "olivedrab2", "cathedral_cave_drip320" = "olivedrab3",
                                "cathedral_cave_drip322" = "springgreen4", "cathedral_cave_drip330" = "#006400", 
                                "Wellington" = "black" ))

p_SISAL_d18O
ggsave("./analyze/Cathedral cave/pic/SISAL_d18O.png", plot = p_SISAL_d18O, width = 5, height = 3.3, dpi = 800, bg = "transparent")

# 1.2.2 d2H
p_SISAL_d2H <- ggplot(df_SISAL_d2H, aes(x = date)) +
  # change
  scale_x_date(limits = as.Date(c("2009-01-02", "2013-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = NULL, breaks = NULL, limits = c(-70, 20),  expand = c(0, 0),
                     sec.axis = sec_axis(trans=~., name = expression("d2H ‰"[paste(VSMOW)]))) +
  geom_point(aes(y = value, color = entity), data = df_SISAL_d2H) +
  geom_line(aes(y = value,color = entity), data = df_SISAL_d2H) +

  # geom_vline(aes(xintercept = as.numeric(as.Date(paste(year(date), "01-01", sep = "-")))), color = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  theme(axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 9),
        # axis.title.x = element_text(vjust = -0.5),
        plot.margin = margin(10, 0, 10, 20, "pt")
        ,legend.position = "none") +
  scale_color_manual(values = c("cathedral_cave_drip279" = "springgreen", "cathedral_cave_drip280" = "seagreen3",
                                "cathedral_cave_drip319" = "olivedrab2", "cathedral_cave_drip320" = "olivedrab3",
                                "cathedral_cave_drip322" = "springgreen4", "cathedral_cave_drip330" = "#006400", 
                                "Wellington" = "black" ))
p_SISAL_d2H
ggsave("./analyze/Cathedral cave/pic/SISAL_d2H.png", plot = p_SISAL_d2H, width = 5, height = 2.5, dpi = 800, bg = "transparent")


#### 1.3 Background plot for drip rate ----
df_dripRate <- read.csv("./analyze/Cathedral cave/Cathedral_dripRate.csv")
df_dripRate$date <- as.Date(df_dripRate$date)

p2 <- ggplot(df_dripRate, aes(x = date)) +
  scale_x_date(limits = as.Date(c("2009-01-02", "2022-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year") +
  # geom_vline(aes(xintercept = as.numeric(as.Date(paste(year(date), "01-01", sep = "-")))), color = "black", linetype = "dashed", size = 0.5) +
  # theme_classic() +
  theme(
    #       plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(face = "bold", size = 9)) +
  theme(plot.margin = margin(10,2, 10, 20, "pt"),
        legend.position = "none",
        axis.text.x = element_blank(),
        panel.border = element_blank(),panel.background = element_rect(fill = "white"),
        axis.line.y = element_line(colour = "black")) 
### method 1: log10 ----
p_driprate1 <- p2+  
  geom_point(aes(y = value, color = entity), data = df_dripRate, size = 1) +
  geom_line(aes(y = value,color = entity), data = df_dripRate, size = 0.4) +
  # geom_smooth(span = 0.03,size = 0.6, se = FALSE) +
  xlab("")+
  ylab(expression(paste("Drip rate (", min^-1, ")")))+
  scale_y_log10(limits = c(0.003, 300),labels = trans_format("log10", math_format(10^.x))) + #some values are 0, so they are on the bottom
  scale_color_manual(values = c("cathedral_cave_drip279" = "springgreen", "cathedral_cave_drip280" = "seagreen3",
                                "cathedral_cave_drip319" = "olivedrab2", "cathedral_cave_drip320" = "olivedrab3",
                                "cathedral_cave_drip322" = "springgreen4", "cathedral_cave_drip330" = "#006400", 
                                "Wellington" = "black" ))
p_driprate1
ggsave("./analyze/Cathedral cave/pic/SISAL_driprate_log2022.png", plot = p_driprate1, width = 5, height = 3.4, dpi = 1000, bg = "transparent")
 
#### 1.4 ADD MSWEP on the top ----
df_MSWEP <- df_cave[, c("year","month","P_MSWEP", "infil_MSWEP")] 
df_MSWEP$date <- as.Date(paste(df_MSWEP$year, df_MSWEP$month, "15", sep = "-"))
df_MSWEP <- df_MSWEP %>% filter(infil_MSWEP >= 0)

p_MSWEP <- ggplot(df_MSWEP, aes(x = date)) +
  scale_x_date(limits = as.Date(c("2009-01-02", "2013-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year") +
  # geom_vline(aes(xintercept = as.numeric(as.Date(paste(year(date), "01-01", sep = "-")))), color = "grey57", linetype = "dashed", size = 0.5) +
  theme_classic() +
  theme(axis.line = element_line(color = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 9),
        axis.title.x = element_text(vjust = -0.5)) +  
  ylab("                        MSWEP") + 
  geom_bar(aes(y = P_MSWEP), stat = "identity", fill = "grey87", width=31) +
  geom_bar(aes(y = infil_MSWEP), stat = "identity", fill = "grey50", width=31) +
  scale_y_reverse(limits=c(550,0), expand=c(0,0))+
  theme(plot.margin = margin(10,2, 10, 20, "pt"),
        axis.line.y = element_line(color = "black", size = 0.5, linetype = "solid")) 
p_MSWEP

ggsave("./analyze/Cathedral cave/pic/MSWEP_water.png", plot = p_MSWEP, width = 5, height = 2.4, dpi = 800, bg = "transparent")
#### 1.5 THEN COMBINE THEM IN PPT ----
 
##  PLOT 2. LMWL -----
df_isotope <- read.csv("./analyze/Cathedral cave/Cathedralcave_isotope.csv")
df_isotope$month <-  month(df_isotope$date)
df_isotope$season <- ifelse(df_isotope$month %in% c(12, 1, 2), "Summer",
                            ifelse(df_isotope$month %in% c(3, 4, 5), "Fall", 
                                   ifelse(df_isotope$month %in% c(6, 7, 8), "Winter", 
                                          "Spring")))

eq_GMWL = paste0("GMWL:d2H = 8 * d18O + 10")
######  2-1 NEW. separate rain and drip + GNIP ----
#only SISAL
color_all <- c("cathedral_cave_drip279" = "springgreen", "cathedral_cave_drip280" = "seagreen3",
                "cathedral_cave_drip319" = "olivedrab2", "cathedral_cave_drip320" = "olivedrab3",
                "cathedral_cave_drip322" = "springgreen4", "cathedral_cave_drip330" = "#006400", 
                 "Wellington" = "red","Brisabane" ="darkblue",
                "Cobar" ="deepskyblue2","Sydney" = "lightsteelblue2", "sin" = "grey")

shape_palette <- c("Wellington" = 19,  # Circle shape
                   "Brisabane" = 19,
                   "Cobar" = 19,
                   "Sydney" = 19,
                   "sin" = 19,
                   "cathedral_cave_drip279" = 15, # Square shape
                   "cathedral_cave_drip280" = 15,
                   "cathedral_cave_drip319" = 15,
                   "cathedral_cave_drip320" = 15,
                   "cathedral_cave_drip319" = 15,
                   "cathedral_cave_drip322" = 15,
                   "cathedral_cave_drip330" = 15)
 
# calculate the rain LMWL
df_isotope_SISAL <- df_isotope %>% filter(str_detect(entity, "Wellington"))
model_SISAL <- lm(d2H ~ d18O, data = df_isotope_SISAL)
r_squared <- summary(model_SISAL)$r.squared
RMSE <- sqrt(mean(model_SISAL$residuals^2))
n <- length(model_SISAL[["fitted.values"]]) 
alpha_SISAL <- coef(model_SISAL)[[2]]
beta_SISAL <- coef(model_SISAL)[[1]]
LWML_text <- paste("δ2H = δ18O *", round(alpha_SISAL, 2), "+", round(beta_SISAL, 2))
LWML_text

df_isotope_1 <- df_isotope %>% filter(str_detect(entity, "Brisabane"))
model_1 <- lm(d2H ~ d18O, data = df_isotope_1)
r_squared1 <- summary(model_1)$r.squared
RMSE1 <- sqrt(mean(model_1$residuals^2))
n1 <- length(model_1[["fitted.values"]]) 
alpha_1 <- coef(model_1)[[2]]
beta_1 <- coef(model_1)[[1]]
LWML_text1 <- paste("δ2H = δ18O *", round(alpha_1, 2), "+", round(beta_1, 2))
LWML_text1

df_isotope_2 <- df_isotope %>% filter(str_detect(entity, "Cobar"))
model_2 <- lm(d2H ~ d18O, data = df_isotope_2)
r_squared2 <- summary(model_2)$r.squared
RMSE2 <- sqrt(mean(model_2$residuals^2))
n2 <- length(model_2[["fitted.values"]]) 
alpha_2 <- coef(model_2)[[2]]
beta_2 <- coef(model_2)[[1]]
LWML_text2 <- paste("δ2H = δ18O *", round(alpha_2, 2), "+", round(beta_2, 2))
LWML_text2

df_isotope_3 <- df_isotope %>% filter(str_detect(entity, "Sydney"))
model_3 <- lm(d2H ~ d18O, data = df_isotope_3)
r_squared3 <- summary(model_3)$r.squared
RMSE3 <- sqrt(mean(model_3$residuals^2))
n3 <- length(model_3[["fitted.values"]]) 
alpha_3 <- coef(model_3)[[2]]
beta_3 <- coef(model_3)[[1]]
LWML_text3 <- paste("δ2H = δ18O *", round(alpha_3, 2), "+", round(beta_3, 2))
LWML_text3

df_isotope_sin <- df_isotope %>% filter(str_detect(entity, "sin"))
model_sin <- lm(d2H ~ d18O, data = df_isotope_sin)
r_squaredsin <- summary(model_sin)$r.squared
RMSEsin <- sqrt(mean(model_sin$residuals^2))
nsin <- length(model_sin[["fitted.values"]]) 
alpha_sin <- coef(model_sin)[[2]]
beta_sin <- coef(model_sin)[[1]]
LWML_textsin <- paste("δ2H = δ18O *", round(alpha_sin, 2), "+", round(beta_sin, 2))
LWML_textsin

p_wml_3source <- ggplot(df_isotope, aes(x = d18O, y = d2H, color= entity, shape = entity)) +
  geom_point(size = 1) +
  # labs(subtitle = paste0("Equation: ", LWML_text, " |","\n",
  #                        "R-squared: ", round(r_squared, 2)," | RMSE: ", round(RMSE, 2)," (‰VSMOW)"," | n: ", n))+
  # xlim(-13, 8) + ylim(-100, 30) +
  scale_color_manual(values = color_all) +  
  scale_shape_manual(values = shape_palette) +
  geom_abline(intercept = beta_SISAL, slope = alpha_SISAL, color="red", size=0.7) + #LMWL of SISAL
  geom_abline(intercept = beta_1, slope = alpha_1, color="darkblue", size= 0.7) + #LMWL of EM
  geom_abline(intercept = beta_2, slope = alpha_2, color="deepskyblue2", size= 0.7) + #LMWL of KA
  geom_abline(intercept = beta_3, slope = alpha_3, color="lightsteelblue2", size= 0.7) + #LMWL of BS
  
  geom_abline(intercept =10, slope =8, color="black", size = 0.2, linetype="dashed" )+ #GMWL
  theme_classic()+
  theme(axis.title = element_text(face = "bold", size = 16),
        axis.text = element_text(face = "bold", size = 13)
        # ,legend.position = "none"
  )

p_wml_3source
ggsave("./analyze/Cathedral cave/pic/MWL_3source.png", plot = p_wml_3source, width = 8, height = 5, bg = "transparent")

######  2-1. separate rain and drip ----
#only SISAL
df_isotope_SISAL <- df_isotope %>% filter(str_detect(entity, "^cathedral|Wellington"))

# calculate the rain LMWL
df_isotope_rain <- df_isotope %>% filter(str_detect(entity, "Wellington"))

model_SISAL <- lm(d2H ~ d18O, data = df_isotope_rain)
r_squared <- summary(model_SISAL)$r.squared
RMSE <- sqrt(mean(model_SISAL$residuals^2))
n <- length(model_SISAL[["fitted.values"]])
alpha_SISAL <- coef(model_SISAL)[[2]]
beta_SISAL <- coef(model_SISAL)[[1]]
annotation_text_drip <- paste("δ2H = δ18O *", round(alpha_SISAL, 2), "+", round(beta_SISAL, 2))

p_wml_2source <- ggplot(df_isotope_SISAL, aes(x = d18O, y = d2H, color= entity, shape = entity)) +  # shape=Site,
  geom_point(size = 1.5) +
   xlim(-6, 1) + ylim(-30, 10) +
  scale_color_manual(values = color_all) +  
  scale_shape_manual(values = shape_palette) +
  geom_abline(intercept = beta_SISAL, slope = alpha_SISAL, color="red", size=0.5) + #LMWL of rain
  geom_abline(intercept =10, slope =8, color="black", size = 0.2, linetype="dashed")+ #GMWL
  theme_classic()+
  theme(axis.title = element_text(face = "bold", size = 23),
        axis.text = element_text(face = "bold", size = 18),
        legend.position = "none"
        )

p_wml_2source
ggsave("./analyze/Cathedral cave/pic/MWL_2source.png", plot = p_wml_2source, width = 6, height = 5, bg = "transparent")

######  2-2. Rain ----
color_season <- c("Winter" = "blue4","Spring" = "pink","Summer" = "red3","Fall" = "orange")

p_wml_season <- ggplot(df_isotope_rain, aes(x = d18O, y = d2H, color= season)) +  # shape=Site,
  geom_point(size = 2.5) +
  # labs(subtitle = paste0("Equation: ", annotation_text_drip, " |","\n",
  #                        "R-squared: ", round(r_squared, 2)," | RMSE: ", round(RMSE, 2)," (‰VSMOW)"," | n: ", n))+
  # xlim(-8, 0) + ylim(-60, 0) +
  scale_color_manual(values = color_season) +
  geom_abline(intercept = beta_SISAL, slope = alpha_SISAL, color="red", size= 0.5) + #LMWL of rain
  geom_abline(intercept =10, slope =8, color="black", size = 0.2, linetype="dashed")+ #GMWL
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 23),
        axis.text = element_text(face = "bold", size = 18))

p_wml_season
ggsave("./analyze/Cathedral cave/pic/MWL_season.png", plot = p_wml_season,width = 6, height = 5, bg = "transparent")

######  2-3. drip ----
df_isotope_drip <- df_isotope %>% filter(str_detect(entity, "^cathedral"))
p_wml_drip <- ggplot(df_isotope_drip, aes(x = d18O, y = d2H, color= entity)) +  # shape=Site,
  geom_point(size = 1.5) +
  xlim(-5.5, 1) +
  ylim(-25, 10) +
  scale_color_manual(values = color_all) +
  geom_abline(intercept = beta_SISAL, slope = alpha_SISAL, color="red", size= 0.5) + #LMWL of rain
  geom_abline(intercept =10, slope =8, color="black", size = 0.2, linetype="dashed")+ #GMWL
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 23),
        axis.text = element_text(face = "bold", size = 18))

p_wml_drip
ggsave("./analyze/Cathedral cave/pic/MWL_drip.png", plot = p_wml_drip, width = 6, height = 5, bg = "transparent")
######  2-4. GNIP 已經放進LMWL了 ----
vars_isotope <- list(
  c("precip_SISAL_Wellington_d18O", "precip_SISAL_Wellington_d2H"),
  c("precip_sin_d18O", "precip_sin_d2H"),
  c("precip_IAEA_Brisabane_d18O", "precip_IAEA_Brisabane_d2H"),
  c("precip_IAEA_Cobar_d18O", "precip_IAEA_Cobar_d2H"),
  c("precip_IAEA_Sydney_d18O", "precip_IAEA_Sydney_d2H")
  # c("d18O_Cathedral_cave_ts_1", "d2H_Cathedral_cave_ts_1"),
  # c("d18O_Cathedral_cave_ts_2", "d2H_Cathedral_cave_ts_2"),
  # c("d18O_Cathedral_cave_ts_3", "d2H_Cathedral_cave_ts_3"),
  # c("d18O_Cathedral_cave_ts_5", "d2H_Cathedral_cave_ts_5"),
  # c("d18O_Cathedral_cave_ts_6", "d2H_Cathedral_cave_ts_6"),
  # c("d18O_Cathedral_cave_ts_7", "d2H_Cathedral_cave_ts_7"),
  # c("d18O_Cathedral_cave_ts_8", "d2H_Cathedral_cave_ts_8")
)

p <- ggplot()+xlim(-12, 5) + ylim(-60, 0) 
for (i in seq_along(LMWLvars)) { #!change! the variable list
  # Please use it with the 2nd function in D:/Master4_TUD/TA_karst/function.R
  # !change! the variable list
  stats <- calculate_comparison_stats(df_cave, vars_isotope[[i]][1], vars_isotope[[i]][2])
  print("ok")
  # print(stats)
  p_value <- stats$p_value
  r_squared <- stats$r_squared
  equation <- stats$equation
  rmse <- stats$rmse
  n <- stats$n
  print(equation)
  # Plot # Please use it with Function 3-2 in D:/Master4_TUD/TA_karst/function.R
  plot_regression(df_cave, vars_isotope, i,equation,p_value,r_squared,rmse,n)
}
 
## PLOT 3. Cross-correlation ---- 
##### 3.1 P.d18O vs drip.d18O  -----
var <- "P.d18O"
df_cor <- read_xlsx(paste0("./analyze/Cathedral cave/ttest_Cathedral_Cor",var,".xlsx"))
lag_range <- 0:12

r_value_list <- vector("list", length = 12)  # One list for each site
p_value_list <- vector("list", length = 12)  # One list for each site

for (site in 3:13) {   #fixed (site從第3格到第幾格)
  site
  site_name <- names(df_cor)[site]  
  site_p_values <- numeric(length(lag_range)) 
  site_r_values <- numeric(length(lag_range)) 
  for (lag in lag_range) { 
    drip_water <- df_cor[[site]]
    d18O <- df_cor[[14 + lag]]  #fixed  第幾格+1
    df <- data.frame(drip_water, d18O)
    df <- na.omit(df)
    correlation_test <- cor.test(df$drip_water, df$d18O, method = "spearman")
    r <- correlation_test$estimate
    site_r_values[lag + 1] <- r
    p_values <- correlation_test$p.value
    site_p_values[lag + 1] <- p_values
  }
  site_r_values <- c(site_name = site_name, site_r_values)
  r_value_list[[site - 2]] <- site_r_values
  site_p_values <- c(site_name = site_name, site_p_values)
  p_value_list[[site - 2]] <- site_p_values
}

lag_p_values_df <- data.frame(Lag = lag_range, stringsAsFactors = FALSE)
correlation_df <- data.frame(Lag = lag_range, stringsAsFactors = FALSE)

for (site in 3:13) {
  site_name <- names(df_cor)[site]
  
  p_values <- p_value_list[[site - 2]][-1]
  correlations <- r_value_list[[site - 2]][-1]
  
  lag_p_values_df[, site_name] <- as.numeric(p_values)
  correlation_df[, site_name] <- as.numeric(correlations)
}
write_xlsx(lag_p_values_df, path = "d18O_p_CD.xlsx")
write_xlsx(correlation_df, path = "d18O_r_CD.xlsx") 

##### 3.2 climate vs DR  -----
var <- "DR"
df_cor <- read_xlsx(paste0("./analyze/Cathedral cave/ttest_Cathedral_Cor",var,".xlsx"))
df_cor$date <- as.Date(paste(df_cor$year, df_cor$month, "01", sep = "-"))

r_value_list <- vector("list", length = 12)  # One list for each site
p_value_list <- vector("list", length = 12)  # One list for each site

for (site in 3:8) {  #fixed (site從第3格到第幾格)
  site_name <- names(df_cor)[site]  
  site_p_values <- numeric(length(lag_range)) 
  site_r_values <- numeric(length(lag_range)) 
  for (lag in lag_range) {
    DR <- df_cor[[site]]
    # climate <- df_cor[[9 + lag]] #fixed (P的第一格)
    # climate <- df_cor[[22 + lag]] #fixed (infil的第一格)
    climate <- df_cor[[35 + lag]] #fixed (ET的第一格)
    df <- data.frame(DR, climate)
    df <- na.omit(df)
    correlation_test <- cor.test(df$DR, df$climate, method = "spearman")
    r <- correlation_test$estimate
    site_r_values[lag + 1] <- r
    p_values <- correlation_test$p.value
    site_p_values[lag + 1] <- p_values
  }
  site_r_values <- c(site_name = site_name, site_r_values)
  r_value_list[[site - 2]] <- site_r_values
  site_p_values <- c(site_name = site_name, site_p_values)
  p_value_list[[site - 2]] <- site_p_values
}

lag_p_values_df <- data.frame(Lag = lag_range, stringsAsFactors = FALSE)
correlation_df <- data.frame(Lag = lag_range, stringsAsFactors = FALSE)

for (site in 3:8) {
  site_name <- names(df_cor)[site]
  p_values <- p_value_list[[site - 2]][-1]
  correlations <- r_value_list[[site - 2]][-1]
  lag_p_values_df[, site_name] <- as.numeric(p_values)
  correlation_df[, site_name] <- as.numeric(correlations)
}
write_xlsx(lag_p_values_df, path = "DR_p_CD.xlsx")
write_xlsx(correlation_df, path = "DR_r_CD.xlsx")


## PLOT 4. Correlation -----
##### 4.1.1 TS1-8 互相對彼此 isotope d18O -----
df_drip_d18O <- df_cave %>%  select("precip_SISAL_Wellington_d18O",
                                    "drip279_d18O","drip280_d18O", "drip319_d18O",                
                                    "drip320_d18O" ,"drip322_d18O","drip330_d18O")%>%
  rename(D279 = drip279_d18O,D280 = drip280_d18O,D319 = drip319_d18O,
         D320 = drip320_d18O,D322 = drip322_d18O,D330 = drip330_d18O 
         ,WT = precip_SISAL_Wellington_d18O
         )
df_drip_d18O_com <- df_drip_d18O[complete.cases(df_drip_d18O), ]
# spearman
d18O_cor_spearman <- round(cor(df_drip_d18O_com, method = "spearman"), digits = 2)
d18O_cor_p_spearman <- cor.mtest(df_drip_d18O_com)$p 
d18O_cor_p_signif <- d18O_cor_p_spearman < 0.05
stars <- ifelse(d18O_cor_p_signif, "*", "")
stars 
png("./cor_drip_d18O_spearman.png", width = 1200, height = 1200, res = 300)
d18O_cor <- corrplot.mixed(d18O_cor_spearman)  
d18O_cor # This will print the plot and save it as png
dev.off() # Close the PNG device

#Pearson
d18O_cor_pearson <- round(cor(df_drip_d18O_com, method = "pearson"), digits = 2)
d18O_cor_p_pearson <- cor.mtest(df_drip_d18O_com)$p 
d18O_cor_p_signif <- d18O_cor_p_pearson < 0.05
stars <- ifelse(d18O_cor_p_signif, "*", "")
stars 
png("./cor_drip_d18O_pearson.png", width = 1200, height = 1200, res = 300)
d18O_cor <- corrplot.mixed(d18O_cor_pearson)  
d18O_cor # This will print the plot and save it as png
dev.off() # Close the PNG device
## put in PPT, manually add stars

##### 4.1.2 TS1-8  互相對彼此 isotope d2H -----
df_drip_d2H <- df_cave %>%  select("precip_SISAL_Wellington_d2H",
                                   "drip279_d2H","drip280_d2H","drip319_d2H","drip320_d2H","drip322_d2H","drip330_d2H")%>%
  rename(D279 = drip279_d2H,D280 = drip280_d2H,D319 = drip319_d2H,
         D320 = drip320_d2H,D322 = drip322_d2H,D330 = drip330_d2H 
         ,WT = precip_SISAL_Wellington_d2H
  )
df_drip_d2H_com <- df_drip_d2H[complete.cases(df_drip_d2H), ] 
#Spearman
d2H_cor_spearman <- round(cor(df_drip_d2H_com, method = "spearman"), digits = 2)
d2H_cor_p_spearman <- cor.mtest(df_drip_d2H_com)$p 
d2H_cor_p_signif <- d2H_cor_p_spearman < 0.05
stars <- ifelse(d2H_cor_p_signif, "*", "")
stars 
png("./cor_drip_d2H_spearman.png", width = 1200, height = 1200, res = 300)
d2H_cor <- corrplot.mixed(d2H_cor_spearman)  
d2H_cor # This will print the plot and save it in png
dev.off() # Close the PNG device

#Pearson
d2H_cor_pearson <- round(cor(df_drip_d2H_com, method = "pearson"), digits = 2)
d2H_cor_p_pearson <- cor.mtest(df_drip_d2H_com)$p 
d2H_cor_p_signif <- d2H_cor_p_pearson < 0.05
stars <- ifelse(d2H_cor_p_signif, "*", "")
stars 
png("./cor_drip_d2H_pearson.png", width = 1200, height = 1200, res = 300)
d2H_cor <- corrplot.mixed(d2H_cor_pearson)  
d2H_cor # This will print the plot and save it as png
dev.off() # Close the PNG device
## put in PPT, manually add stars
               
##### 4.1.3 TS1-8 互相對彼此 drip rate ----- 
df_cave$Date_DR <- as.Date(df_cave$Date_DR) 
df_drip_DR <- df_cave %>%  select(
  # "Date_DR",
  "D279_DR","D280_DR","D319_DR","D320_DR","D322_DR","D330_DR") %>%
  rename(D279 = D279_DR,D280 = D280_DR,D319 = D319_DR,
         D320 = D320_DR,D322 = D322_DR,D330 = D330_DR)
df_drip_DR_com <- df_drip_DR[complete.cases(df_drip_DR), ]
#spearman
DR_cor_spearman <- round(cor(df_drip_DR_com, method = "spearman"), digits = 2)
DR_cor_p_spearman <- cor.mtest(df_drip_DR_com)$p 
DR_cor_p_signif <- DR_cor_p_spearman < 0.05
stars <- ifelse(DR_cor_p_signif, "*", "")
stars 

png("./cor_drip_DR_2014_spearman.png", width = 1200, height = 1200, res = 300)
DR_cor <- corrplot.mixed(DR_cor_spearman)  
DR_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 

#pearson
DR_cor_pearson <- round(cor(df_drip_DR_com, method = "pearson"), digits = 2)
DR_cor_p_pearson <- cor.mtest(df_drip_DR_com)$p 
DR_cor_p_signif <- DR_cor_p_pearson < 0.05
stars <- ifelse(DR_cor_p_signif, "*", "")
stars 
png("./cor_drip_DR_2014_pearson.png", width = 1200, height = 1200, res = 300)
DR_cor <- corrplot.mixed(DR_cor_pearson)  
DR_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 
##### 4.2.1 GNIP 互相對彼此 P -----
df_P <- df_cave %>%  select("P_MSWEP","P_SISAL","P_IAEA_Brisabane","P_Cobar","P_Sydney")%>%
  rename(MSW = P_MSWEP,BB = P_IAEA_Brisabane,Cobar = P_Cobar,
         Sydney = P_Sydney,WT = P_SISAL)
df_P_com <- df_P[complete.cases(df_P), ]

#spearman
P_cor_spearman <- round(cor(df_P_com, method = "spearman"), digits = 2)
P_cor_p_spearman <- cor.mtest(df_P_com)$p 
P_cor_p_signif <- P_cor_p_spearman < 0.05
stars <- ifelse(P_cor_p_signif, "*", "")
stars 

png("./cor_P_spearman.png", width = 1200, height = 1200, res = 300)
P_cor <- corrplot.mixed(P_cor_spearman)  
P_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 

#pearson
P_cor_pearson <- round(cor(df_P_com, method = "pearson"), digits = 2)
P_cor_p_pearson <- cor.mtest(df_P_com)$p 
P_cor_p_signif <- P_cor_p_pearson < 0.05
stars <- ifelse(P_cor_p_signif, "*", "")
stars 
png("./cor_P_pearson.png", width = 1200, height = 1200, res = 300)
P_cor <- corrplot.mixed(P_cor_pearson)  
P_cor # This will print the plot and save it in png
dev.off() # Close the PNG device  

##### 4.2.2 GNIP 互相對彼此 Infiltration -----  
df_infil <- df_cave[, c("infil_MSWEP","infil_SISAL", "infil_IAEA_BB","infil_IAEA_Cobar", "infil_IAEA_Sydney" )] %>%
              rename(MSW = infil_MSWEP,BB = infil_IAEA_BB,Cobar = infil_IAEA_Cobar,
                     Sydney = infil_IAEA_Sydney,WT = infil_SISAL)
df_infil_com <- df_infil[complete.cases(df_infil), ] 
 
#spearman
infil_cor_spearman <- round(cor(df_infil_com, method = "spearman"), digits = 2)
infil_cor_p_spearman <- cor.mtest(df_infil_com)$p 
infil_cor_p_signif <- infil_cor_p_spearman < 0.05
stars <- ifelse(infil_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_infil_spearman.png", width = 1200, height = 1200, res = 300)
infil_cor <- corrplot.mixed(infil_cor_spearman)  
infil_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 

#pearson
infil_cor_pearson <- round(cor(df_infil_com, method = "pearson"), digits = 2)
infil_cor_p_pearson <- cor.mtest(df_infil_com)$p 
infil_cor_p_signif <- infil_cor_p_pearson < 0.05
stars <- ifelse(infil_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_infil_pearson.png", width = 1200, height = 1200, res = 300)
infil_cor <- corrplot.mixed(infil_cor_pearson)  
infil_cor # This will print the plot and save it in png
dev.off() # Close the PNG device  

##### 4.2.3 GNIP 互相對彼此 d18O -----   
df_d18O <- df_cave[, c("precip_sin_d18O","precip_SISAL_Wellington_d18O","precip_IAEA_Brisabane_d18O","precip_IAEA_Cobar_d18O","precip_IAEA_Sydney_d18O")]%>%
  rename(sin = precip_sin_d18O,BB = precip_IAEA_Brisabane_d18O,Cobar = precip_IAEA_Cobar_d18O,
         Sydney = precip_IAEA_Sydney_d18O,WT = precip_SISAL_Wellington_d18O)
df_d18O_com <- df_d18O[complete.cases(df_d18O), ] 

#spearman
d18O_cor_spearman <- round(cor(df_d18O_com, method = "spearman"), digits = 2)
d18O_cor_p_spearman <- cor.mtest(df_d18O_com)$p 
d18O_cor_p_signif <- d18O_cor_p_spearman < 0.05
stars <- ifelse(d18O_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_d18O_spearman.png", width = 1200, height = 1200, res = 300)
d18O_cor <- corrplot.mixed(d18O_cor_spearman)  
d18O_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 

#pearson
d18O_cor_pearson <- round(cor(df_d18O_com, method = "pearson"), digits = 2)
d18O_cor_p_pearson <- cor.mtest(df_d18O_com)$p 
d18O_cor_p_signif <- d18O_cor_p_pearson < 0.05
stars <- ifelse(d18O_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_d18O_pearson.png", width = 1200, height = 1200, res = 300)
d18O_cor <- corrplot.mixed(d18O_cor_pearson)  
d18O_cor # This will print the plot and save it in png
dev.off() # Close the PNG device  

##### 4.2.4 GNIP 互相對彼此 d2H -----   
df_d2H <- df_cave[, c("precip_sin_d2H","precip_SISAL_Wellington_d2H","precip_IAEA_Brisabane_d2H","precip_IAEA_Cobar_d2H","precip_IAEA_Sydney_d2H")]%>%
  rename(sin = precip_sin_d2H,BB = precip_IAEA_Brisabane_d2H,Cobar = precip_IAEA_Cobar_d2H,
               Sydney = precip_IAEA_Sydney_d2H,WT = precip_SISAL_Wellington_d2H)
df_d2H_com <- df_d2H[complete.cases(df_d2H), ]

#spearman
d2H_cor_spearman <- round(cor(df_d2H_com, method = "spearman"), digits = 2)
d2H_cor_p_spearman <- cor.mtest(df_d2H_com)$p 
d2H_cor_p_signif <- d2H_cor_p_spearman < 0.05
stars <- ifelse(d2H_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_d2H_spearman.png", width = 1200, height = 1200, res = 300)
d2H_cor <- corrplot.mixed(d2H_cor_spearman)  
d2H_cor # This will print the plot and save it in png
dev.off() # Close the PNG device 

#pearson
d2H_cor_pearson <- round(cor(df_d2H_com, method = "pearson"), digits = 2)
d2H_cor_p_pearson <- cor.mtest(df_d2H_com)$p 
d2H_cor_p_signif <- d2H_cor_p_pearson < 0.05
stars <- ifelse(d2H_cor_p_signif, "*", "")
stars 
png("./cor_GNIP_d2H_pearson.png", width = 1200, height = 1200, res = 300)
d2H_cor <- corrplot.mixed(d2H_cor_pearson)  
d2H_cor # This will print the plot and save it in png
dev.off() # Close the PNG device  


#### 5.1 P -----
df_SISAL <- df_cave[, c("year","month","P_SISAL","P_MSWEP", "P_Cobar", "P_IAEA_Brisabane","P_Sydney")] 
df_SISAL$date <- as.Date(paste(df_SISAL$year, df_SISAL$month, "01", sep = "-"))
 
p_SISAL <- ggplot(df_SISAL, aes(x = date)) +
  scale_x_date(limits = as.Date(c("2010-01-02", "2013-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year" 
  ) + 
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(face = "bold", size = 13),
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),  # Add panel border
        plot.margin = margin(10,5,10,20,"pt")
  ) + 
  ylab("P (mm/mon)") + 
  geom_smooth(aes(y = P_SISAL),color = "red", span = 0.2, se = FALSE) +
  geom_smooth(aes(y = P_Cobar),   color = "purple", span = 0.2, se = FALSE) +
  geom_smooth(aes(y = P_IAEA_Brisabane),  color = "blue", span = 0.2, se = FALSE) +
  geom_smooth(aes(y = P_Sydney),  color = "orange", span = 0.2, se = FALSE) +
  geom_smooth(aes(y = P_MSWEP),  color = "grey75", span = 0.2, se = FALSE) +
  geom_point(aes(y = P_SISAL),color = "red", se = FALSE) +
  geom_point(aes(y = P_Cobar),   color = "purple",  se = FALSE) +
  geom_point(aes(y = P_IAEA_Brisabane),  color = "blue",se = FALSE) +
  geom_point(aes(y = P_Sydney),  color = "orange",  se = FALSE) +
  geom_point(aes(y = P_MSWEP),  color = "grey75", se = FALSE) +
  scale_y_continuous( expand = c(0, 0)) 
p_SISAL


##### 5.2 d18O -----   
entities_to_delete <- c() #"sin","Brisabane","Cobar","Sydney"
df_SISAL_d18O <- df_d18O[!df_d18O$entity %in% entities_to_delete, ]
df_SISAL_d2H <- df_d2H[!df_d2H$entity %in% entities_to_delete, ]
# convert to date format
df_SISAL_d18O$date <- as.Date(df_SISAL_d18O$date)
df_SISAL_d2H$date <- as.Date(df_SISAL_d2H$date)
# 1.2.1 d18O
p_SISAL_d18O <- ggplot(df_SISAL_d18O, aes(x = date)) +
  scale_x_date(limits = as.Date(c("2010-01-02", "2013-12-31")), expand = c(0, 0), 
               date_labels = "%Y", date_breaks = "1 year" ) +
  geom_point(aes(y = value, color = entity), data = df_SISAL_d18O) + 
  geom_smooth(aes(y = value,color = entity), data = df_SISAL_d18O, 
              span = 0.2, se = FALSE)+
  ylab(expression( "d18O ‰"[paste(VSMOW)]))+
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(face = "bold", size = 13),
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),  # Add panel border
        plot.margin = margin(10, 5, 10, 20, "pt")
        ,legend.position = "none"
  ) + 
  scale_color_manual(values = c("cathedral_cave_drip279" = "springgreen", "cathedral_cave_drip280" = "seagreen3",
                                "cathedral_cave_drip319" = "olivedrab2", "cathedral_cave_drip320" = "olivedrab3",
                                "cathedral_cave_drip322" = "springgreen4", "cathedral_cave_drip330" = "#006400", 
                                "Wellington" = "red","sin"= "grey75", "Sydney"= "orange", "Cobar"= "purple", "Brisabane"= "blue"))

p_SISAL_d18O
