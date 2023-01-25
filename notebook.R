library(tidyverse)
library(plotly)
library(glue)
library(htmlwidgets)

library(readxl)
X81bacias <- read_excel("data/81bacias.xlsx")
pred_data <- read_csv("data/pred_data.csv")

data = pred_data[pred_data$bacia %in% X81bacias$Estação, c("bacia", "ano", "mes", "P1", "P2")]
data[is.na(data)] = 0


data_a = data %>%
    mutate(P = P1 + P2) %>%
    group_by(bacia, ano) %>%
    mutate(media_ano = mean(P),
           "P/media_ano"=P/media_ano) %>% select(bacia,ano,mes,P, media_ano,"P/media_ano") %>%
  ungroup()

data_a$bacia = as.factor(data_a$bacia)



a= left_join(
  data_a %>% 
    filter(ano==1985, mes ==1) %>%
    select(bacia,Start=media_ano)
  ,
  data_a %>% distinct(ano,bacia,media_ano)
  ) %>%
  
  group_by(bacia) %>%
  mutate(index=row_number(),
         media_ref1985 = round(media_ano/Start,4) ,
         radius = media_ref1985,
         theta = 2 * pi * (index-1)/66) %>%
  mutate(
    x = radius * sin(theta),
    y = radius * cos(theta),
    z = ano,
    label = glue("{ano} - {bacia}\n Média|1985 - {media_ref1985*100}%")
    )
         

 

 


t_data <- read_csv("Spiral/climate_viz/data/GLB.Ts+dSST.csv", skip=1, na="***") %>%
  select(year = Year, all_of(month.abb)) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na() %>%
  mutate(month = factor(month, levels = month.abb)) %>%
  arrange(year, month) %>%
  mutate(month_number = as.numeric(month),
         radius = t_diff + 1.5,
         theta = 2 * pi * (month_number-1)/12,
         x = radius * sin(theta),
         y = radius * cos(theta),
         z = year,
         label = glue("{month} {year}\n{t_diff}\u00B0 C"))


input = data_a %>% dplyr::group_by(bacia,ano) %>%
  select(bacia,ano, media_ano)   %>%
  distinct_all() %>%
  mutate(month_number = as.numeric(month),
         radius = t_diff + 1.5,
         theta = 2 * pi * (month_number-1)/12,
         x = radius * sin(theta),
         y = radius * cos(theta),
         z = year,
         label = glue("{month} {year}\n{t_diff}\u00B0 C"))




axx <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axy <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE
)

axz <- list(
  title = ""
)


plot_ly(a,
        x = ~x, y = ~y, z = ~z, text = ~label,
        hoverinfo = "text",
        opacity  = .7,
        type = 'scatter3d',
        mode = 'lines',
        line = list(width = 6, color = ~media_ref1985,
                    cmid = 0.5,# cmin=min(t_data$t_diff), cmax=max(t_data$t_diff),
                    colorscale = list(c(0,'#c4f1ff'),
                                      c(0.15, "#0d00ff"),
                                      c(0.5,'#ff5457'),
                                      c(1,'#c70000'),
                                      c(1.5,'#000000')
                                      ))) %>%
  layout(scene = list(xaxis=axx,
                      yaxis=axy,
                      zaxis=axz))


p <- plot_ly(t_data,
             x = ~x, y = ~y, z = ~z, text = ~label,
             hoverinfo = "text",
             type = 'scatter3d',
             mode = 'lines',
             line = list(width = 10, color = ~t_diff,
                         cmid = 0,# cmin=min(t_data$t_diff), cmax=max(t_data$t_diff),
                         colorscale = list(c(0,'#0000FF'),
                                           c(0.5, "#FFFFFF"),
                                           c(1,'#FF0000')))) %>%
  layout(scene = list(xaxis=axx,
                      yaxis=axy,
                      zaxis=axz))

saveWidget(p, "figures/climate_spiral_plotly.html")

