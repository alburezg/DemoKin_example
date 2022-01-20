# library(tidyverse)
# library(ggrepel)
# library(latex2exp)

lexis1 <- function(){
  r <- 0:50
  mu <- 30
  shift <- 5
  j <- mu + shift
  max_j <- max(r) - shift - mu
  max_i <- max(r) - shift
  line_size <- 1
  
  br <- c(shift, j, max(r))
  labs <- c("t - i", "t - j", "t")
  labs <- paste0("Time\n", labs)
  
  br_y <- c(max_j, max_i)
  labs_y <- c("j", "i")
  labs_y <- paste0("Age\n   ", labs_y)
  
  
  lex <- 
    data.frame(x = r) %>% 
    mutate(
      mother = x - shift
      , child = mother - mu
    ) %>% 
    pivot_longer(-x) 
  
  labs_df <- 
    data.frame(
      x = c(max(r), max(r)) 
      , value = c(max_j, max_i)
      , label = c("n_{i,j,t-i}^*", "e_{i,j,t-i}^*")
    )
  
  labs_lab <- 
    labs_df %>% 
    mutate(
      x = x - 3
      , value = value +2
      , label = c(
        "Child deaths aged j\nto mothers aged i:"
        , "Person-years from mothers\naged i losing a child aged j:"
      )
    )
  
  labs_lat <- 
    labs_lab %>% 
    mutate(
      label = c(
        "h_{i,j,t-i}^* d_{j,t-j}^* l_{i,t-i}^*"
        , "b_{i,j,t-j}^* p_{i, t-i}^*"
      )
    )
  
  
  ggplot(mapping = aes(x = x, y = value)) +
    # lexis lines
    geom_line(
      aes(group = name)
      , data = lex
      , size = line_size
    ) +
    # vertical line
    geom_line(
      data = data.frame(x = shift + mu, value = c(0, mu))
      , linetype = "dashed"
      , size = line_size
    ) +
    # show child death with a cross
    geom_point(
      data = data.frame(x = max(r), value = max_j)
      , shape = 3
      , size = 2
      , stroke = 2
    )+
    # Labels small
    geom_label(
      aes(label = TeX(label, output="character"))
      , data = labs_df
      , nudge_y = 2.5
      , label.size = NA
      , parse = T
    ) +
    # Labels big
    geom_label_repel(
      aes(label = label)
      , nudge_x = -14
      , nudge_y = 1
      , label.size = NA
      , arrow = arrow(length = unit(0.015, "npc"))
      , data = labs_lab %>% 
        bind_rows(
          data.frame(
            x = max(r) - (max(r) - mu) + 4
            , value = (max_i - max_j)
            , label = "Childbirth"
          )
        )
    ) +
    # Labels latex
    geom_label(
      aes(label = TeX(label, output="character"))
      , data = labs_lat
      , nudge_x = -14
      , nudge_y = -4
      , label.size = NA
      , parse = T
    ) +
    scale_x_continuous(breaks = br, labels = labs) +
    scale_y_continuous(breaks = br_y, labels = labs_y, position = "right") +
    coord_equal(xlim = c(min(r), max(r)+3), ylim = range(r), expand = F) +
    theme_bw() +
    theme(
      axis.title.x=element_blank()
      , axis.text.x = element_text(size = 14)
      , axis.title.y=element_blank()
      , axis.text.y = element_text(size = 14)
      , panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
      , plot.margin=grid::unit(c(0,0,0,0), "mm")
    )
}
