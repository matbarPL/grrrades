make_barplot_with_bg_tiles <- function(data,
                                       famsup, schoolsup,paid, G3,
                                       title = "Groups of students obtaining different support",
                                       #max_tile_height = 1,
                                       tile_alpha= 0.3,
                                       bar_alpha = 0.8,
                                       bar_width = 0.8,
                                       bar_color = "steelblue") {


  # DATA
  # prepare main df with data
  supp <- data %>% 
    group_by({{famsup}}, {{schoolsup}}, {{paid}}) %>%
    summarise(n = n(),
              grade = mean({{G3}}),
              .groups = "rowwise") %>%
    mutate(cat_name = paste("f", {{famsup}},
                            "s", {{schoolsup}},
                            "p", {{paid}}, 
                            sep = '_')) %>%
    mutate(id = cur_group_id())
  
  # prepare helper df for coloring tiles
  tmp_1 <- supp  %>%
    pivot_longer(c({{famsup}}, {{schoolsup}}, {{paid}}))
  
  ## TILES
  # calculate positions of tiles
  # where the highest tile reaches
  max_n <-  0.5#max(supp$n/sum(supp$n))#max_tile_height#max(supp$n)
  tile_half_height <- (max_n * 1.1) / 6
  # where the middles of the tiles are
  tile_y_middles <- c(tile_half_height, 
                      3 * tile_half_height, 
                      5 * tile_half_height)
  
  tiles_df <- data.frame(x = rep(seq(1, 8, 1), 3),
                         y = rep(tile_y_middles, each = 8)) %>%
    arrange(x) %>%
    mutate(z = tmp_1$value)
  
  # main plot
  supp_plot <- ggplot() +
    geom_tile(data = tiles_df,
              aes(x, y, fill = z),
              colour = "white",
              alpha = tile_alpha) +
    scale_fill_manual(
      breaks = c("no", "yes"),
      values = c("#F05454", "#F5F5F5"),
      name = "Support provided?"
    ) +
    scale_x_discrete() +
    geom_col(
      data = supp,
      aes(x = cat_name, y = n / sum(n)),
      alpha = bar_alpha,
      width = bar_width,
      fill =  bar_color
    ) +
    scale_y_continuous(
      breaks = seq(0, max_n, length.out = 5),
      labels = scales::percent,
      sec.axis = sec_axis(
        ~ .,
        name   = "Kind of support" ,
        breaks = c(tile_half_height, 3 *
                     tile_half_height, 5 * tile_half_height),
        labels = c("Family", "School", "Paid")
      )
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      axis.line.y = element_line()
    ) +
    ylab("Fraction of students") +
    ggtitle(title)+
    geom_text(data = supp, 
              aes(x = cat_name, y = n / sum(n) + 0.02, label = round(grade,2)))
  
  return(supp_plot)
}


get_diag_segment_ends <- function(start = 0,side_length = 20){
  df <- master_data %>% 
    filter(G3_m == G3_p) %>% 
    group_by(sex) %>% 
    summarise(cnt = n()) %>% 
    mutate(perc = cnt/sum(cnt))
  
  diag_length <- side_length*sqrt(2)
  
  #female
  fem_perc <- df[df$sex == "F", "perc"] %>% pull()
  seg_1_end <- diag_length * fem_perc/sqrt(2)
  return(list(end = seg_1_end, fem_perc = fem_perc ))
  
}




