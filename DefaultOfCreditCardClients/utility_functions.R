###############################
## Dataset Summary
###############################
featuresReview <- function(df){
  df_review <- data.frame(names = names(df))
  df_review$class <- map_chr(df, ~class(.x))
  df_review$type <- map_chr(df, ~typeof(.x))
  df_review$n_unique <- map_dbl(df, ~n_distinct(.x))
  df_review$n_NA <- map_dbl(df, ~sum(is.na(.x)))
  df_review$n_0 <- map_dbl(df, ~ifelse(is.numeric(.), sum(.x==0), NA))
  df_review$mean <- map_dbl(df, ~ifelse(is.numeric(.), round(mean(.x, na.rm = TRUE),2), NA ))  
  df_review$min <- map_dbl(df, ~ifelse(is.numeric(.), min(.x, na.rm = TRUE), NA ))
  df_review$median <- map_dbl(df, ~ifelse(is.numeric(.), median(.x, na.rm = TRUE), NA ))
  df_review$max <- map_dbl(df, ~ifelse(is.numeric(.), max(.x, na.rm = TRUE), NA ))  
  df_review
}

#featuresReview(df)

###############################
## Plot categorical/discrete variables
###############################
plotFactor <- function(df, varlist){
  map(varlist, function(x)
    ggplot(df, aes_string(x = as.factor(df[[x]]))) + 
      geom_bar(stat = 'count', fill='steelblue3')+
      theme_light()+
      ggtitle(paste0('Feature: ', x))+
      xlab(x) +
      theme(plot.title = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 0)) 
  )
}


#plotFactor(df_learn_fct, colnames(df_learn_fct))

###############################
## Plot continuous variables
###############################

plotHistogram <- function(df, varlist){
  map(varlist, function(x){
    xmin <- min(df[[x]])
    xmax <- max(df[[x]])
    #binwidth <- 2 * IQR(df[[x]]) / length(df[[x]])^(1/3)
    plt1 <- ggplot(df, aes_string(x = df[[x]])) + geom_histogram(fill = 'steelblue3', breaks = seq(xmin, xmax, length.out = 50) ) +
      theme_light()+
      ggtitle(paste0('Feature: ', x)) +
      xlab(x) +
      xlim(xmin, xmax)
    plt2 <- ggplot(df, aes_string(x = df[[x]])) + geom_boxplot(color = 'steelblue3') +
      theme_light() +
      ylab('')+
      xlim(xmin, xmax)+
      theme(
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y  = element_blank(),
        panel.grid.minor.y  = element_blank(),
        axis.ticks.y = element_blank()
      ) 
    cowplot::plot_grid(plt1, plt2, 
                       ncol = 1, rel_heights = c(2, 1),
                       align = 'v', axis = 'lr')
  }
  )
}
#plotHistogram(df_learn_cont, colnames(df_learn_cont) )


###############################
## Weight of Evidence 
###############################

plotWoe <- function(df, varlist) {
  map(varlist, function(x){
    ordered_bins <- df[[x]] %>% arrange(as.numeric(breaks)) %>% pull(bin)
    df_tmp <- df[[x]] %>% mutate(bin = ordered(bin, levels = ordered_bins ))
    plt1 <- ggplot(df_tmp, aes(x = bin, y = woe)) + geom_col(aes(fill = woe>0)) + 
      coord_flip() +
      theme_light() +
      theme(legend.position= 'none') +
      ylab('WoE') +
      ggtitle(x)
    plt2 <- ggplot(df_tmp, aes(x = bin, y = count)) + geom_col() + 
      coord_flip()+
      theme_light() +
      theme(
        axis.title.y = element_blank()
      )
    cowplot::plot_grid(plt1, plt2, 
                       ncol = 2, rel_heights = c(2, 1),
                       align = 'h', axis = 'lr')
  })}


#plotWoe(df_woebin, names(df_woebin) )


###############################
## Good/Bad distirbution
###############################

plotGoodBadDist <- function(df, y_pred, y_real, set_var){
  plt <- df %>% ggplot(aes(x = y_pred)) + 
    geom_density(aes(fill = factor(get(y_real))), alpha = 0.3) + 
    facet_grid(get(set_var)~.)+
    ggtitle("Good/Bad Distribution")+
    labs(fill = y_real)+
    theme_light()+
    theme(
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1))
  print(plt)
}

#plotGoodBadDist(df_final,'y_pred', 'DEFAULT', 'set' )

###############################
## Success Rate Plot
###############################

plotSuccessRateInBins <- function(df, y_pred_bin, y_real, set_var, y_lab = 'Success Rate'){
  df_tmp <- df_final %>% group_by(!!as.symbol(set_var), !!as.symbol(y_pred_bin))%>% 
    summarise(sr = mean(get(y_real)))
  
  plt <- df_tmp %>%
    ggplot(aes(x = y_pred_bin, y = sr , group = get(set_var)))+ 
    geom_point(aes(color = get(set_var)), size = 2) +
    geom_line(aes(color = get(set_var)), size = 1) +
    ylim(0,1)+
    ylab(y_lab) +
    labs(color = set_var)+
    ggtitle(paste(y_lab,'in Score Bands')) +
    theme_light() +
    theme(
      axis.text.x = element_text(angle = 90),
      plot.title = element_text(hjust = 0.5)
    )
  print(plt)}

#plotSuccessRateInBins(df_final,'y_pred_bin','DEFAULT', 'set', 'Default Rate' )

###############################
## Gain Chart
###############################


plotGainChart <- function(df, y_pred, y_real, set_var){
  require(tidyr)
  
  df_gain <- df %>% select(!!as.symbol(y_real), !!as.symbol(y_pred), !!as.symbol(set_var))
  
  df_gain <- df_gain %>%
    group_by(!!as.symbol(set_var)) %>%
    arrange(desc(!!as.symbol(y_pred))) %>%
    mutate(id = 1,
           gain = cumsum(!!as.symbol(y_real))/sum(!!as.symbol(y_real)),
           perc_pop = cumsum(id)/n()
    )
  df_sr <- df_gain %>% group_by(!!as.symbol(set_var)) %>%
    summarise(success_rate = sum(!!as.symbol(y_real))/n()) %>%
    mutate(zeros = 0,
           ones = 1) %>%
    tidyr::gather(key = 'point', value = 'x', -!!as.symbol(set_var))
  
  df_sr[df_sr$point == 'zeros', 'y'] <- 0
  df_sr[df_sr$point == 'ones', 'y'] <- 1
  df_sr[df_sr$point == 'success_rate', 'y'] <- 1
  
  plt <- df_gain %>% ggplot(aes(x = perc_pop, y = gain, color = get(set_var))) +
    geom_path(size = 0.8)+
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = 'grey20', size = 0.5, linetype = 'dashed' ) +
    geom_line(data = df_sr, aes(x = x, y = y,group = set, color = set), size = 0.5, linetype = 'dashed') +
    theme_light() +
    labs(color = set_var)+
    ylab('Percent of Population') +
    theme(legend.position= 'bottom',
          plot.title = element_text(hjust = 0.5)) +
    ggtitle('Gain Chart')
  
  print(plt)
}

#plotGainChart(df_final, 'y_pred', 'DEFAULT', 'set')

###############################
## ROC
###############################

BuildRocDF_ <- function(y_real, y_pred, n_cutoff) {
  df <- data.frame(y_real,y_pred )
  seq_vec <- seq(0,1, length.out = n_cutoff)
  roc_df <- data.frame( fpr = 1:n_cutoff ,tpr = 1:n_cutoff)
  i=4
  for (i in 1:n_cutoff){
    
    df$y_pred_int <- ifelse(df$y_pred < seq_vec[i], 0, 1)
    TP <- sum(ifelse(df$y_pred_int==1 & df$y_real ==1, 1, 0))
    FP <- sum(ifelse(df$y_pred_int==1 & df$y_real ==0, 1, 0))
    TN <- sum(ifelse(df$y_pred_int==0 & df$y_real ==0, 1, 0))
    FN <- sum(ifelse(df$y_pred_int==0 & df$y_real ==1, 1, 0))
    roc_df$fpr[i] <- FP/(TN+FP)
    roc_df$tpr[i] <- TP/(TP+FN)
    roc_df$cutoff[i] <- i
  }
  return(roc_df)}



plotROC <- function(y_real, y_pred, n_cutoff = 100, group_feat = NULL){
  require(ggrepel)
  if(is.null(group_feat)){
    group_feat <- 1
  }
  df <- data.frame(y_real,y_pred,group_feat )
  groups_vec <- unique(df$group_feat)
  final_df <- data.frame()
  for (item in groups_vec){
    df_temp <- df %>% filter(group_feat == item)
    temp_df_roc <- BuildRocDF_(df_temp$y_real,df_temp$y_pred,n_cutoff )
    temp_df_roc$Set <- item
    final_df <- rbind(final_df, temp_df_roc)
  }
  
  
  p <- ggplot(final_df, aes(x = fpr, y=tpr, color = Set)) + 
    geom_path(size = 0.8) + 
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = 'grey20', linetype = 'dashed') +
    geom_point(data = final_df %>% filter(cutoff%%25==0), aes( x = fpr, y=tpr) ) +
    geom_label_repel(data = final_df %>% filter(cutoff%%25==0), 
                     aes(label = cutoff/100),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50',
                     show.legend = FALSE) +
    theme_light() + theme(plot.title = element_text(hjust = 0.5)) +
    xlab('False Positive Rate') +
    ylab('True Positive Rate') +
    ggtitle('ROC Curve')
  if (length(groups_vec)==1) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "bottom")
  }
  return(p)
}

#plotROC(df_final$DEFAULT,df_final$y_pred, n_cutoff = 100, df_final$set)


