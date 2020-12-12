# CrossBoruta Dez 2020

multitarget <- function(targets, data = data, maxRuns = 300){
  
  targets <- unique(targets)
  data <- as_tibble(data)
  
  Boruta <- dplyr::select(data, any_of(targets)) %>% 
    map(~ 
          Boruta(
            y = .x, 
            x = data %>% select(-any_of(targets)),
            maxRuns = maxRuns, doTrace = 1
          )
    )
  
  return(Boruta)
}


# harvest trees function

harvest_trees <- function(Boruta, minHits = 0.5, min_medianImp = 1){
  
  if(!require(pacman))install.packages("pacman"); pacman::p_load(tidyverse, Boruta)
  
  summary <- if ("call"%in%names(Boruta)) {
    
    filter(
      attStats(Boruta),decision=="Confirmed" &
        normHits >= minHits & medianImp >= min_medianImp
    ) %>% 
      rownames_to_column(var="predictors") %>% 
      mutate(target = as.character(getConfirmedFormula(Boruta))[[2]]) %>% 
      select(target, predictors, medianImp, normHits) %>% 
      arrange(desc(medianImp))
    
    
  } else {
    
    map_dfr(Boruta, ~ as_tibble(attStats(.x),rownames = "predictors"), .id = "target") %>%
      filter(target!=predictors & decision=="Confirmed" &
               normHits >= minHits & medianImp >= min_medianImp) %>% 
      select(target, predictors, medianImp, normHits) %>% 
      arrange(desc(medianImp))
    
  }
  
  targets <- unique(summary$target)
  predictors <- unique(summary$predictors)
  
  ans <- list(summary=summary,targets=targets,
              predictors=predictors)
  
  return(ans)
}

# plot Boruta function 

plot_Boruta <- function(Boruta, 
                        predictors, drop_targets = NULL,
                        text_size = 12, scale = 1, vline = 0, nr_cols=1){
  
  if(!require(pacman))install.packages("pacman"); 
  pacman::p_load(ggridges, Boruta, dplyr, tidyr, ggplot2)
  
  extract.labels_RFimp <- function(Boruta){
    
    labels <- as_tibble(attStats(Boruta),rownames = "pred") %>% 
      mutate(
        labels = 
          paste0(
            substr(decision,0,1),sprintf(round(normHits,2), fmt = '%#.2f')
          )
      ) %>%
      select(pred, labels)
    
    RFimp <-
      as_tibble(Boruta$ImpHistory) %>%
      pivot_longer(everything(),names_to = "pred", values_to = "RFimp") %>%
      mutate(across(.cols = RFimp, ~ ifelse(sign(.x)==-1,NA,.x)))
    
    ans <- left_join(labels,RFimp, by = "pred") %>% na.omit()
    
    return(ans)
    
  }
  
  
  if ("call"%in%names(Boruta)) {
    
    data <- extract.labels_RFimp(Boruta) %>% 
      dplyr::filter(pred %in% predictors) %>% 
      mutate(labels = paste0(pred," |",labels)) %>% 
      mutate(labels = factor(labels, levels = sort(unique(labels)))) 
    
    data %>% 
      ggplot(aes(x = RFimp, y = fct_relevel(labels, rev), fill = 0.5 - abs(0.5 - stat(ecdf)))) +
      stat_density_ridges(geom = "density_ridges_gradient",
                          calc_ecdf = TRUE, rel_min_height = 0.005, scale = scale) +
      geom_vline(xintercept = vline, color="black", linetype="dashed", size = 0.5) +
      scale_fill_viridis_c(name = "", direction = 1) +
      theme_ridges(font_size = text_size, font_family = "",
                   line_size = 0.5, grid = TRUE, center_axis_labels = FALSE) +
      theme(
        legend.position = "none",
        strip.text.x = element_text(margin = margin(5,0,5,0, "mm")),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
      )
    
  } else {
    
    Boruta %>% map_dfr(~ extract.labels_RFimp(.x), .id = "target") %>% 
      dplyr::filter(pred %in% predictors) %>% 
      dplyr::filter(!(target %in% drop_targets)) %>% 
      dplyr::filter(target!=pred) %>% 
      mutate(Decision = ifelse(substr(labels, 1, 1)=="C","confirmed","not confirmed")) %>% 
      ggplot(aes(x = RFimp, y = fct_relevel(pred,rev))) +
      geom_boxplot(outlier.shape = NA, aes(fill = Decision)) +
      scale_fill_manual(values=c("#E69F00","#D6DBDF")) +
      facet_wrap(~target, ncol = nr_cols) +
      theme_minimal() +
      theme(
        text = element_text(size = text_size),
        legend.position="bottom",
        strip.background = element_rect(
          color="black", fill="gray20", size=1.5, linetype="solid"
        ),
        strip.text = element_text(size = text_size*1.3, color = "white")
      ) +
      labs(y=NULL,x=NULL
           #x="RandomForest-importance"
      )
    
  }
  
}


#### CrossBoruta main function
CrossBoruta<-function(
  X,targets=names(X),
  withTentative=FALSE,withNormImp=FALSE,withSpearman=FALSE,
  doTrace=0,
  maxRuns = 200, # i have added this line
  ...){
  stopifnot(is.data.frame(X))
  stopifnot(all(targets%in%names(X)))
  ordMask<-sapply(X,function(x) is.numeric(x) || is.ordered(x))
  
  
  lapply(which(names(X)%in%targets),function(idx){
    if(doTrace>0) message("Analysing target ",names(X)[idx])
    
    #Executing Boruta & extracting useful outputs
    Boruta::Boruta(X[,-idx],X[,idx],doTrace=doTrace,holdHistory=TRUE,maxRuns = maxRuns,...)->B
    selMask<-with(B,if(withTentative) finalDecision!="Rejected" else finalDecision=="Confirmed")
    if(!any(selMask)) return(NULL)
    selImp<-B$ImpHistory[,c(selMask,rep(FALSE,3)),drop=FALSE]
    xshImp<-B$ImpHistory[,"shadowMax"]
    
    data.frame(
      from=colnames(selImp),
      to=names(X)[idx],
      normHits=colMeans(selImp>xshImp)
    )->ans
    
    #Importance normalisation, if desired
    ans$normImportance<-if(withNormImp){
      NRM_OVER<-5
      (function(getImp=Boruta::getImpRfZ) getImp)(...)(X[,idx,drop=FALSE],X[,idx])->tarImp
      xshAve<-median(tail(xshImp,NRM_OVER),na.rm=TRUE)
      (apply(tail(selImp,NRM_OVER),2,median,na.rm=TRUE)-xshAve)/(tarImp-xshAve)
    }
    
    #Spearman correlation, if desired
    ans$Spearman<-if(withSpearman)
      if(ordMask[idx]){
        sapply(ans$from,function(x) if(ordMask[x]) cor(X[,idx],X[,x],method="spearman",use="pair") else NA)
      }else NA
    
    ans
  })->ans
  do.call(rbind,ans)->ans
  rownames(ans)<-NULL
  class(ans)<-c("CrossBorutaOutput",class(ans))
  ans
}


# pheatmap for directed RF imp
plot_hits <- function(x, minHits = 0.9, remove_empty = TRUE){
  
  if(!require(pacman))install.packages("pacman")
  pacman::p_load(pheatmap, janitor)
  try(dev.off(),silent=TRUE) # pheatmap "forgets" to clear the output before displaying
  
  rows<-sort(unique(x$from))
  cols<-sort(unique(x$to))
  matrix(0,nrow=length(rows),ncol=length(cols))->ans
  dimnames(ans)<-list(from=rows,to=cols)
  ans[cbind(match(x$from,rows),match(x$to,cols))]<-x$normHits
  
  pallete <- c("#CAEAC3","#B9E3B2","#A7DBA1","#93D290","#7EC87E","#68BE70","#50B264",
               "#3BA558","#2D964D","#1E8742","#0E7936","#006A2B","#005723","#00441B")
  
  if(remove_empty==TRUE){
    ans[ans <= minHits] <- NA
    ans <- ans[rowSums(is.na(ans)) != ncol(ans),]
    ans <- ans[,colSums(is.na(ans)) < nrow(ans)]
  }else{
    ans[ans <= minHits] <- NA
  }
  
  pheatmap(ans,
           color = pallete,
           angle_col = "45",cluster_rows=F, cluster_cols=F, na_col = "white")
  
}



### plot_sym_CB

plot_symCB <- function(CB,
                       predictors_list = NULL,
                       remove_nodes = NULL,
                       min_alpha = 0.3,
                       min_metric = 1,
                       max_metric = 100,
                       maxwidth = 3,
                       bend = 0.125,
                       legend_position = "right"
){
  
  if(!require(pacman))install.packages("pacman")
  pacman::p_load(tidyverse, tidygraph, ggraph, graphlayouts)
  
  
  if(is.null(predictors_list)){
    CB <- as_tibble(CB)
  }else{
    CB <- as_tibble(CB) %>% 
      filter(from %in% predictors_list) %>% 
      filter(to %in% predictors_list) 
  }
  
  if(is.null(remove_nodes)){
    CB <- as_tibble(CB)
  }else{
    CB <- CB %>% 
      filter(!from %in% remove_nodes) %>% 
      filter(!to %in% remove_nodes) 
  }
  
  # symmetrise 
  
  CB_graph <- CB %>% mutate(
    normImportance = ifelse(normImportance<0,0,normImportance),
    metric = 2*(normHits-0.5)*pmax(0, normImportance),
    Spearman = replace_na(Spearman, 0),
    pair = paste(pmin(from,to),pmax(from,to), sep = "~")
  ) %>% 
    group_by(pair) %>% 
    summarise(pair_metric = 100*sum(metric), Spearman = first(Spearman), .groups = "keep") %>% 
    filter(between(pair_metric, min_metric, max_metric)) %>% 
    separate(pair, c("from","to"),sep = "~") %>% 
    mutate(
      grouped_Spearman = case_when(
        between(Spearman, -0.1, 0.1) ~ "-0.1 to 0.1",
        between(Spearman, 0.1, 0.3) ~ "0.1 to 0.3",
        between(Spearman, -0.3, -0.1) ~ "-0.3 to -0.1",
        between(Spearman, 0.3, 0.5) ~ "0.3 to 0.5",
        between(Spearman, -0.5, -0.3) ~ "-0.5 to -0.3",
        between(Spearman, 0.5, 0.7) ~ "0.5 to 0.7",
        between(Spearman, -0.7, -0.5) ~ "-0.7 to -0.5",
        between(Spearman, 0.7, 1) ~ "0.7 to 1",
        between(Spearman, -1, -0.7) ~ "-1 to -0.7",
        TRUE ~ as.character(Spearman)
      ),
      Spearman = as.character(Spearman)
  ) %>%
  tidygraph::as_tbl_graph(directed = FALSE) 
  
  CB_graph %>% ggraph::ggraph(layout = "stress") +
    geom_node_point() +
    geom_edge_arc(aes(
      width = pair_metric,
      alpha = pair_metric,
      color = fct_reorder(grouped_Spearman, as.numeric(Spearman), .desc = TRUE)),
      strength = bend,
      check_overlap = TRUE
    ) +
    scale_edge_width(range = c(0,maxwidth)) +
    scale_edge_alpha(range = c(min_alpha,1)) +
    scale_edge_color_manual(values= c(
      "0.7 to 1" = "#2166AC","-1 to -0.7" = "#B2182B",
      "0.5 to 0.7" = "#4393C3", "-0.7 to -0.5" = "#D6604D",
      "0.3 to 0.5" = "#92C5DE", "-0.5 to -0.3" = "#F4A582",
      "0.1 to 0.3" = "#D1E5F0", "-0.3 to -0.1" = "#FDDBC7",
      "-0.1 to 0.1" = "#000000")) +
    geom_node_text(aes(label = name), repel = TRUE)+
    theme_graph()+
    theme(legend.position = legend_position,  legend.title = element_blank())+
    theme(legend.key.width=unit(1,"cm"))
  
}
