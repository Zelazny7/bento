#' @include opts.R

#setOldClass("opts")
#' @export
Discretizer <- setRefClass("Discretizer", fields=list(
  opts = "opts",
  tbls = "list",
  vals = "numeric",
  same = "logical",
  breaks = "numeric" 
), methods = list(
  
  initialize = function(opts) {
    .self$opts <<- opts
    .self$breaks <- c(-Inf, Inf)
  },
  
  tabulate_ = function(x, y) {
    stop("Must Implement")
  },
  
  fit = function(x, y) {
    #stop("Must Implement")
    
    tabulate_(x, y) ## create the crosstab used for discretizing
    .self$break_and_heal_()
    .self$generate_breaks_()
    
  },
  
  metric_ = function(z) {
    "Metric calculated on a summary table of x and y"
    stop("Must Implement")
  },
  
  metric_list_of_tables_ = function(l) {
    "Metric calculated on a list of tables"
    stop("Must Implement")
  },
  
  calculate_metric_ = function() {
    
    metric_list_of_tables_(tbls)
    
  },
  
  break_ = function() {
    #print("Break")
    
    metric_current <- calculate_metric_()
    
    best <- list(metric=-Inf, tbls=NULL, i=NULL)
    for (i in seq_along(tbls)) {
      
      k <- find_best_split_index_(tbls[[i]])
      
      ## If a valid split was found
      if (k > 0) {
        
        ## if k is negative, ignore the split, otherwise evaluate the metric
        zs <- split_at_index_(tbls[[i]], k)
        metric <- metric_list_of_tables_(c(zs, tbls[-i]))
        
        if ((metric - metric_current) > opts$metric.inc.min && metric > best$metric) {
          best <- list(metric=metric, tbls=zs, i=i)
        }
      }
    }
    
    ## check if best is null or not
    if (is.null(best$tbls)) {
      same <<- TRUE ## no change in the table structure
      return(invisible())
    } else { ## There was a split that matched criteria
      same <<- FALSE
      tbls <<- append(tbls, best$tbls, best$i)[-best$i]
    }
  },
  
  heal_ = function(metric.dec.max = .self$opts$metric.dec.max) {
    #print("Heal")
    
    current_metric <- calculate_metric_()
    
    for (i in seq.int(length(tbls) - 1)) {
      
      if (identical(length(tbls), 1L)) return(invisible()) ## exit if only one bin
      
      tmp <- tbls
      tmp[[i]] <- do.call(rbind, tmp[i:(i+1)]) ## temporarily merge adjacent bins
      tmp[[i + 1]] <- NULL
      
      metric <- metric_list_of_tables_(tmp)
      
      if (current_metric - metric < metric.dec.max) { ## merge them
        tbls <<- tmp
        same <<- FALSE
        i <- 1 ## start over
        current <- metric
      }
    }
  },
  
  break_and_heal_ = function(metric.dec.max=.self$opts$metric.dec.max) {
    
    #print(metric.dec.max)
    
    while(!same) { ## no longer changing
      old <- tbls
      .self$break_()
      .self$heal_(metric.dec.max)
      
      if (identical(old, tbls)) {
        same <<- TRUE
      }
    }
    
    if (length(tbls) <= opts$max.bin) {
      return(invisible())
      
    } else {
      same <<- FALSE
      break_and_heal_(metric.dec.max = metric.dec.max * opts$expansion)
    }
  },
  
  find_best_split_index_ = function(tbl) {
    metric <- sapply(seq.int(nrow(tbl) - 1), .self$evaluate_split_, tbl)
    if (any(metric > 0)) which.max(metric) else - 1
  },
  
  evaluate_split_ = function(i, tbl) {
    stop("Must Implement")
  },
  
  generate_breaks_ = function() {
    if (identical(length(tbls), 1L)) return(c(-Inf, Inf))
    v <- vals[cumsum(sapply(head(tbls, -1), nrow)) + 1]
    breaks <<- c(-Inf, v, Inf)
  }
  
), contains = "VIRTUAL")