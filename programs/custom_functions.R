################################################################################
# This program houses generic custom functions used in various programs through
# out the project.
# Author: Greg Casey, Stephie Fried, Ethan Goode
# Last Edited: 8/9/2022
#
################################################################################

#Shift x-axis
shift_axis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

#negate in operator
`%!in%` = Negate(`%in%`)

#This function allows you to subset data for regression
subset_reg <- function(data, varlist, startsvars) {

  make_df_reg <- data %>%
    select(varlist,
           starts_with(startsvars)
           )
  
  return(make_df_reg)
}

#Custom run function that accepts a second argument to choose which part of the
#code to run
run <- function(filename, section_name) {
  section <- section_name
  source(filename, local = TRUE)
} 

#Custom Theme for plotting
theme_ekg <- function() {
  theme (
    panel.background = element_blank(),
    #legend.position = "none",
    legend.title = element_blank(),
    axis.line = element_line(size=0.5, color = "black")
  ) 
  
}

#Custom mapping theme
theme_mapsEKG <- function() {
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank()
    ,axis.text = element_blank()
    ,axis.ticks = element_blank()
    ,axis.title = element_blank(),
    legend.position = c(0.1,0.25),
    legend.background = element_rect(fill = 'white', color = 'black'),
    legend.key.size = unit(1,'cm'),
    legend.key.width = unit(3,'cm'),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15)
  )
}

#Custom function for getting clustered standard errors
cluster <- function(spec, cluster= "countryname") {
  return(coef(summary(spec, cluster = c(cluster)))[, 2])
}

#Custom function for putting estimates into dataframe
getEstimates <- function(results) {
  local_df <- as.data.frame(results$coefficients) %>%
    rename(pvalue = "Pr(>|t|)") %>%
    mutate(
      isSignif = ifelse(pvalue <= 0.1, 1, 0),
      estSig = isSignif * Estimate
    )
  return(local_df)
}

#Custom summary function: Eases the process of printing only a subset of results.
custom.summary.lm = function (x, digits = max(3L, getOption("digits") - 3L), 
                          symbolic.cor = x$symbolic.cor, 
                          signif.stars = getOption("show.signif.stars"), 
                          my.rows, ...)                     # NOTE NEW my.rows ARGUMENT
{
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  cat(if (!is.null(x$weights) && diff(range(x$weights))) 
    "Weighted ", "Residuals:\n", sep = "")
  if (rdf > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- if (length(dim(resid)) == 2L) 
      structure(apply(t(resid), 1L, quantile), dimnames = list(nam,
                                                               dimnames(resid)[[2L]]))
    else {
      zz <- zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (rdf > 0L) {
    print(resid, digits = digits, ...)
  }
  else {
    cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  }
  else {
    if (nsingular <- df[3L] - df[1L]) 
      cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", 
          sep = "")
    else cat("\nCoefficients:\n")
    coefs <- x$coefficients[my.rows,]                      # SUBSET my.rows
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, 
                                                              colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                 na.print = "NA", ...)
  }
  cat("\nResidual standard error:", format(signif(x$sigma, 
                                                  digits)), "on", rdf, "degrees of freedom")
  cat("\n")
  if (nzchar(mess <- naprint(x$na.action))) 
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(",\tAdjusted R-squared: ", formatC(x$adj.r.squared, 
                                           digits = digits), "\nF-statistic:", formatC(x$fstatistic[1L], 
                                                                                       digits = digits), "on", x$fstatistic[2L], "and", 
        x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], 
                                                          x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE), 
                                                       digits = digits))
    cat("\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      }
      else {
        correl <- format(round(correl, 2), nsmall = 2, 
                         digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}

create_globalTproj <- function(missing_countries, clist) {

  for (country in clist) {
    
    #Fix south korea
    if (country =="Korea, Rep"){
      country = "Korea, Rep."
    }
    
    #Get common name of country
    country2 <- df_sim$kmni_countries[df_sim$countryname2 == country]
    country2 <- country2[[1]]
    
    #Linearly interpolate temperature projections for countries with missing data
    if (country %in% missing_countries) {
      t_2009 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2009)]
      t_2010 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)]
      t_2100 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)] + missing_dtemps$Tchg[(missing_dtemps$countryname2 == country)]
      slope <- (t_2100-t_2010)/(2100-2010)
      temp <- c(t_2010)
      temp_nocc <- c(t_2010)
      dtemp <- c(t_2010-t_2009)
      dtemp_nocc <- c(0)
      for (i in 1:90) {
        year <- 2010 + i
        tempnew <- temp[i]+slope
        temp <- c(temp, tempnew)
        temp_nocc <- c(temp_nocc, t_2010)
        
        dtemp <- c(dtemp,slope)
        dtemp2 <- c(t_2010^2 - t_2009^2)
        for (i in 2:length(temp)) {
          dtemp2 <- c(dtemp2,((temp[i]^2)-(temp[i-1]^2)))
        }
        dtemp_nocc <- c(dtemp_nocc,0)
      }
      
      #Create temp dataframe
      year <- seq(2010,2100,by=1)
      repcountry <- rep(c(country),length(temp))
      temp <- cbind(temp,repcountry, year, temp_nocc)
      temp <- as.data.frame(temp) %>%
        rename(countryname=repcountry) %>%
        mutate(temp = as.numeric(temp),
               year = as.numeric(year),
               temp_nocc = as.numeric(temp_nocc)
               )

      
    } else {
      #Pull for non-missing countries
      factor_name = paste0("factor(countryname)", country2)
      files <- list.files(path = here('data','climProj',country2,'atlas', 'series','CMIP5',
                                      'rcp85','monthly_dump1',country2), pattern = "*.txt")
      
      df_list <- list()
      
      for (i in 1:length(files)) {
        iter <- i
        res <- try(silent = TRUE, {
          df_list[[i]] <- read.table(here('data','climProj',country2,'atlas', 'series','CMIP5',
                                          'rcp85','monthly_dump1',country2,files[i]))
        }
        )
        if(inherits(res, "try-error" )& (!(iter < 40))) {
          next
        }
      }
      
      #All projections
      CMIP5 <- bind_rows(df_list, .id = "projection_type")
      
      #Average temperature projection
      CMIP5avg <- CMIP5 %>%
        mutate(year = V3) %>%
        group_by(year) %>%
        mutate(tavg = mean(V2),
               kmni_countries = country2,
               
        ) %>%
        filter(projection_type == 1) %>%
        ungroup()   %>% 
        filter(year >= 2009) %>% 
        select(year, tavg)
      
      CMIP5avg <- CMIP5avg %>%
        mutate(tproj_2010 = CMIP5avg[(CMIP5avg$year == 2010),]$tavg,
               tdiff_2010 = CMIP5avg$tavg - tproj_2010, 
               #Historical 2010 temperature
               thist_2010 <- df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2010)],
               #Project future temperature
               temp = thist_2010 + tdiff_2010
        )
      
      #Use historical temperature for 2009
      CMIP5avg[(CMIP5avg$year == 2009),]$temp <-df_sim$temp[(df_tfp$countryname == country & df_tfp$year == 2009)]
      
      #Find dtemp and dtemp2
      CMIP5avg <- CMIP5avg %>%
        mutate(dtemp = temp - lag(temp),
               dtemp2 = temp^2 - lag(temp^2),
               countryname=country)%>%
        #Adjust range to go from 2010-2010
        filter(year >=2010)
      
      temp <- CMIP5avg[c("temp","countryname","year")] 
      
      noccvars <- CMIP5avg %>%
        mutate(temp_nocc = temp[1],
               dtemp_nocc = 0,
        )
      temp_nocc <-noccvars$temp_nocc
      temp <- cbind(temp,temp_nocc)
      
    }
    
    if (country == clist[1]) {
      temps <- temp
      
    } else {
      temps <- rbind(temps, temp)

    }
    
  }
  return(temps)
}