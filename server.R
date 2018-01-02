library(magrittr)
library(tidyverse)

z_0 <- qnorm(0.975)

# p <- 0.5
B <- 1000
# n <- 30

server <- function(input, output) {
  # n <- input$nsamp
  # p <- input$prop
  
  output$distPlot <- renderPlot({
    p_hat_dist <- map_df(seq_len(B), function(i) {
      simulated_dataset <- rbinom(input$nsamp, 
                                  size = 1, 
                                  prob = input$prop)
      p_hat <- mean(simulated_dataset)
      # Compute CIs
      approx_lb <- p_hat - z_0*sqrt(p_hat*(1-p_hat)/input$nsamp)
      approx_ub <- p_hat + z_0*sqrt(p_hat*(1-p_hat)/input$nsamp)
      
      cont_lb <- approx_lb - 0.5/input$nsamp
      cont_ub <- approx_ub + 0.5/input$nsamp
      
      clop_pearson <- binom.test(sum(simulated_dataset), 
                                 input$nsamp)$conf.int
      cp_lb <- clop_pearson[1]
      cp_ub <- clop_pearson[2]
      
      data.frame(
        p_hat,
        approx_lb,
        approx_ub,
        cont_lb,
        cont_ub,
        cp_lb,
        cp_ub
      )
    })
    
    cov_prob <- p_hat_dist %>% 
      summarise(approx_cov = mean(p >= approx_lb & p <= approx_ub),
                cont_cov = mean(p >= cont_lb & p <= cont_ub),
                cp_cov = mean(p >= cp_lb & p <= cp_ub))
    
    max_count <- p_hat_dist %$% 
      max(table(cut(p_hat,
                    seq(min(p_hat),
                        max(p_hat),
                        dist(range(p_hat))/30),
                    include.lowest = TRUE)))
    
    # Pick instance closer to truth, so that CI align with empirical...
    conf_int <- arrange(p_hat_dist, abs(p_hat - mean(p_hat)))[1,,drop=FALSE] %>% 
      select(-p_hat) %>% 
      gather(Type, Value) %>% 
      mutate(Type = stringr::str_replace_all(Type, "_(l|u)b", "")) %>% 
      mutate(y_lb = case_when(
        Type == "approx" ~ 0.0,
        Type == "cont" ~ max_count/3,
        Type == "cp" ~ 2*max_count/3
      ),
      y_ub = case_when(
        Type == "approx" ~ max_count/3,
        Type == "cont" ~ 2*max_count/3,
        Type == "cp" ~ as.numeric(max_count)
      ))
    
    p_hat_dist %>% 
      ggplot(aes(p_hat)) + 
      geom_histogram() +
      geom_ribbon(data = conf_int, 
                  aes(x = Value, fill = Type,
                      ymin = y_lb, ymax = y_ub),
                  alpha = 1/2) + 
      geom_vline(data = conf_int, 
                 aes(xintercept = Value, colour = Type),
                 linetype = 2) + 
      theme(legend.position = 'top') + # coord_cartesian(xlim = c(0,1)) +
      xlab("Distribution of proportions")
  })
}