
library(tidyverse)

input <- list(alpha=.05, 
              power=.95, 
              # relative_cost_1=1,
              # relative_cost_2=.95, 
              sample_size=2000)

out_frame <- data.frame(initial_guess = c(0,.01,.05,.1,.25,.5,.75,.9,.95,.99,1))
           
out_frame %>% 
  mutate( percent_report_sig = ((1-initial_guess) * input$alpha) + (initial_guess * input$power), 
          pos_predict_rate =   (initial_guess * input$power) / percent_report_sig,
          significant_per_run = percent_report_sig * input$sample_size, 
          true_positive = pos_predict_rate * significant_per_run, 
          num_wrong = (1-pos_predict_rate) * significant_per_run, 
          percent_report_not_sig =  1-percent_report_sig, 
          neg_predict_rate = 1-((1-input$power) * initial_guess) / percent_report_not_sig, 
          total_accuracy = pos_predict_rate * percent_report_sig + neg_predict_rate * percent_report_not_sig
          ) ->
  out_frame_wide

out_frame_wide %>%
  select(initial_guess, pos_predict_rate, neg_predict_rate, 
         total_accuracy, percent_report_sig) %>%
  gather( var, percent, -initial_guess) ->
  out_frame


ggplot(data=out_frame) +
  geom_line(aes(x=initial_guess, y=percent, color=var))  +
  labs(x = "Initial Guess About Probability that Treatment is Better (Ha=True)", 
       y=''
       ) +
  theme(legend.position="bottom")
