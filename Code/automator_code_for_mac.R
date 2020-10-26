#Scheduler for our data runs
library(cronR)

#Running things from JSON code file
drive <- "/Users/denisebradford/Documents/Data-Sources/Code/json_code.R"
cmd <- cron_rscript(drive)
cron_add(command = cmd, frequency = 'monthly', 
         id = 'update_scc_json_code', 
         description = 'Job will update monthly with new data from JSON code')


#Running things from html code file
drive2 <- "/Users/denisebradford/Documents/Data-Sources/Code/html_code.R"
cmd2 <- cron_rscript(drive2)
cron_add(command = cmd2, frequency = 'monthly', 
         id = 'update_scc_html_code', 
         description = 'Job will update monthly with new data from HTML code')

