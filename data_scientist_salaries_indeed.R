library(RCurl)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(XML)
library(rvest)
library(stringr)
library(data.table)

# link to Indeed.com to do a salary search (national wide) for
# Data Scientist roles
url <- "https://www.indeed.com/salaries/Data-Scientist-Salaries"

# list to hold salary results and compensation results from webpages
sal <- list()
comp <- list()

# j will act as an index for the list
j=1

# for loop to go through all the search result pages on Indeed.com
# each search result page holds about 10 listings of average salaries
# which explains why the counter 'i' that gets attached to the url
# jumps by 10 each time

for (i in seq(0,510,10)) {
        # first page of results doesn't have a counter in the url
        if (i == 0) link <- url else link <- paste0(url,"?start=",i)
        
        # the HTML/CSS is messy, but I found the CSS tags associated with 
        # salary and average compensation
        pg_sal <- read_html(link) %>% html_nodes('.cmp-sal-summary') %>% html_text()
        pg_comp <- read_html(link) %>% html_nodes('.cmp-sal-links') %>% html_text()
        
        # save each table of search results as a list within a list
        sal[j] <- list(pg_sal)
        comp[j] <- list(pg_comp)
        
        # increase our index by 1 each time
        j = j + 1
}


# I couldn't think of a better way to extract the information from the nested list
# the following lines of code were leveraged from StackOverflow.com
# and hacked together by me to get an output -- a dataframe with company and salary

# following two lines turn the nest listed into a dataframe
dfs <- lapply(comp, data.frame, stringsAsFactors = FALSE)
y <- bind_rows(dfs)

# ugly solution to clean up extra text after name of company...
colnames(y) <- "company"
a <- str_split_fixed(y$company,'-',2)
colnames(a) <- c('name','misc')
a <- a[,-2]
a <- str_split_fixed(a,' Jobs',2)

# following two lines turn the nest listed into a dataframe
dfs2 <- lapply(sal, data.frame, stringsAsFactors = FALSE)
z <- bind_rows(dfs2)
colnames(z) <- "salary"

# removes the row of national wide average that appears on every search result page
z2 <- data.frame(z[!grepl("Average", z$salary),])
colnames(z2) <- "salary"

# combines company and offered Data Scientist salary in a 2-column dataframe
final <- data.frame(cbind(a[,1], z2))
colnames(final) <- c('company','salary')

write.csv(final,file="company_salary.csv")

