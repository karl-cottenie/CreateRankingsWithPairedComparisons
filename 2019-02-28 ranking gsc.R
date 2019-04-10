##############################
## Ranking method for GSC evaluations
##
## Karl Cottenie
##
## 2019-02-28
##
##############################

library(tidyverse)
library(viridis)
# + scale_color/fill_viridis(discrete = T/F)
theme_set(theme_light())
library(BradleyTerryScalable)
library(igraph)

# Startup ends here

## Comment codes ------
# Coding explanations (#, often after the code, but not exclusively)
# Code organization (## XXXXX -----)
# Justification for a section of code ## XXX
# Dead end analyses because it did not work, or not pursuing this line of inquiry (but leave it in as a trace of it, to potentially solve this issue, or avoid making the same mistake in the future # (>_<)
# Solutions/results/interpretations (#==> XXX)
# Reference to manuscript pieces, figures, results, tables, ... # (*_*)
# TODO items #TODO
# names for data frames (dfName), for lists (lsName), for vectors (vcName) (Thanks Jacqueline May)

## _Sources ------
# https://www.anishathalye.com/2015/03/07/designing-a-better-judging-system/
# https://cvxr.rbind.io/
# https://www.anishathalye.com/2015/11/09/implementing-a-scalable-judging-system/
# https://cran.r-project.org/web/packages/BradleyTerryScalable/index.html
# https://cran.r-project.org/web/packages/BradleyTerry2/vignettes/BradleyTerry.pdf

## _Input example from the website -----
dfComparison = read_table("https://www.anishathalye.com/media/2015/03/07/blueprint-rookie-data.txt", col_types = "ccc", comment = "#", col_names = c("Team1", "Team2", "Team2Better"))

## (>_<) __Create S matrix - Method 1 -----
# rows and colums are the teams, entries count the number of times the row team was
# preferred over the column team

# key point: entry "c0d7 70ef true" and  "70ef c0d7 false" are equivalent
# to make computation easier, for all rows w/ a false comparison, switch Team1 and Team2
# followed by counting the number of combinations per unique pair

# dfS = bind_rows(dfComparison %>%
#                   filter(Team2Better == FALSE) %>% # select the FALSE rows
#                   mutate(Team1S = Team2,           # switch team identity
#                          Team2S = Team1),
#                 dfComparison %>% filter(Team2Better == TRUE) %>%
#                   mutate(Team1S = Team1,
#                          Team2S = Team2)) %>%     # bind it w/ the TRUE rows
#   group_by(Team1S, Team2S) %>%                    # create unique pairs
#   summarise(TotalBetter = n()) %>%                # count the total number
#   spread(Team2S, TotalBetter) %>%                 # create the wide format
#   replace(is.na(.), 0)                            # clean up
# #==> Not a square matrix, what could be wrong?
# 
# "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
# 
# names(dfS[,-1]) %w/o% dfS$Team1S
# dfS$Team1S %w/o% names(dfS[,-1])
#==> certain Teams were always ranked worse/better, so do not show up in column/row!
#==> do not use this method

## __Create S matrix - Method 2 ------

## Step 1: create unique team names
vcNodes = dfComparison %>% select(Team1, Team2) %>%
  unlist() %>%
  unique()
vcNodes = vcNodes[order(vcNodes)]
vcNodes = data_frame(vcNodes, number = 1:length(vcNodes))

## Step 2: replace team names w/ numbers
dfComparisonNumber =  bind_cols(dfComparison,
                                (dfComparison %>% select(Team1) %>%
                                   left_join(vcNodes, by = c("Team1" = "vcNodes"))),
                                (dfComparison %>% select(Team2) %>%
                                   left_join(vcNodes, by = c("Team2" = "vcNodes")))) %>%
  select(number, number1, Team2Better) %>%
  rename(Team1Number = number,
         Team2Number = number1) %>%
  mutate(row = 1:nrow(dfComparison)) %>%
  as.data.frame()

## Step 3: create empty matrix
dfS = array(0, dim = c(nrow(vcNodes), nrow(vcNodes), nrow(dfComparison)))

## Step 4: fill empty array the smart way
for (i in 1:nrow(dfComparisonNumber)) {
  if (dfComparisonNumber[i,3] == "false"){
    dfS[dfComparisonNumber[i,1], dfComparisonNumber[i,2], dfComparisonNumber[i,4]] = 1
  } else {
    dfS[dfComparisonNumber[i,2], dfComparisonNumber[i,1], dfComparisonNumber[i,4]] = 1
  }
}

## Step 5: compute the sum of all the comparisons per team
dfS = apply(dfS, c(1,2), sum)

## __sanity check1 ----
# rowsums should equal frequency of each team in Team1 column and TRUE
# PLUS frequency in Team2 column and FALSE

full_join(dfComparison %>% dplyr::filter(Team2Better == "false") %>%
            group_by(Team1) %>%
            summarise(n1 = n()),
          dfComparison %>% dplyr::filter(Team2Better == "true") %>%
            group_by(Team2) %>%
            summarise(n2 = n()) %>%
            dplyr::rename(Team1 = Team2)) %>%
  # necessary for the full_join
  replace(is.na(.), 0) %>%
  # necessary for computing the total sum
  mutate(n = n1 + n2) %>%
  # necessary to get the same order as the dfS
  arrange(Team1) %>%
  pull(n)

rowSums(dfS)

## __sanity check2: ------
## colsums should equal frequency of each team in Team1 column and FALSE
## PLUS frequency in Team2 column and TRUE

(full_join(dfComparison %>% dplyr::filter(Team2Better == "true") %>%
            group_by(Team1) %>%
            summarise(n1 = n()),
          dfComparison %>% dplyr::filter(Team2Better == "false") %>%
            group_by(Team2) %>%
            summarise(n2 = n()) %>%
            dplyr::rename(Team1 = Team2)) %>%
  # necessary for the full_join
  replace(is.na(.), 0) %>%
  # necessary for computing the total sum
  mutate(n = n1 + n2) %>%
  # necessary to get the same order as the dfS
  arrange(Team1) %>%
  pull(n))

colSums(dfS)

#==> all elements correspond, except for the 2 0's in the colSums(dfS)
# correspond to the teams that always won

## _BradleyTerryScalable approach --------

## __preparing the data from the raw counts ------
dfComparison_4col = codes_to_counts(dfComparison, c("false","true"))
dfComparison_btdata = btdata(dfComparison_4col, return_graph = T)

par(mar = c(0, 0, 0, 0) + 0.1)
plot.igraph(dfComparison_btdata$graph, edge.arrow.size = 0.5)

summary(dfComparison_btdata)

## __Fitting the model -------
Comparisons_fit = btfit(dfComparison_btdata, 1.1)
coef(Comparisons_fit)
#==> very similar to the results reported in the original data set
# https://www.anishathalye.com/media/2015/03/07/blueprint-rookie-results.txt
#==> method is similar enough

## (>_<) Fitting the model from the S matrix
# 
# dfS %>% btdata(return_graph = T) %>% 
#   btfit(1.1) %>% 
#   coef()
#==> identical to the coef(Comparsions_fit) above, so yay!
#==> not use this approach

## __Plotting the model results -----

# create the basic figure
ggRankings = summary(Comparisons_fit, SE = T) %>% .$item_summary %>% 
  # add the total wins and losses bc more intuitive
  left_join(tibble(item = vcNodes$vcNodes,
                   TotalWins = rowSums(dfS),
                   TotalLosses = colSums(dfS))) %>% 
  ggplot(aes(x = fct_reorder(item, desc(estimate)), y = estimate, 
             ymin = estimate-SE, ymax = estimate + SE)) + 
  geom_pointrange() + xlab("Item") + ylab("Rank")

# print the base figure
print(ggRankings) 
#==> find the max and min of the Rank axis, in this case 4 and -10

# Replance the 4 and -10 in code below w/ the top ranks to add total wins and losses
ggRankings +
  # TotalWins at the top of the figure
  geom_text(aes(x = fct_reorder(item, desc(estimate)), y = 4, label = TotalWins)) +
  # TotalLosses at the bottom of the figure
  geom_text(aes(x = fct_reorder(item, desc(estimate)), y = -10, label = TotalLosses))

# TODO convert into shiny app
# https://rmarkdown.rstudio.com/flexdashboard/
