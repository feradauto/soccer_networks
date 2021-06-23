# Team composition analysis in football

## Description of R files

### Data Exploration
In the file *data_exploration.R* the following data is loaded into R:
* Event data for all games included in the free StatsBomb dataset
* Line-ups of these games

The data is then reduced to include the games of FC Barcelona only. 
After some basic data exploration and visualisation, the amount of minutes played for each line-up is computed, including the number of goals that were scored by that line-up in this time.

### Compute xA
In the file *compute_xa.R*, the expected goals (xG) values and the corresponding expected assists (xA) values are obtained for each pair of players. The xA value is used as a measure of influence from one player to another.

### Compute network
In *compute_network.R*, the necessary values for construction of the network are calculated and aggregated. Additionally, the file includes a few plots visualising the characteristics of the network and the individual nodes/players.

### Regression Table
In the file *regression_table.R*, the pair-wise influences are extended to the full line-ups. The linear model is fit to get the relationship between the two following variables: Goals scored per 90 minutes for a line-up and the score of a line-up (the sum of influences).


