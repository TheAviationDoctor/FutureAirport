
library(yaml)

url <- "https://github.com/junzis/openap/blob/master/openap/data/dragpolar/a320.yml"

con <- url(description = url)

read_yaml(con)

close(con)

