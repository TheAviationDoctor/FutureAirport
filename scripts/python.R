# Clear the console
cat("\014")

library("reticulate")

source_python("scripts/6_thrust.py")

py_thrust(0, 0)