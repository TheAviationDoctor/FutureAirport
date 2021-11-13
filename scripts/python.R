# Clear the console
cat("\014")

library("reticulate")

source_python("scripts/6_thrust.py")

py_thrust(0, 0)

source_python("scripts/6_drag.py")

py_drag(72000, 150, 0, 15, 0, TRUE)