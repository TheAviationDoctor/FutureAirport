# Clear the console
cat("\014")

library("reticulate")

openap <- import("openap")

# py_run_file("scripts/6_takeoff.py")

py_config()