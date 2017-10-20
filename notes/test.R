#!/usr/bin/env Rscript

args = commandArgs(trailing = TRUE)

name = args[1]

msg = paste0("Hello there ", name)

print(msg)
