#!/usr/bin/env Rscript
# MAWEI – single cross-platform launcher
# Works on macOS, Windows, and Linux.
# Usage:  Rscript launch.R

# Locate this script's directory robustly under Rscript
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grepl("--file=", args)])
if (length(script_path) == 0 || !nzchar(script_path)) {
  # Fallback: assume working directory is the project root
  script_dir <- getwd()
} else {
  script_dir <- dirname(normalizePath(script_path, mustWork = FALSE))
}

html <- file.path(script_dir, "interface", "MAWEI.html")

if (!file.exists(html)) stop("Cannot find interface/MAWEI.html relative to this script.")

message("Opening MAWEI dashboard...")
browseURL(paste0("file:///", normalizePath(html, winslash = "/", mustWork = FALSE)))
