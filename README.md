# Canavan Recipes

A family-and-friends recipe collection powered by R/Shiny. Browse, filter and view your favourite dishes in one place, switch between Metric and Imperial units on the fly, and convert common cooking measurements.

## Features

- **Browse & Search**  
  – Filter by keyword across titles, ingredients or steps  
  – Narrow down by category badges (vegetarian, main, dessert, etc.)  

- **Styled Recipe View**  
  – Colour-coded badges beneath each title  
  – Source attribution in muted text  
  – Ingredients as a bulleted list and steps as a numbered list  

- **Unit Toggle**  
  – Switch between Metric and Imperial when viewing any recipe  

- **Conversions Tab**  
  – Instantly convert between temperature, weight and volume units  

## Getting Started

### Prerequisites

- R (>= 4.0)  
- R packages:  
  ```r
  install.packages(c(
    "shiny", "shinythemes", "readxl",
    "dplyr", "stringr", "DT"
  ))