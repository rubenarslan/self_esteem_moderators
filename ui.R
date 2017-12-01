countries = c("Canada", "United States", "Australia", "New Zealand", "Argentina", 
							"Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Dominican Republic", 
							"Ecuador", "Guatemala", "Mexico", "Peru", "Venezuela", "Denmark", 
							"Finland", "Ireland", "Norway", "Sweden", "United Kingdom", "Austria", 
							"Belgium", "France", "Germany", "Netherlands", "Switzerland", 
							"Greece", "Italy", "Portugal", "Romania", "Spain", "Egypt", "South Africa", 
							"China", "Hong Kong", "India", "Japan", "Korea, Republic of", 
							"Philippines", "Indonesia", "Malaysia", "Singapore", "Thailand", 
							"Israel", "Turkey", "United Arab Emirates")
moderators = c("GDP", "HDI", "GINI", "MAMm", "MAMf", "AFR", "GGI", 
							 "SUF", "POW", "IND", "MAS", "UNC")

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
	
	# Application title
	titlePanel("Self esteem by age, gender and country"),
	
	fluidRow(
		column(2,
					 selectInput("moderator", 
					 						label = "Moderator",
					 						choices = moderators, 
					 						selected = "GDP")
		),
		column(2,
					 checkboxInput("fitted", 
					 							label = "Predicted", 
					 							value = T)
		),
		conditionalPanel(
			condition = "input.fitted == false",
			column(2,
						 checkboxInput("standard_errors", 
						 							label = "Show uncertainty", 
						 							value = F)
			)
		),
		conditionalPanel(
			condition = "input.fitted == true",
			column(2,
						 checkboxInput("moderated_intercepts", 
						 							label = "Show sex differences", 
						 							value = F)
			)
		),
		column(2,
					 checkboxInput("annotate_countries", 
					 			label = "Label countries", 
					 			value = F)
		),
		column(2,
					 actionButton("all_countries", label="(de)select all countries")
		)
	),
	fluidRow(
		column(10, 
					 mainPanel(
					 	plotOutput("agePlot", height = "700px")
					 ,width = 12),
		offset = 1)
	),
	fluidRow(
		column(12,
			checkboxGroupInput("countries", 
												 label = h3("Countries"), 
												 choices = countries,
												 selected = countries,
# 												 selected =  c("United States", "Singapore", "Argentina", "India"),
												 inline = T)
		)
	)
))