library(shiny)
library(tidyverse)
library(DT)

creatures <- read_csv("creatures.csv") %>%
	mutate(
		`Experience per Creature` = NA_real_
	)

#' get_creature_exp
#'
#' A function that calculates a creatures exp given from the creatures level and the parties level
#'
#' @param creature_level A numeric vector that is the level of the creature
#' @param party_level A numeric scalar that is equal to the average level of the party
#' @return A numeric vector of the same length as the creature_level vector that contains the amount of experince the creatures should grant
get_creature_exp <- function(creature_level, party_level){
	level_diff <- creature_level - party_level
	
	ret <- if_else(level_diff <= -4, 10, 
		   if_else(level_diff <= -3, 15,
		   if_else(level_diff <= -2, 20,
		   if_else(level_diff <= -1, 30,
		   if_else(level_diff <= 0, 40,
		   if_else(level_diff <= 1, 60,
		   if_else(level_diff <= 2, 80,
		   if_else(level_diff <= 3, 120,
		    160))))))))
	return(ret)
}

ui <- fluidPage(
	titlePanel("Pathfinder 2e Balencer"),

	sidebarLayout(
		sidebarPanel(
			numericInput("player_num", "Number of Players", 1),
			numericInput("player_level", "Level of Players", 1),
			textOutput("exp"),
			DTOutput("choosen_table"),
			actionButton("remove_creature", "Remove Creature Selected above from encounter")
		),

		mainPanel(	
			DTOutput("possible_table"),
			numericInput("creature_num", "Number of creature to add", 1),
			actionButton("add_creature", "Add Selected Creature to Encounter")
		)
	)

)

server <- function(input, output, session){
	# passer$pos is the set of creatures that can be choosen from
	# passer$choosen is the set of creatures that have been choosen
	passer <- reactiveValues(
		pos = creatures,
		choosen = tibble(Creature = character(), Level = numeric(), `Experience per Creature` = numeric(), Number = numeric(), `Total Experience` = numeric())
	)

	observeEvent(input$add_creature,{
		if(is.null(input$possible_table_rows_selected)){
			modalDialog(
				p(
					"Please Select a Creature to add"
				)
			)
			return()
		}
		base <- passer$pos[input$possible_table_rows_selected,] %>%
			transmute(
				Creature,
				Level,
				`Experience per Creature`,
				Number = input$creature_num,
				`Total Experience` = Number * `Experience per Creature`
			)
		passer$choosen <- rbind(passer$choosen, base)
	})

	observeEvent(input$remove_creature,{
		if(is.null(input$choosen_table_rows_selected)){
			modalDialog(
				p(
					"Please Select a Creature to remove"
				)
			)
			return()
		}
		passer$choosen <- passer$choosen[-input$choosen_table_rows_selected,]
	})

	observe({
		passer$pos <- creatures %>%
			filter(Level >= input$player_level - 4, Level <= input$player_level + 4) %>%
			mutate(
				`Experience per Creature` = get_creature_exp(Level, input$player_level),
			)
	})

	observe({
		passer$choosen <- passer$choosen %>%
			mutate(
				`Experience per Creature` = get_creature_exp(Level, input$player_level),
				`Total Experience` = Number * `Experience per Creature`
			)
	})

	observeEvent(input$choosen_table_cell_edit,{
		passer$choosen <- editData(passer$choosen, input$choosen_table_cell_edit, "choosen_table")
	})
	
	output$exp <- renderText({
		total_exp <- sum(passer$choosen$`Total Experience`, na.rm = TRUE)
		encounter_difficulty <-if_else( total_exp <= 40 + (input$player_num - 4) * 10, "Trivial",
								if_else(total_exp <= 60 + (input$player_num - 4) * 15, "Low",
								if_else(total_exp <= 80 + (input$player_num - 4) * 20, "Moderate",
								if_else(total_exp <= 120 + (input$player_num - 4) * 30, "Severe",
								if_else(total_exp <= 160 + (input$player_num - 4) * 40, "Extreme", 
								"TPK")))))
		ret <- sprintf("%0.0f Exp; %s", total_exp, encounter_difficulty)
		return(ret)
	})

	output$choosen_table <- renderDT(passer$choosen, selection = "single", editable = "cell")
	output$possible_table <- renderDT(passer$pos, selection = "single")
}

shinyApp(ui, server)
