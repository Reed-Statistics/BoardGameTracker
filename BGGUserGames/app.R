# Load libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(htmltools)
library(viridis)
library(wordcloud)
library(tidytext)
library(kableExtra)
library(shinycssloaders)

options(spinner.color = "white", spinner.type = 4) 

# User interface
ui <- fluidPage(
    tags$head(
        tags$style(HTML(paste("@import url('//fonts.googleapis.com/css?family=Passion+One:400');",
                              "@import url('//fonts.googleapis.com/css?family=Quicksand:300');", sep = "")))),
    #                a {color: #c71028};"))),
    setBackgroundColor("#3f3a60"),
    titlePanel(h1("What's in a Board Game Geek Collection?", 
                   style = "color: white; font-family: 'Passion One'; font-size: 56px; padding: 10px;")),
    sidebarLayout(
        sidebarPanel(
            img(src = paste("https://images.squarespace-cdn.com/content/5902292fd482e9284cf47b8d/",
                            "1567633740689-7KJJU4XTDX297H0GGV49/BGG.jpeg?content-type=image%2Fjpeg", sep = ""), 
                width = 150, 
                style = "display: block; margin-left: auto; margin-right: auto;"),
            h6(" "),
            textInput("username", "Enter username here (case-sensitive): ",
                      value = ""),
            tags$i(h6("(Ex: try 'Sci_Karate'!)")),
            htmlOutput("loading"),
            width = 3
        ),
        mainPanel(
            div(h2(paste("Enter a username on the left as it's displayed in BGG, ",
                         "and learn more about that user's board game collection.", sep = "")),
                style = "color: white; padding: 10px; font-family: 'Quicksand'"),
            htmlOutput("gamesList", 
                       style = paste("color: white; font-size: 30px; font-family: ",
                                     "'Helvetica Neue', Helvetica, sans-serif; padding: 15px; ", sep = "")),
            uiOutput("imageGrid") %>%
                withSpinner(),
            uiOutput("stats") %>%
                withSpinner(color = "#3f3a60"),
            plotOutput("wordcloud") %>%
                withSpinner(color = "#3f3a60"),
            plotOutput("commentWordcloud") %>%
                withSpinner(color = "#3f3a60")
        )
    )
)

# Server function
server <- function(input, output){
    user_games <- reactive({
        user <- input$username 
        user_request <- paste("https://www.boardgamegeek.com/xmlapi2/collection?username=", 
                              user, 
                              "&subtype=boardgame&excludesubtype=boardgameexpansion&own=1", sep = "")
        user_data <- GET(user_request) %>%
            xmlParse() %>%
            xmlToList() 
    })
    games_list <- reactive({
        if (is.null(user_games()$error)) {
            game_id <- lapply(user_games()[1:(length(user_games()) - 1)], 
                              function(x) as.character(x[[".attrs"]][["objectid"]])) %>%
                unlist()
            user_games_request <- "https://www.boardgamegeek.com/xmlapi2/thing?id="
            for (id in seq_along(game_id)) {
                user_games_request <- paste(user_games_request, game_id[[id]], ",", sep = "")
            }
            user_games_request <- str_sub(user_games_request, end = -2)
            user_games_request <- paste(user_games_request, "&stats=1&comments=1&ratingcomments=1", sep = "")
            user_games <- GET(user_games_request) %>%
                xmlParse() %>%
                xmlToList()
        } else {
            error <- "Invalid"
        }
    })
    games <- reactive({
        if (games_list() != "Invalid") {
            games_df <- list()
            for (i in seq_along(games_list())) {
                vars <- unique(names(games_list()[[i]]))
                vars <- vars[! vars %in% c("link", ".attrs", "poll", "comments")]
                games_df[[i]] <- list()
                for (j in seq_along(vars)) {
                    if (vars[[j]] == "name") {
                        games_df[[i]][["name"]] <- games_list()[[i]][["name"]][["value"]]
                    } else if (vars[[j]] == "statistics") {
                        use <- c("usersrated", "average", "bayesaverage", "stddev", "owned", 
                                 "trading", "wanting", "wishing", "numcomments", "numweights", "averageweight")
                        for (stat in use) {
                            games_df[[i]][[stat]] <- games_list()[[i]][["statistics"]][["ratings"]][[stat]]
                        }
                    } else {
                        games_df[[i]][[vars[[j]]]] <- games_list()[[i]][[vars[[j]]]]
                    }
                }
                rownames <- names(games_df[[i]])
                games <- unlist(games_df[[i]])
                games_df[[i]] <- data.frame(matrix(games, nrow = length(games), byrow = T), row.names = rownames)
                name <- as.character(games_df[[i]]["name", ])
                games_df[[name]] <- duplicate(games_df[[i]])
                games_df[[i]] <- NULL
            }
            game_names <- sapply(games_df, function(x) as.character(x["name", 1]))
            all_games <- list.cbind(games_df)
            colnames(all_games) <- game_names
            all_games_tidy <- all_games[1:(ncol(all_games) - 1)] %>%
                t() %>%
                as.data.frame() %>%
                select(3, 1:2, 4:ncol(.)) %>%
                mutate_at(vars(1:11), as.character) %>%
                mutate_at(vars(5:11), as.numeric)
            rownames(all_games_tidy) <- NULL
            return(all_games_tidy)
        }
    })
    comments <- reactive({
        if (games_list() != "Invalid") {
            games_comments <- tibble(game = c(NA), 
                                     rating = c(NA), 
                                     comment = c(NA))
            curr_row <- 0
            for (i in seq_along(games_list())) {
                if (class(games_list()[[i]]) != "character") {
                    for (j in seq_along(games_list()[[i]][["comments"]])) {
                        curr_row <- max(curr_row+1, i+(j-1))
                        if (length(games_list()[[i]][["comments"]][[j]]) > 2) {
                            games_comments[curr_row, "game"] <- games_list()[[i]][["name"]][["value"]]
                            if (games_list()[[i]][["comments"]][[j]][["rating"]] != "N/A") {
                                games_comments[curr_row, "rating"] <- games_list()[[i]][["comments"]][[j]][["rating"]]
                            }
                            games_comments[curr_row, "comment"] <- games_list()[[i]][["comments"]][[j]][["value"]]
                        }
                    }
                }
            }
            return(games_comments)
        }
    })
    output$gamesList <- renderUI({
        if (input$username == "") {
            HTML("")
        } else if (games_list() != "Invalid") {
            games_names <- as.character(games()$name)
            #games_concat <- paste(games_names, sep = "", collapse = "<br/>")
            p("This user owns ", span(length(games_names), style = "font-weight: bold"), " games:", 
                style = "font-family: 'Quicksand'; font-size: 24px") #<br/>", games_concat))
        }
    })
    output$imageGrid <- renderUI({
        if (input$username != "" & games_list() != "Invalid") {
            thumbnails <- as.character(games()$thumbnail)
            fluidRow(
                lapply(thumbnails, function(x) {
                    column(2, img(src = x, height = "70", 
                                  style = "display: block; margin-left: auto; margin-right: auto;"))
                })
            )
        }
    })
    output$stats <- renderUI({
        if (input$username != "" & games_list() != "Invalid") {
            summ <- games() %>%
                summarise(avgRating = mean(as.numeric(as.character(average)), na.rm = TRUE),
                          avgWeight = mean(as.numeric(as.character(averageweight)), na.rm = TRUE))
            avgRating <- round(summ$avgRating[[1]], digits = 2)
            avgWeight <- round(summ$avgWeight[[1]], digits = 2)
            div(HTML(paste("<br/>Average rating: <b>", avgRating, "</b><br/>",
                           "Average weight score: <b>", avgWeight, "</b><br/><br/>", sep = "")),
                style = "color: white; font-family: 'Quicksand'; font-size: 24px")
        }
    })
    output$wordcloud <- renderPlot({
        if (input$username != "" & games_list() != "Invalid") {
            data("stop_words")
            words_to_exclude <- c("game", "player", "players", "played", "play", "playing", "score",
                                  "win", "lose", "hand", "round", "wins", "set", "choose", "takes",
                                  "quot", "mdash", "ndash", "eacute", "rsquo", "amp")
            # "card", "cards", "deck", "decks", "board",
            palette <- viridis(10, direction = -1)
            game_desc_words <- games() %>%
                unnest_tokens(output = word, input = description,
                              token = "words")
            game_desc <- game_desc_words %>%
                anti_join(stop_words, by = "word") %>%
                filter(is.na(as.numeric(word)),
                       !(word %in% words_to_exclude)) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                top_n(20)
            game_desc %>%
                with(wordcloud(word, n, colors = palette,
                               random.order = FALSE,
                               scale = c(8, 1)))
        }
    })
    output$commentWordcloud <- renderPlot({
        if (input$username != "" & games_list() != "Invalid") {
            data("stop_words")
            words_to_exclude <- c("game", "player", "players", "played", "play", "playing", "score",
                                  "win", "lose", "hand", "round", "wins", "set", "choose", "takes",
                                  "quot", "mdash", "ndash", "eacute", "rsquo", "amp",
                                  "ð", "ñ", "à")
            # "card", "cards", "deck", "decks", "board",
            palette <- viridis(10, direction = -1)
            game_desc_words <- comments() %>%
                unnest_tokens(output = word, input = comment,
                              token = "words")
            game_desc <- game_desc_words %>%
                anti_join(stop_words, by = "word") %>%
                filter(is.na(as.numeric(word)),
                       !(word %in% words_to_exclude)) %>%
                count(word) %>%
                arrange(desc(n)) %>%
                top_n(20)
            game_desc %>%
                with(wordcloud(word, n, colors = palette,
                               random.order = FALSE,
                               scale = c(8, 1)))
        }
    })
    output$loading <- renderUI({
        if (input$username != "") {
            if (!(is.null(user_games()$error))) {
                tags$i(h5("Not a valid username", style = "color: red"))
            }
        }
    })
}

# Creates app
shinyApp(ui = ui, server = server)