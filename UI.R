ui <- fluidPage(
  includeCSS("www/Style.css"),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Segoe+UI:wght@300;400;600;700&display=swap"
    )
  ),
  
  div(class = "banner-container",
      div(class = "banner",
          div(class = "banner-text-content", 
              h1("City of Greenvale",
                 style = "margin: 0px 0px 0px 30px; padding: 0; line-height: 1.2;"),
              h2("Pollution Reduction Planner using Simplex Method!",
                 style = "margin: 0px 0px 0px 50px; padding: 0; line-height: 1.2;")
          )
      ),
      
      div(class = "right-aligned-button",
          actionButton("infoButton", 
                       "About Us", 
                       icon = icon("info-circle"),
                       class = "frutiger-aero-button medium",
                       style = "margin: 5px 0px 10px 30px; --hue: 240; font-size: 14px;")
      )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      h4("Select Projects"),
      actionButton("selectAll", "Check All",
                   class = "frutiger-aero-button medium btn-block",
                   style = "margin-bottom: 10px; font-size: 14px;"),
      actionButton("reset", "Reset",
                   class = "frutiger-aero-button medium btn-block",
                   style = "margin-bottom: 20px; --hue: 190; font-size: 14px;"),
      
      hr(),
      div(class = "project-scroll-box", uiOutput("projectCheckboxes")),
      hr(),
      
      actionButton("solve", "Let's Optimize!",
                   class = "frutiger-aero-button medium btn-block",
                   style = "margin-bottom: 10px; --hue: 180; font-size: 14px;",
                   icon = icon("calculator")),
      
      hr(),
      div(class = "user-info-container",
          style = "background: linear-gradient(135deg, rgba(255,255,255,0.5) 0%, rgba(240,248,255,0.4) 100%);
                   backdrop-filter: blur(20px);
                   border-radius: 12px;
                   padding: 5px;
                   margin-top: 10px;
                   box-shadow: 0 4px 12px rgba(0,0,0,0.1), 0 1px 0 rgba(255,255,255,0.6) inset;
                   border: 1px solid rgba(255,255,255,0.6);",
          
          h4("Behind the Scenes", style = "margin: 15px 5px 15px 5px; font-weight: bold;"),
          hr(style = "margin: 5px 0;"),
          
          div(style = "display: flex; align-items: center; gap: 10px; margin: 5px 20px 5px 5px;",
              tags$img(
                src = "smiski.png",
                style = "width: 50px; height: 50px; border-radius: 50%;"
              ),
              span("Hi There! I'm Jodi.", style = "font-size: 16px; margin: 3px; font-family: 'Conthrax'; font-size: 14px; color: #1948a6;")
          ),
          
          p(
            " I am a Sophomore Computer Science Student at the University of the Philippines - Los Banos.",
            br(),
            " While the work is tedious, I enjoy bringing niche and quirky designs into real life!",
            style = "font-size: 14px; margin: 10px 10px 15px 5px;"
          )
          
      )
    ),
    
    mainPanel(
      width = 8,
      
      h3("Your Input"),
      verbatimTextOutput("selectedProjects"),
      
      conditionalPanel(
        condition = "output.hasSolution",
        
        hr(),
        uiOutput("resultHeader"),
        
        conditionalPanel(
          condition = "output.isOptimal",
          
          hr(),
          h3("Basic Solution"),
          div(class = "tableau-box", DTOutput("BasicSolution")),
          
          hr(),
          h3("Project Breakdown"),
          DTOutput("solutionTable"),
          
          hr(),
          h3("Constraint Verification"),
          DTOutput("constraintTable")
        ),
        
        hr(),
        h3("Tableau and Iteration Details"),
        
        div(
          checkboxInput("showInitial", "Show Initial Tableau", FALSE),
          checkboxInput("showIterations", "Show All Iterations", FALSE),
          checkboxInput("showFinal", "Show Final Tableau", TRUE)
        ),
        
        conditionalPanel(
          condition = "input.showIterations",
          h4("Iteration Details"),
          uiOutput("iterationDetails")
        ),
        
        conditionalPanel(
          condition = "input.showInitial",
          h4("Initial Tableau"),
          div(class = "tableau-box", DTOutput("initialTableau"))
        ),
        
        conditionalPanel(
          condition = "input.showFinal",
          h4("Final Tableau"),
          div(class = "tableau-box", DTOutput("finalTableau"))
        )
      )
    )
  )
)
