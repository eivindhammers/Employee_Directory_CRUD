
#################Server############################################

server <- function(input, output, session) {
  
##################Data############################################
  
participants_df <- reactive({
    
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button
  input$edit_button2

  participants_df <- dbReadTable(pool, "participants_df")
  return(participants_df)
})
 
#Filtered DF
 
participants_df_filtered <- reactive({
  
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button
  input$edit_button2

  #needed to filter table again for reactivity
  participants_filtered <- dbReadTable(pool, "participants_df")
  
  if (input$location_filter != "all") {
    participants_filtered <- participants_filtered %>% 
      filter(grepl(input$location_filter, location, ignore.case = TRUE))
  }
  
  return(participants_filtered)
  
})

participants_df_table <- reactive({
  
  #make reactive to
  input$submit
  input$yes_button
  input$edit_button2
  input$edit_button
  
  participants_filtered <- dbReadTable(pool, "participants_df")

  if (input$location_filter != "all") {
    participants_filtered <- participants_filtered %>% 
      filter(grepl(input$location_filter, location, ignore.case = TRUE))
  }
  
  participants_df_table <- participants_filtered %>% 
    select(picture, image_ext, title, first_name, last_name, job_title, location, email, date, row_id)
  
  participants_df_table$participant <- paste0( "<b>", participants_df_table$title, " ",
                                         participants_df_table$first_name, " ",
                                         participants_df_table$last_name, "</b><br>",
                                         '<div class="table-sub-title">',participants_df_table$job_title, "</div>")
  participants_df_table$email <- str_replace(participants_df_table$email, participants_df_table$email, sprintf('<a href="mailto:%s">%s</a>',  participants_df_table$email, participants_df_table$email))
  participants_df_table$view <- paste("<button id=\"info_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-primary btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;info_button&quot;,  Math.random())\"><i class=\"fa fa-address-card fa-2x\"></i></button>")
  participants_df_table$actions <-paste("<button id=\"edit_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-link btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;edit_button&quot;,  Math.random())\"><i class=\"fa fa-edit fa-2x\"></i></button>",
                                     "<button id=\"delete_button\" 
                                              type=\"button\" 
                                              class=\"btn btn-link btn-sm\"
                                              onclick=\"Shiny.onInputChange(&quot;delete_button&quot;,  Math.random())\"><i class=\"fa fa-times fa-2x\"></i></button>") 
  
  #Replace picture with anonymous picture when No
  participants_df_table$picture[participants_df_table$picture == "No"] <- paste0('<img class="profile-table-img" src="no_picture.png"></img>')
  
  #Replace picture with profile picture when Yes
  participants_df_table$picture <- str_replace(participants_df_table$picture, "Yes", sprintf('<img class="profile-table-img" src="profile_images/%s_%s_%s_%s.%s"></img>', 
                                                                                       paste(participants_df_table$date), 
                                                                                       paste(participants_df_table$first_name), 
                                                                                       paste(participants_df_table$last_name),
                                                                                       paste(participants_df_table$row_id),
                                                                                       paste(participants_df_table$image_ext)
  ))
  participants_df_table <- participants_df_table %>% 
    select(picture, participant, view, location, email, actions, row_id)
  return(participants_df_table)
})


download_df <- reactive({
  
  download_df <- dbReadTable(pool, "participants_df")
  download_df <- download_df %>% select(last_name, first_name, title, job_title, location)
  
  return(download_df)
  
})

###################Data Filters###########################

output$table_filters <- renderUI({
  
div(class="filter-container",
      dropdownButton(
        selectInput("location_filter", label= "Location", multiple = FALSE, choices = c("all", unique(sort(tolower(participants_df()$location))))),
        circle = TRUE, status = "info", 
        icon=icon("filter"), width = "200px", size = "sm",
        tooltip = tooltipOptions(title = "Click to see filters.")))
  
})


#####Download data buttons################################################

output$download_button <- downloadHandler(
  filename = function() {"employee_directory.xlsx"},
  content = function(file){write.xlsx(download_df(), file)
  
})

output$excel_download <- downloadHandler(
  filename = function() {"employee_directory_full.xlsx"},
  content = function(file){write.xlsx(participants_df(), file)
    
  })

output$image_download <- downloadHandler(
  filename <- function() {
    paste("profile_images", "tar", sep=".")
  },
  
  content <- function(file) {
    tar(file, "www/profile_images/")
  }
)

#################################################Login#################################################################


observeEvent(input$login_link,{

  showModal(
    modalDialog(div(class="login-modal", div(class="login-header", h4("Enter Login Details"))),
                textInput("userInp", "Login"),
                passwordInput("pwInp", "Password"),
                p(id="errorMessage", html("&nbsp;")),
                actionButton("butLogin", "login", class = 'btn action-button btn-success', icon = icon('sign-in-alt')),
                div(class="login-footer", actionButton("dismiss", "Dismiss")),
                size = "s",
                easyClose = TRUE,
                footer = NULL))
})


user <- reactiveValues(login = FALSE, name = NULL, header = NULL)

observeEvent(input$butLogin,{
  
  req(input$userInp, input$pwInp)
  if(input$userInp != "admin"){
    user$login <- FALSE
    user$header <- "Incorrect information."
    html("errorMessage", paste0(user$header))
  } else {
    if (input$pwInp == "SHINY2020") {  ## match
      user$login <- TRUE
      user$name <- input$userInp
      removeModal()
    } else {  ## no match
      user$login <- FALSE
      user$header <- 'Incorrect information.'
      html("errorMessage", paste0(user$header))
    }
  }
})

output$download <- renderUI({
  
  if(user$login == FALSE){
  div(class="download-container",
      tableDownloadbutton("download_button", label=NULL),
      bsTooltip(id = "download_button", title = "Download table as Excel file.", 
                placement = "left", trigger = "hover"),
      actionLink("login_link", "Login"))} 
  else{
    div(class="download-container",
        imageDownloadbutton("image_download", label=NULL),
        bsTooltip(id = "image_download", title = "Download images as .tar file.", 
                  placement = "left", trigger = "hover"),
        excelDownloadbutton("excel_download", label=NULL),
        bsTooltip(id = "excel_download", title = "Download backup of database as Excel file.", 
                  placement = "left", trigger = "hover"),
        downloadButton("download_button", label=NULL, icon = icon("download")),
        bsTooltip(id = "download_button", title = "Download table as Excel file.", 
                  placement = "left", trigger = "hover"),
        actionLink("login_link", "Login"))} 
      
})

observeEvent(input$dismiss,{
  removeModal()
  
})

#################################################Profile Modal################################

observeEvent(input$info_button, {
  
  sel_row <- input$participants_table_row_last_clicked
  row_id <- participants_df_filtered()[sel_row, "row_id"]
  table <- dbReadTable(pool, "participants_df")
  
  #Profile values
  
  title <- table[table$row_id == row_id, "title"]
  profile_date <- table[table$row_id == row_id, "date"]
  image_ext <- table[table$row_id == row_id, "image_ext"]
  first_name <- table[table$row_id == row_id, "first_name"]
  last_name <- table[table$row_id == row_id, "last_name"]
  
  picture <- table[table$row_id == row_id, "picture"]
  profile_image <- if(picture == "No"){HTML('<img class="profile-img" src="no_picture.png" height="120"></img>')} else{ 
  profile_image <- if(picture == "Yes"){HTML(sprintf('<img class="profile-img" src="profile_images/%s_%s_%s_%s.%s" height="120"></img>', 
                                                           paste(profile_date),  
                                                           paste(first_name), 
                                                           paste(last_name),
                                                           paste(row_id),
                                                           paste(image_ext)))}}

  job_title <- table[table$row_id == row_id, "job_title"]
  location <- table[table$row_id == row_id, "location"]
  email <- table[table$row_id == row_id, "email"]

  note <- table[table$row_id == row_id, "note"]

  showModal(
    modalDialog(id="profile_form",
      title = NULL,
      footer = NULL,
      easyClose = TRUE,
      div(
          fluidPage(
            htmlTemplate(
              filename = "www/profile_form.html",
              profile_image = profile_image,
              title = title,
              first_name = paste(" ", first_name, " "),
              last_name = last_name,
              job_title = job_title,
              location = location,
              email = tags$a(href=sprintf("mailto:%s", email), email),
              note = note
              
              )))))

})

#################################################Add data################

##########Functions

#List of mandatory fields for submission
fieldsMandatory <- c("title", "first_name", "last_name", "job_title", "location", "email")

#define which input fields are mandatory 
observe({
    
    #Make reactive to add button
    input$submit
    input$edit_button2
    
    mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    shinyjs::toggleState(id = "edit_button2", condition = mandatoryFilled)
})

#define which input fields should be saved and recorded at time of submission
fieldsAll <- c("title", "first_name", "last_name", "job_title", "location", "email", "note")  

#add a unique row id
unique_id <- function(data){
    replicate(nrow(data), UUIDgenerate())
}

#Needed to reset fileInput
cachedFile <- reactiveValues(
  datapath = NULL
)

observeEvent(input$add_button, {
  
  File <- input$picture_upload 
  cachedFile$datapath <- File$datapath
  entry_form()
  
})

observeEvent(input$edit_button, priority = 20,{
  
  File <- input$picture_upload 
  cachedFile$datapath <- File$datapath
  
})

#Remove any malicious html code
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#save form data into data_frame format
formData <- reactive({
    
    #check if file has already been submitted
    inputFile <- input$picture_upload
    file_cached <- identical(cachedFile$datapath, inputFile$datapath)
    
    data <- sapply(fieldsAll, function(x) (cleanFun(input[[x]])))
    data <- t(data)
    data <- data.frame(data)
    data <- c(data, 
              date=format(Sys.time(), "%Y%m%d_%H%M%S"), 
              row_id = unique_id(data))
    if(file_cached == FALSE){
    data <- c(data, picture = paste("Yes"), image_ext = tools::file_ext(inputFile$name))
    } else{
    data <- c(data, picture = paste("No"), image_ext = paste("ND"))
    }
    
    return(data)
}) 

#Append data to sql table
appendData <- function(data) {
    query <- sqlAppendTable(pool, "participants_df", data, row.names = FALSE)
    dbExecute(pool, query)
}

form <- function(title, button_id, button_name){
    
    #add otherwise modal is only triggered once
    input$add_button
    input$edit_button

    showModal(
        modalDialog(id = "entry_form",
            title = title,
            div(
                fluidPage(
                    htmlTemplate(
                        filename = "www/entry_form.html",
                        title = selectInput("title", label= NULL, 
                                                           selected = NULL, 
                                                           multiple = FALSE,
                                                           choices = c("", "Ms.", "Mr.")),
                        first_name =  textInput("first_name", label = NULL, placeholder = NULL, width = "100%"),
                        last_name = textInput("last_name", label = NULL, placeholder = NULL, width = "100%"),
                        job_title = textInput("job_title", label = NULL, placeholder = NULL, width = "100%"),
                        location = textInput("location", label = NULL, placeholder = NULL, width = "100%"),
                        email = textInput("email", label = NULL, placeholder = NULL, width = "100%"),
                        picture_upload = fileInput("picture_upload", label = NULL, multiple = FALSE, width = "100%", accept = c('image/png', 'image/jpeg')), 
                        note_input = textInput("note", label=NULL, placeholder = "e.g. Monday morning home office.", width = "100%"),
                        submit_button = actionButton(button_id, button_name, icon("save"))),
                        easyClose =TRUE))))
    
    
}

###Add profile picture
saveData <- function(data, outputDir, rowid = formData()$row_id) {
  
  # Create a unique file name
  fileName <- sprintf("%s_%s_%s_%s.%s",  
                      paste(formData()$date),
                      paste(formData()$first_name),
                      paste(formData()$last_name),
                      paste(rowid),
                      paste(formData()$image_ext))
  # Write the file to the local system
  file.copy(data$datapath, file.path(outputDir, fileName), overwrite = TRUE
  )
} 

entry_form <- reactive({
    
    form("New Entry", "submit", "Save")
    
})

success_alert <- reactive({
    
    #make reactive to
    input$submit
    
    shinyalert("Thank you!", "Your data has been submitted.", type = "success", 
               confirmButtonCol = "#337ab7")
    
})

###Observers

observeEvent(input$submit, priority = 20, {
    
  file_ext <- c("JPG", "jpg", "PNG", "png", "JPEG", "jpeg", "JPE", "jpe", "jfif", "JFIF")
  File <- input$picture_upload
  
  #check if first name or last name containes illegal characters
  #check if first name or last name containes illegal characters
  names <- c(input$first_name, input$last_name)
  names <- gsub(" ", "", names, fixed = TRUE)
  name_check <- grepl("^[a-zA-Z0-9_-]*$", names)
  illegal_char <- any(name_check == FALSE)
  
  file_cached <- identical(cachedFile$datapath, File$datapath)
  
  if(is.null(File)){File$name <- "photo.jpg"}
  #Check for correct file format
  if (!tools::file_ext(File$name) %in% file_ext){
    shinyalert("Error", "Wrong file format. Only .jpg and .png allowed.", type = "error", 
               confirmButtonCol = "#337ab7")
  } else if(illegal_char == TRUE){
    shinyalert("Error", "First Name or Last Name contains illegal characters.", type = "error", 
               confirmButtonCol = "#337ab7")
  } else{
    
    appendData(formData())
    # Create participant sql table containing books read
    table_name <- paste("participant_", formData()$row_id, sep = "")
    book_df <- data.frame(participant_id = formData()$row_id, book_title = NA, author = NA, date = NA, pages_total = NA, pages_read = NA)
    dbWriteTable(pool, table_name, book_df, temporary = FALSE, overwrite = FALSE)
    if (file_cached == FALSE) { 
      saveData(data = input$picture_upload, outputDir = "www/profile_images")
    }
    shinyjs::reset("entry_form")
    removeModal()
    success_alert()
  }
})

#################################Edit################################################

edit_form <- reactive({
  
  #Make reactive to add button

  form("Edit Entry", "edit_button2", "Save")
  
})

observeEvent(input$edit_button, priority = 20, {
  
  sel_row<- isolate(input$participants_table_row_last_clicked) 
  row_id <- participants_df_table()[sel_row, "row_id"] 
  cachedRowid$row_id <- row_id
})

observeEvent(input$edit_button,{
  
    sel_row <- input$participants_table_row_last_clicked
    row_id <- participants_df_filtered()[sel_row, "row_id"] 
    table <- participants_df_filtered()

    edit_form()
    
    updateSelectInput(session, "title", selected = table[table$row_id == row_id, "title"])
    updateTextInput(session, "first_name", value = table[table$row_id == row_id, "first_name"])
    updateTextInput(session, "last_name", value = table[table$row_id == row_id, "last_name"])
    updateTextInput(session, "job_title", value = table[table$row_id == row_id, "job_title"])
    updateTextInput(session, "location", value = table[table$row_id == row_id, "location"])
    updateTextInput(session, "email", value = table[table$row_id == row_id, "email"])
    updateTextInput(session, "note", value = table[table$row_id == row_id, "note"])
    
})

removeData <- function(rowid = formData()$row_id) {
  
  # Create a unique file name
  delfile <- dir(path="www/profile_images", pattern = paste(rowid))
  file.remove(file.path("www/profile_images", delfile))
} 

renameData <- function(rowid = formData()$row_id){
  
  table <- participants_df_filtered()
  
  date <- table[table$row_id == rowid, "date"]
  image_ext <- table[table$row_id == rowid, "image_ext"]
  
  fileName <- sprintf("%s_%s_%s_%s.%s",  
                      date,
                      input$first_name,
                      input$last_name,
                      rowid,
                      image_ext)
  
  renfile <- dir(path="www/profile_images/", pattern = paste(rowid))
  file.rename(paste0("www/profile_images/", renfile), paste0("www/profile_images/", fileName))
}

observeEvent(input$edit_button2, {
  
  File <- input$picture_upload
  row_id <-  cachedRowid$row_id 
  
  #check if file has already been submitted
  inputFile <- input$picture_upload
  file_cached <- identical(cachedFile$datapath, File$datapath)
  
  #check if first name or last name containes illegal characters
  names <- c(input$first_name, input$last_name)
  names <- gsub(" ", "", names, fixed = TRUE)
  name_check <- grepl("^[a-zA-Z0-9_-]*$", names)
  illegal_char <- any(name_check == FALSE)

if(illegal_char == TRUE){
  shinyalert("Error", "First Name or Last Name contains illegal characters.", type = "error", 
             confirmButtonCol = "#337ab7")
} else if(file_cached == FALSE){
    
  file_ext <- c("JPG", "jpg", "PNG", "png", "JPEG", "jpeg", "JPE", "jpe", "jfif", "JFIF")

    #Check for correct file format
    if (!tools::file_ext(File$name) %in% file_ext){
      shinyalert("Error", "Wrong file format. Only .jpg and .png allowed.", type = "error", 
                 confirmButtonCol = "#337ab7")}    

    image_ext <- tools::file_ext(File$name)
    profile_date <- format(Sys.time(), "%Y%m%d_%H%M%S")

    removeData(rowid = row_id)
    
    dbExecute(pool, sprintf('UPDATE "participants_df" SET "date" = ?, "title" = ?, "first_name" = ?, "last_name" = ?, 
                          "job_title" = ?, "location" = ?, "email" = ?, "picture" = ?, "image_ext" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id), 
              param = list(profile_date,
                           input$title,
                           cleanFun(input$first_name),
                           cleanFun(input$last_name),
                           cleanFun(input$job_title),
                           cleanFun(input$location),
                           cleanFun(input$email),
                           "Yes",
                           image_ext,
                           input$note
              ))
    saveData(data = input$picture_upload, outputDir = "www/profile_images", rowid = row_id)
    removeModal() 
} else{
  
  dbExecute(pool, sprintf('UPDATE "participants_df" SET "title" = ?, "first_name" = ?, "last_name" = ?, 
                          "job_title" = ?, "location" = ?, "email" = ?, "note" = ?
                          WHERE "row_id" = ("%s")', row_id), 
            param = list(input$title,
                         cleanFun(input$first_name),
                         cleanFun(input$last_name),
                         cleanFun(input$job_title),
                         cleanFun(input$location),
                         cleanFun(input$email),
                         input$note
            ))
  renameData(rowid = row_id)
  removeModal()
  }

})

#################################################Delete data###################################

#Needed to keep rowID after DT refresh
cachedRowid <- reactiveValues(
  row_id = NULL
)

observeEvent(input$delete_button, {
  
  sel_row<- isolate(input$participants_table_row_last_clicked) 
  row_id <- participants_df_table()[sel_row, "row_id"] 
  cachedRowid$row_id <- row_id
  
})

del_row <- reactive({
    
    row_id <- cachedRowid$row_id 
  
    query <- dbExecute(pool, sprintf('DELETE FROM "participants_df" WHERE "row_id" == ("%s")', row_id)) 
})

del_screenshot_file <- function(data_path = "www/profile_images/"){
  
  row_id <- cachedRowid$row_id 
  
  files_in_dir <- data.frame(list.files(data_path, full.names = TRUE), stringsAsFactors = FALSE)
  names(files_in_dir) <- "file_name"
  file_id <- files_in_dir %>% filter(str_detect(file_name, paste(row_id, collapse = "|")))
  file_names <- file_id[, "file_name"]
  file_name <- file_id[1, "file_name"] 
  if(!is.na(file_name)){
    lapply(file_names, function(file){file.remove(file)})
  }
  
}

observeEvent(input$delete_button, priority = 20,{
  
    showModal(
        modalDialog(id="delete_modal",
            title = "Warning",
            paste("Are you sure you want to delete this row?"),
            br(),
            br(),
            actionButton("yes_button", "Yes"),
            actionButton("no_button", "No"),
            easyClose = TRUE, footer = NULL))
  
})

observeEvent(input$yes_button, priority = 20, {
     
    del_screenshot_file()
    del_row()
    removeModal()
    
})

observeEvent(
    input$no_button, 
    priority = 20, {
        removeModal()
    }
)

######################################DT Output############################################

DTproxy <- dataTableProxy("participants_table") 
observeEvent(input$search_field, {
  updateSearch(DTproxy, keywords = list(global = input$search_field, columns = NULL))
})
 
output$participants_table <- DT::renderDataTable({
  
    participants_df_table <- participants_df_table() %>% select(-row_id)
    
    names(participants_df_table) <- c("Bilde", "Deltaker", "Vis kort", "Sted", "E-post", "Handlinger")
    
    table <- DT::datatable(
        participants_df_table,
        rownames = FALSE,
        escape = FALSE,
        filter = "none",
        selection = "single",
        options = list(searching = TRUE, 
                       lengthChange = FALSE,
                       pageLength = 20,
                       autoWidth = FALSE,
                       columnDefs = (list(list(width = '50px', targets = c(0, 2, 3)),
                                          list(width = '300px', targets = c(1)),
                                          list(width = '80px', targets = c(0, 5)),
                                          list(width = '130px', targets = c(4)),
                                     list(className = 'dt-center', targets = c(2, 3)))),
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#375a7f', 'color': '#fff'});",
                           "}")))
})

output$footer_date <- renderUI({
  
  last_modified <- max(participants_df()$date, na.rm = TRUE)
  last_date <- as.Date(last_modified, "%Y%m%d")
  last_time <- gsub(".*_", "", last_modified)
  last_time <- gsub('(..)(?=.)', '\\1:', last_time, perl=TRUE)

  div(class="footer-date", HTML(paste0("Last Entry: ", last_date, " ", last_time)))
  
})            
    

}