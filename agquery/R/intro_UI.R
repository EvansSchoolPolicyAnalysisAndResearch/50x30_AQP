intro_ui <- function(id, pathway_names){
  ns <- shiny::NS(id)
  tabPanel("Introduction", column(1),
           column(10, wellPanel(HTML(
             "The 50x30 Cambodia Data explorer is a tool to rapidly summarize and visualize the Cambodian Agricultural Survey data. In conjunction with other forms of analysis, this app..."
           )),
           img(src='logic-model.png', width='80%'),
           hr(),
           fluidRow(column(8, 
                           HTML(paste('<h3>Purpose</h3><br><p>The 50x30 Cambodia Data Explorer is a way to view and compare information from the Cambodian Agricultural Surveys to address the following policy priorities:',
                                         '<ul>',
                                          paste(lapply(pathway_names, FUN=function(x){paste0("<li>",x, "</li>")}), collapse=" "),
                                         '</ul></p>',
                                         '<h3>Using the Cambodia 50x30 App</h3> <p>The Cambodian Agricultural Survey contains information on household production of crops and livestock that can be used to understand trends in small-scale farmer contributions to national supply and the economic conditions small-scale producers face.',
                                         'Selecting a policy priority will allow you to narrow down the indicators to those considered most relevant.</p>',
                                         '<h3> Tabs </h3>',
                                         '<h4> Instructions </h4>',
                                         '<p>This tab provides step-by-step instructions, tips, and frequently asked questions (FAQs) about the data explorer.',
                                         '<h4>Policy Pathways</h4>',
                                         '<p>This tab overviews select academic and gray literature for policy pathways that can generate ideas for effective policies and programs that can help shift key indicators of agricultural development</p>',
                                         '<h4>Trends Explorer</h4>',
                                         '<p>This tab shows changes in variables between surveys and across provinces.</p>',
                                         '<h4>Data Explorer</h4>',
                                         '<p>This tab allows for direct comparisons of indicators and provides detailed graphs and summaries of correlations.</p>',
                                         '<h4>Downloads</h4>',
                                         '<p>This tab allows you to download the spreadsheets used to run the app and the processed survey data.</p>',
                                         '<br>',
                                         '<h3>Code and Data Availability</h3>',
                                         '<p> The Stata code used to process the data is publicly available at (Git Repository TBD). <br> The app source code and related files can be downloaded at (Git repository TBD)</p>',
                                         '<br>',
                                         '<h3>Inquire</h3>',
                                         '<p>This tool is maintained by <i>responsible party</i> who has <i>contact info</i>.</p>',
                                         '<br><br>',
                                         '<p> The raw data for the 50x30 survey is located at <a href="https://nada.nis.gov.kh/index.php/catalog/36">https://nada.nis.gov.kh/index.php/catalog/36</a>.</p><br><br><br>')
           )
           )
           )
           )
  )
}