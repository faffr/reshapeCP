#'A function to reshape form data from ChangePond (Best Plus, EPT)
#'
#'This function reshapes form data downloaded from ChangePond
#'@param form Data frame of messy data downloaded from ChangePond
#'@param NitemsInPanel vector indicating the number of items in each panel, in the order that panels appear
#'@param NitemsInModule vector indicating the number of items in each module, in the order that modules appear in a panel
#'@param outName output file name to be saved as .csv in working directory
#'@keywords reshape
#'@export
#'@examples
#'###7 values for 7 panels. Each value represents the number of items in each panel.
#'ItemsInPanelsEPTS = c(3,3,4,5,5,5,10) 
#'#18 values for 18 modules. Each value represents the number of items in each module.
#'ItemsInModulesEPTS = c(3,3,2,2,1,2,2,1,2,2,1,2,2,2,2,2,2,2) 
#'#EPTform is the data frame that is downloaded from CP and read in as a data frame.
#'reshapeCP(EPTform, ItemsInPanelsEPTS, ItemsInModulesEPTS, outName = EPTform_clean)

reshapeCP = function(form, NitemsInPanel, NitemsInModule, outName = NULL){
  
  NPanels = length(NitemsInPanel)
  Nmodules = length(NitemsInModule)
  
  #LEVEL 1: TEST FORM INFORMATINO AND PANEL INFORMATION
  PanelID = BPform[, grepl("Panel.ID", names(BPform))]
  PanelName = BPform[, grepl("Panel.Name", names(BPform))]
  PanelStatus = BPform[, grepl("Panel.Status", names(BPform))]
  PanelNotes = BPform[, grepl("Panel.Notes", names(BPform))]
  
  #LEVEL 2
  modID = BPform[, grepl("Module.ID", names(BPform))]
  modName = BPform[,grepl("Module.Name", names(BPform))]
  modStatus = BPform[, grepl("Module.Status", names(BPform))]
  modNotes = BPform[,grepl("Module.Notes", names(BPform))]
  
  #LEVEL 3
  itemID = BPform[, grepl("Item.ID", names(BPform))]
  itemName = BPform[, grepl("Item.Name", names(BPform))]
  itemStatus = BPform[, grepl("Item.Status", names(BPform))]
  itemSetup = BPform[, grepl("Item.Setup", names(BPform))]
  itemPrompt = BPform[, grepl("Item.Prompt", names(BPform))]
  itemGraphic = BPform[, grepl("Item.Graphic", names(BPform))]
  itemRubric = BPform[, grepl("Scoring.Rubric", names(BPform))]
  itemScale = BPform[, grepl("Measurement.Scale", names(BPform))]
  itemDiffic = BPform[, grepl("Item.Difficulty", names(BPform))]
  itemSelecDiff = BPform[, grepl("Selection.Difficulty", names(BPform))]
  itemNotes = BPform[, grepl("Item.Notes", names(BPform))]
  
  if(length(itemID) != sum(NitemsInPanel) ){stop("Number of items specified does not match number of items in data")}
  
  #PUT TOGETHER LEVEL 1 FORM NAMES
  dfForm = data.frame(rep(BPform[,grepl("Test.Form.ID", names(BPform))], sum(NitemsInPanel)), rep(BPform[,grepl("Test.Form.Name", names(BPform))], sum(NitemsInPanel)), rep(BPform[,grepl("Test.Family", names(BPform))], sum(NitemsInPanel))  )
  colnames(dfForm) = c("Test.Form.ID", "Test.Form.Name", "Test.Family")
  
  #PUT TOGETHER LEVEL 2 PANEL NAMES
  dfPanel = data.frame(t(PanelID),
                       t(PanelName),
                       t(PanelStatus),
                       t(PanelNotes))
  colnames(dfPanel) = c("Panel.ID", "Panel.Name", "Panel.Status", "Panel.Notes")
  
  #NUMBER OF ITEMS WITHIN PANELS: FIRST ROW IS PANEL NUMBER, SECOND ROW IS NUMBER OF ITEMS
  panStruc = data.frame(seq(1:NPanels), NitemsInPanel)
  
  #SIMPLE FUNCTION TO BIND
  fun1 = function(i, struct, dfNames){
    do.call("rbind", replicate(struct[i,2], dfNames[i,], simplify = FALSE))
  }
  
  #INTERMEDIARY STEP TO CREATE FINAL PANEL INFORMATION COLUMN
  datalist = list()
  
  for(i in 1:NPanels){
    dat = data.frame(fun1(i, panStruc, dfPanel))
    datalist[[i]] = dat
    
  }
  
  dfPans = do.call(rbind, datalist)
  
  #PUT TOGETHER LEVEL 3 MODULE NAMES AND RELATED COLUMNS
  dfModule = data.frame(t(modID), t(modName),
                        t(modStatus), t(modNotes), stringsAsFactors = FALSE )
  colnames(dfModule) = c("Module.ID", "Module.Name",
                         "Module.Status", "Module.Notes")
  
  modStruc = data.frame(seq(1:Nmodules), NitemsInModule)
  
  #INTERMEDIARY STEP TO CREATE FINAL MODULE INFORMATION COLUMN
  datalist = list()
  
  for(i in 1:Nmodules){
    dat = data.frame(fun1(i, modStruc, dfModule))
    datalist[[i]] = dat
  }
  
  dfMods = do.call(rbind, datalist)
  
  #PUT TOGETHER LEVEL 4 ITEM NAMES AND RELATED COLUMNS
  dfItem = data.frame(t(itemID), t(itemName),
                      t(itemStatus), t(itemSetup),
                      t(itemPrompt), t(itemGraphic),
                      t(itemRubric), t(itemScale),
                      t(itemDiffic), t(itemSelecDiff),
                      t(itemNotes) )
  
  colnames(dfItem) = c("Item.ID", "Item.Name",
                       "Item.Status", "Item.Setup",
                       "Item.Prompt", "Item.Graphic",
                       "Scoring.Rubric", "Measurement.Scale",
                       "Item.Difficulty", "Selection.Difficulty",
                       "Item.Notes")
  
  #COMBINE ALL TOGETHER
  df.final = data.frame(dfForm, dfPans, dfMods, dfItem, row.names = NULL)
  
  #WRITE OUT THE .CSV
  if(!is.null(outName)){write.csv(df.final,paste0(outName,".csv"), row.names = FALSE)}
  
  return(df.final)
  
}