# reactable table function
lulcrct_fun <- function(sums, colnm, grpby = T, yrsel = '1990'){
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  )
  
  if(grpby)
    out <- reactable(
      sums, 
      groupBy = 'grpval',
      columns = list(
        grpval = colDef(name = '', minWidth = 170, class = 'sticky left-col-1', headerClass = 'sticky left-col-1', footerClass = 'sticky left-col-1'),
        val = colDef(
          name = colnm, 
          footer = 'Total', 
          minWidth = 200, 
          class = 'sticky left-col-2', headerClass = 'sticky left-col-2', footerClass = 'sticky left-col-2'
          ), 
        `1990` = colDef(footer = sum(sums$`1990`), aggregate = 'sum'),
        `1995` = colDef(footer = sum(sums$`1995`), aggregate = 'sum'),
        `1999` = colDef(footer = sum(sums$`1999`), aggregate = 'sum'),
        `2004` = colDef(footer = sum(sums$`2004`), aggregate = 'sum'),
        `2007` = colDef(footer = sum(sums$`2007`), aggregate = 'sum'),
        `2011` = colDef(footer = sum(sums$`2011`), aggregate = 'sum'),
        `2014` = colDef(footer = sum(sums$`2014`), aggregate = 'sum'),
        `2017` = colDef(footer = sum(sums$`2017`), aggregate = 'sum'),
        chg = colDef(name = paste0(yrsel, '-2017 change'), minWidth = 140,
                     style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
        ), 
        chgper = colDef(name = '% change', minWidth = 85,
                        style = jsfun,
                        format = colFormat(suffix = '%', digits = 0), 
                        class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1'
                        
        )
      ),
      defaultColDef = colDef(
        footerStyle = list(fontWeight = "bold", separators = T),
        format = colFormat(digits = 0, separators = TRUE), 
        resizable = F
      ),
      highlight = T,
      wrap = T
    )
  
  if(!grpby)
    out <- reactable(
      sums, 
      columns = list(
        val = colDef(name = colnm, footer = 'Total', minWidth = 240, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
        `1990` = colDef(footer = sum(sums$`1990`), aggregate = 'sum'),
        `1995` = colDef(footer = sum(sums$`1995`), aggregate = 'sum'),
        `1999` = colDef(footer = sum(sums$`1999`), aggregate = 'sum'),
        `2004` = colDef(footer = sum(sums$`2004`), aggregate = 'sum'),
        `2007` = colDef(footer = sum(sums$`2007`), aggregate = 'sum'),
        `2011` = colDef(footer = sum(sums$`2011`), aggregate = 'sum'),
        `2014` = colDef(footer = sum(sums$`2014`), aggregate = 'sum'),
        `2017` = colDef(footer = sum(sums$`2017`), aggregate = 'sum'),
        chg = colDef(name = paste0(yrsel, '-2017 change'), minWidth = 140,
                     style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
        ), 
        chgper = colDef(name = '% change', minWidth = 85,
                        style = jsfun,
                        format = colFormat(suffix = '%', digits = 0), 
                        class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1'
                        
        )
      ),
      defaultColDef = colDef(
        footerStyle = list(fontWeight = "bold", separators = T),
        format = colFormat(digits = 0, separators = TRUE), 
        minWidth = 75, resizable = TRUE
      ),
      defaultPageSize = nrow(sums),
      showPageSizeOptions = F,
      highlight = T,
      wrap = F
    )
  
  return(out)
  
}

# change estimate as percentages for supra or subtidal, used in lngtrmtab_fun
chgfun <- function(datin, yrsel){
  
  # yrs in input data
  yrs <- datin$name %>% 
    unique

  datin <- datin %>% 
    spread(name, Acres, fill = 0)
  
  # calc diffs if both yrsel present
  if(sum(unique(yrsel) %in% yrs) == 2){
    out <- datin %>%
      rename(chgyr1 = !!yrsel[1]) %>% 
      rename(chgyr2 = !!yrsel[2]) %>% 
      mutate(
        chg = chgyr2 - chgyr1,
        chgper = 100 * (chgyr2 - chgyr1) / chgyr1, 
        chgicon = case_when(
          chgper > 0 ~ 'arrow-circle-up', 
          chgper <= 0 ~ 'arrow-circle-down'
        ), 
        chgcols = case_when(
          chgper >= 0 ~ '#008000E6', 
          chgper < 0 ~ '#e00000E6'
        )
      ) %>% 
      rename(val = HMPU_TARGETS)
    names(out)[names(out) == 'chgyr1'] <- yrsel[1]
    names(out)[names(out) == 'chgyr2'] <- yrsel[2]
  }
  
  # NA if yrsel is equal or missing a yrsel
  if(yrsel[1] == yrsel[2] | any(!yrsel %in% yrs)){
    out <- datin %>% 
      mutate(
        chg = NA, 
        chgper = NA, 
        chgicon = '', 
        chgcols = ''
      ) %>% 
      rename(val = HMPU_TARGETS)
  }
  
  return(out)
  
}

# reactable table function that works for supra/intertidal and subtidal
lngtrmtab_fun <- function(datin, colnm, yrsel, firstwidth = 240){
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee")

  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  )

  # get change summary
  sums <- chgfun(datin, yrsel)

  totab <- sums %>% 
    mutate(
      chg = formatC(round(chg, 0), format = "d", big.mark = ","),
      chgper = as.character(round(chgper, 0))
    )
  
  out <- reactable(
    totab, 
    columns = list(
      chgicon = colDef(show = F), 
      chgcols = colDef(show = F),
      val = colDef(name = colnm, footer = 'Total', minWidth = firstwidth, class = 'sticky left-col-1-bord', headerClass = 'sticky left-col-1-bord', footerClass = 'sticky left-col-1-bord'), 
      chg = colDef(name = paste0(yrsel[1], '-', yrsel[2], ' change'), minWidth = 140,
                   style = jsfun, class = 'sticky right-col-2', headerClass = 'sticky right-col-2', footerClass = 'sticky right-col-2'
      ), 
      chgper = colDef(name = '% change', minWidth = 85,
                      style = jsfun,
                      format = colFormat(suffix = '%', digits = 0), 
                      align = 'right', 
                      class = 'sticky right-col-1', headerClass = 'sticky right-col-1', footerClass = 'sticky right-col-1', 
                      cell = icon_sets(totab, icon_ref = 'chgicon', icon_position = 'right', icon_color_ref = 'chgcols')
                      
      )
    ),
    defaultColDef = colDef(
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      footerStyle = list(fontWeight = "bold"),
      format = colFormat(digits = 0, separators = TRUE), 
      minWidth = 80, resizable = TRUE
    ),
    defaultPageSize = nrow(sums),
    showPageSizeOptions = F,
    highlight = T,
    wrap = F
  )
  
  return(out)
  
}

# alluvial function
# https://www.data-to-viz.com/graph/sankey.html
alluvout <- function(chgdat, lkup, var = 'HMPU_DESCRIPTOR', height = 1200){
  
  clp <- lkup %>% 
    select(!!var, FLUCCSCODE) %>% 
    deframe() %>%
    map(as.character)
  
  sumdat <- chgdat %>% 
    select(FLUCCS17, FLUCCS90, Acres) %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    mutate(
      FLUCCS17 = factor(FLUCCS17, levels = lkup$FLUCCSCODE),
      FLUCCS17 = fct_recode(FLUCCS17, !!!clp),
      FLUCCS90 = factor(FLUCCS90, levels = lkup$FLUCCSCODE),
      FLUCCS90 = fct_recode(FLUCCS90, !!!clp),
    ) %>% 
    na.omit() %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    select(source = FLUCCS90, target = FLUCCS17, value = Acres) %>% 
    data.frame(stringsAsFactors = F)
  sumdat$target <- paste(sumdat$target, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(sumdat$source), as.character(sumdat$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  sumdat$IDsource=match(sumdat$source, nodes$name)-1 
  sumdat$IDtarget=match(sumdat$target, nodes$name)-1
  
  out <- sankeyNetwork(Links = sumdat, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", height = height, width = 800,
                sinksRight=FALSE, units = 'acres', nodeWidth=40, fontSize=13, nodePadding=5)
  
  return(out)
  
}

# reactable change table for 1990 to 2017 comp
cmprctfun <- function(chgdat, lkup, var = 'HMPU_DESCRIPTOR'){
  
  clp <- lkup %>% 
    select(!!var, FLUCCSCODE) %>% 
    deframe %>%
    map(as.character)

  sumdat <- chgdat %>% 
    select(FLUCCS17, FLUCCS90, Acres) %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    mutate(
      FLUCCS17 = factor(FLUCCS17, levels = lkup$FLUCCSCODE),
      FLUCCS17 = fct_recode(FLUCCS17, !!!clp),
      FLUCCS90 = factor(FLUCCS90, levels = lkup$FLUCCSCODE),
      FLUCCS90 = fct_recode(FLUCCS90, !!!clp),
    ) %>% 
    na.omit() %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    rename(
      source = FLUCCS90, 
      target = FLUCCS17, 
      value = Acres
    ) %>% 
    mutate(
      target = factor(target, levels = sort(levels(target))),
      source = factor(source, levels = sort(levels(source)))
    )
  
  totab <- sumdat %>% 
    complete(source, target) %>% 
    spread(target, value, fill = 0) %>% 
    mutate(Total = select_if(., is.numeric) %>% rowSums)
  
  srcttl <- select(totab, source, Total)
  trgttl <- totab %>% 
    select(-source, -Total) %>% 
    gather('Category', 'Total') %>% 
    mutate(Category = factor(Category, levels = levels(totab$source))) %>% 
    group_by(Category) %>% 
    summarise(Total = sum(Total)) %>% 
    ungroup
  
  totab <- totab %>%
    mutate(
      chg = trgttl$Total - Total,
      chgper = 100 * chg / Total, 
      chgper = ifelse(is.na(chgper), 0, chgper),
      chg = as.character(round(chg, 0)),
      chgper = as.character(round(chgper, 1)), 
      Total = as.character(round(Total, 0))
    )
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  ) 
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee", fontWeight = 'bold')
  
  out <- reactable(
    totab, 
    columns = list(
      source = colDef(
        name = '', 
        footer = '2017 total', 
        minWidth = 250,
        style = sticky_style,
        headerStyle = sticky_style, 
        footerStyle = sticky_style
        ), 
      Total = colDef(
        name = '1990 total', 
        style = list(fontWeight = 'bold'),
        class = "sticky right-col-3a",
        headerClass = "sticky right-col-3a",
        footerClass = "sticky right-col-3a"
        ),
      chg = colDef(
        name = '1990-2017 change (acres)', 
        style = jsfun,
        class = "sticky right-col-2a",
        headerClass = "sticky right-col-2a",
        footerClass = "sticky right-col-2a"
        ), 
      chgper = colDef(
        name = '% change',
        style = jsfun,
        format = colFormat(suffix = '%', digits = 0),
        class = "sticky right-col-1",
        headerClass = "sticky right-col-1",
        footerClass = "sticky right-col-1"
        )
    ),
    defaultColDef = colDef(
      footerStyle = list(fontWeight = "bold"),
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        round(sum(values), 0)
        
      },
      format = colFormat(digits = 0, separators = TRUE),
      resizable = TRUE
    ),
    # height = 800,
    highlight = T,
    wrap = T, 
    pagination = F
  )
  
  return(out)
  
}

# alluvial plot function, for HMPU targets
# https://www.data-to-viz.com/graph/sankey.html
alluvout2 <- function(datin, fluccs){
  
  clp <- fluccs %>%
    pull(HMPU_TARGETS) %>% 
    unique %>% 
    c('Coastal Uplands', .) %>% 
    sort

  sumdat <- datin %>% 
    rename(Acres = value) %>% 
    mutate(
      target = gsub(',\\s[0-9]+$', '', target),
      source = gsub(',\\s[0-9]+$', '', source)
    ) %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    na.omit() %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    select(source = source, target = target, value = Acres) %>% 
    data.frame(stringsAsFactors = F)
  sumdat$target <- paste(sumdat$target, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(sumdat$source), as.character(sumdat$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  sumdat$IDsource=match(sumdat$source, nodes$name)-1 
  sumdat$IDtarget=match(sumdat$target, nodes$name)-1
  
  out <- sankeyNetwork(Links = sumdat, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "value", NodeID = "name", height = 1200, width = 800,
                       sinksRight=FALSE, units = 'acres', nodeWidth=40, fontSize=13, nodePadding=5)
  
  return(out)

}

# reactable change table for year pairs
cmprctfun2 <- function(datin, fluccs, yrsel = '1990', maxyr = '2017', subt = F){
  
  clp <- fluccs %>%
    pull(HMPU_TARGETS) %>% 
    unique
  
  subtclp <- c('Hard Bottom', 'Open Water', 'Oyster Bars', 'Restorable', 'Seagrasses', 'Tidal Flats', 'other')
  if(subt) 
    clp <- subtclp
  
  if(!subt)
    clp <- c(sort(c('Coastal Uplands', 'Restorable', 'Open Water', clp[!clp %in% subtclp])), 'other')
  
  sumdat <- datin %>% 
    select(target, source, Acres = value) %>% 
    mutate(
      target = gsub(',\\s[0-9]+$', '', target),
      source = gsub(',\\s[0-9]+$', '', source)
    ) %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    mutate(
      target = factor(target, levels = clp),
      source = factor(source, levels = clp)
    ) %>%
    na.omit() %>% 
    group_by(target, source) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    rename(
      source = source, 
      target = target, 
      value = Acres
    ) 
  
  totab <- sumdat %>% 
    complete(source, target) %>% 
    mutate_if(is.factor, as.character) %>% 
    spread(target, value, fill = 0) %>% 
    mutate(Total = select_if(., is.numeric) %>% rowSums)
  
  srcttl <- select(totab, source, Total)
  trgttl <- totab %>% 
    select(-source, -Total) %>% 
    gather('Category', 'Total') %>% 
    group_by(Category) %>% 
    summarise(Total = sum(Total)) %>% 
    ungroup
  
  totab <- totab %>%
    mutate(
      chg = trgttl$Total - Total,
      chgper = 100 * chg / Total, 
      chgper = ifelse(is.na(chgper), 0, chgper),
      chg = as.character(formatC(round(chg, 0), format = "d", big.mark = ",")),
      chgper = as.character(round(chgper, 0)),
      Total = as.character(formatC(round(Total, 0), format = "d", big.mark = ",")),
      source = as.character(source)
    )
  
  # arrange columns, rows by character (factors screws up shiny server)
  totab <- totab[rank(clp, totab$source), ]
  totab <- totab[, c('source', clp, 'Total', 'chg', 'chgper')]
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (parseInt(value) >= 0) {
      var color = '#008000E6'
    } else if (parseInt(value) < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  ) 
  
  sticky_style <- list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                       borderRight = "1px solid #eee", fontWeight = 'bold')
  
  out <- reactable(
    totab, 
    columns = list(
      source = colDef(
        name = '', 
        footer = paste0(maxyr, ' total'), 
        minWidth = 250,
        style = sticky_style,
        headerStyle = sticky_style, 
        footerStyle = sticky_style
      ), 
      Total = colDef(
        name = paste0(yrsel, ' total'), 
        style = list(fontWeight = 'bold'),
        class = "sticky right-col-3a",
        headerClass = "sticky right-col-3a",
        footerClass = "sticky right-col-3a"
      ),
      chg = colDef(
        name = paste0(yrsel, '-', maxyr, ' change (acres)'), 
        style = jsfun,
        class = "sticky right-col-2a",
        headerClass = "sticky right-col-2a",
        footerClass = "sticky right-col-2a"
      ), 
      chgper = colDef(
        name = '% change',
        style = jsfun,
        format = colFormat(suffix = '%', digits = 0),
        class = "sticky right-col-1",
        headerClass = "sticky right-col-1",
        footerClass = "sticky right-col-1"
      )
    ),
    defaultColDef = colDef(
      footerStyle = list(fontWeight = "bold"),
      footer = function(values){
        if(!is.numeric(values))
          return()
        
        formatC(round(sum(values), 0), format= "d", big.mark = ",")
        
      },
      format = colFormat(digits = 0, separators = TRUE),
      resizable = TRUE
    ),
    # height = 800,
    highlight = T,
    wrap = T, 
    pagination = F
  )
  
  return(out)
  
}
