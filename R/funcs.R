map_fun <- function(locs, tfwdat){
  
  # summarize events by site
  sitsum <- tfwdat %>% 
    group_by(Site) %>% 
    summarise(
      nevnt = length(unique(Date)), 
      totlbs = sum(LitterRecyclablesWeightLb, LitterTrashWeightLb, DebrisRecyclablesWeightLb, DebrisTrashWeightLb, na.rm = T)
    )
  
  # locations combined with all site data
  tomap <- locs %>% 
    select(Site, County, lon, lat) %>% 
    unique %>% 
    left_join(sitsum, by = 'Site') %>% 
    mutate(
      nevnt = ifelse(is.na(nevnt), 0, nevnt), 
      lab = paste0("<b>Site</b>: ", Site, '<br/><b>County</b>: ', County, '<br/><b>Number of events</b>: ', nevnt),
      lab = lapply(lab, HTML), 
      totlbs = round(totlbs, 1)
    ) %>% 
    select(Site, County, `Number of events` = nevnt, `Total (lbs)` = totlbs, lab, lon, lat)
  
  sd <- SharedData$new(tomap)
  
  out <- bscols(list(
    leaflet(sd) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(lng = ~lon, lat = ~lat, label = ~lab),
    datatable(sd, extensions="Scroller", style="bootstrap", class="compact", width="100%", rownames = F,
              options=list(deferRender=TRUE, scrollY=300, scroller=F, dom = 'ltp',
                           columnDefs = list(list(visible=FALSE, 
                                                  targets=c(4, 5, 6)))
              ))
  ))
  
  return(out)
  
}