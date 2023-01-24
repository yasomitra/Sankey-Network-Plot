sankey_chart <- function(data)
{
  input_data <- data
  
  #getting count of source in each line
  grp_cnt <- input_data %>%
    mutate(row = row_number()) %>% #a new column with a row number
    gather('column', 'source',-row) %>% #gathers data without including column row and creates a pair of "column" and "source" using 2 columns- "column"- column name and "source"- value in that column
    mutate(column = match(column, names(input_data))) %>% #gets the location of column from the original dataset
    group_by(column, source) %>% #grouping by column and source
    summarise(count = n()) %>% #summaring the group by and getting count
    filter(!is.na(source)) %>% #removing NA
    filter((source) != "") %>% #removing missing
    mutate(new_source = paste0(source, '_', column)) %>% #adding the column to the end of the source in order differentiate the value
    ungroup() %>%
    mutate(new_source_2 = ifelse(
      source == "Did not continue",
      '1',
      ifelse(source == "Other", '2', '3')
    )) %>%
    relocate("column", "source", "new_source", "new_source_2", "count") %>%
    arrange(column, desc(new_source_2), desc(count), source)
  
  links <- input_data %>%
    mutate(row = row_number()) %>% #a new column with a row number
    gather('column', 'source',-row) %>% #gathers data without including column row and creates a pair of "column" and "source" using 2 columns- "column"- column name and "source"- value in that column
    mutate(group = source) %>% #a new column group with values from source
    mutate(column = match(column, names(input_data))) %>% #gets the location of column from the original dataset
    group_by(row) %>% #grouping by row number
    arrange(column) %>% #arranging by column number
    mutate(target = lead(source)) %>% #creating column, target by leading the source value which is grouped by row
    ungroup() %>% #data is ungrouped
    filter(!is.na(target)) %>% #removing NA
    filter((target) != "") %>% #removing missing
    mutate(source_2 = paste0(source)) %>% #adding the column to the end of the source in order differentiate the value
    mutate(target_2 = paste0(target)) %>%
    mutate(name = paste0(source, ' → ', target)) %>%
    mutate(source = paste0(source, '_', column)) %>% #adding the column to the end of the source in order differentiate the value
    mutate(target = paste0(target, '_', column + 1)) %>% #adding the column to the end of the target in order differentiate the value, +1 to indicate its part of next set
    select(column, source, target, group, name, source_2, target_2)
  
  nodes <- data.frame(name = unique(c(grp_cnt$new_source)))
  
  #########################################################################
  
  links_col <-
    data.frame(name = unique(c(links$source_2, links$target_2)))
  
  links_col <- links_col %>%
    mutate(row = row_number())
  
  links_col_merge <-
    merge(links_col,
          node_names_w_cols,
          by.x = "name",
          by.y = "Name")
  
  links_col_merge <- links_col_merge %>%
    arrange(row)
  
  #########################################################################
  
  nodes_col <- data.frame(name = unique(c(grp_cnt$source)))
  
  nodes_col <- nodes_col %>%
    mutate(row = row_number())
  
  nodes_col_merge <-
    merge(nodes_col,
          node_names_w_cols,
          by.x = "name",
          by.y = "Name")
  
  nodes_col_merge <- nodes_col_merge %>%
    arrange(row)
  
  nodes_collapse <- paste(nodes_col_merge$name, collapse = '","')
  rgb_collapse <- paste(nodes_col_merge$rgb, collapse = '","')
  
  nodes_collapse <- paste('["', nodes_collapse, '"]')
  rgb_collapse <- paste('["', rgb_collapse, '"]')
  
  
  #########################################################################
  
  nodes <- nodes %>%
    mutate(row = row_number()) #a new column with a row number
  
  #merging the grp_cnt and nodes table to get count column
  node_merge <- merge(nodes, grp_cnt, by.x = "name", by.y = "new_source")
  
  node_merge_srt <- node_merge %>%
    arrange(row) #arranging by row number as merge messes up order which is very crucial while creating links
  
  links$source <-
    match(links$source, node_merge_srt$name) - 1 #converting the source column to a number, i.e. a position as sankey network cannot understand string and needs integer to get the flow
  links$target <-
    match(links$target, node_merge_srt$name) - 1 #converting the target column to a number, i.e. a position as sankey network cannot understand string and needs integer to get the flow
  links$value <-
    1 #assigning a value 1 to all links, i.e. every chain carries a value 1. This determines the thickness of sankey
  
  node_merge_srt$name <- sub('_[0-9]+$', '', node_merge_srt$name)
  
  node_merge_srt$group <- node_merge_srt$name
  
  node_merge_srt <- node_merge_srt %>%
    mutate(name_2 = paste0(name, ' (N=', count, ')')) #creating a column with count
  
  colors <- paste(links_col_merge$hex, collapse = '", "')
  
  colorJS <- paste('d3.scaleOrdinal(["', colors, '"])')
  
  sankey_plot <-
    sankeyNetwork(
      nodePadding = 20,
      sinksRight = FALSE,
      Links = links,
      Nodes = node_merge_srt,
      Source = 'source',
      Target = 'target',
      Value = 'value',
      NodeID = 'name_2',
      LinkGroup = "group",
      NodeGroup = "group",
      fontSize = 12,
      height = "450",
      iterations = 0,
      width = "1350",
      colourScale = colorJS
    )
  
  sankey_plot$x$links$link_name <- links$name
  sankey_plot$x$nodes$node_name <- node_merge_srt$name_2
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el, x) {
                                       d3.selectAll(".node text")

                                       .style("font-family","calibri")
                                       .style("font-weight","bold")
                                       .attr("font-size",15);
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el) {
                                       d3.select(el).select("svg").select("g")
                                       .attr("transform","translate(20,80)")
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el, x) {
                                       d3.select("div").select("div")
                                       .style("height","1000px");
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el, x) {
                                       d3.select(el).select("svg")
                                       .attr("viewBox","-20,250,1350,500");
                                       }
                                       '
  )
  
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el) {
                                       var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i);
                                       var labels1 = ["1","2","3","4","5"];
                                       var labels2 = ["st","nd","rd","th","th"];
                                       cols_x.forEach((d, i) => {
                                       d3.select(el).select("svg")
                                       .append("text")
                                       .attr("x", d-10)
                                       .attr("y", 50)
                                       .attr("font-size",30)
                                       .attr("font-weight","bold")
                                       .attr("font-family", "calibri")
                                       .append("tspan")
                                       .text(labels1[i])
                                       .append("tspan")
                                       .attr("font-size",30)
                                       .attr("dy",-10)
                                       .text(labels2[i])
                                       .append("tspan")
                                       .attr("dy",10)
                                       .text(" LoT");
                                       })
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(
    sankey_plot,
    '
                                       function(el) {
                                       d3.select(el).select("svg")
                                       .append("text")
                                       .attr("x", 1000)
                                       .attr("y", 450)
                                       .attr("font-size",20)
                                       .attr("font-weight","bold")
                                       .attr("font-family", "calibri")
                                       .text("LoT- Line of Therapy");
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(sankey_plot,
                                       '
                                       function(el, x) {
                                       d3.selectAll(".link")
                                       .style("opacity","0.7")

                                       }
                                       ')
  
  plot_legend <- paste(
    '
                                       function(el) { ',
    '
                                       var labels1 = ',
    nodes_collapse,
    ';
                                       labels1.forEach((d,i) => {
                                       d3.select(el).select("svg")
                                       .append("text")
                                       .attr("x", 30 + 250 * Math.floor(i/4))
                                       .attr("y", 550 + 20 * (i % 4))
                                       .attr("font-size",12)
                                       .attr("font-weight","bold")
                                       .attr("font-family", "calibri")
                                       .append("tspan")
                                       .text(labels1[i]);
                                       })
                                       }
                                       '
  )
  
  sankey_plot <- htmlwidgets::onRender(sankey_plot, plot_legend)
  
  plot_legend_2 <- paste(
    '
						   function(el) { ',
    '
						   var labels1 = ',
    rgb_collapse,
    ';
						   labels1.forEach((d,i) => {
						   d3.select(el).select("svg")
						   .append("rect")
						   .attr("x", 10 + 250 * Math.floor(i/4))
						   .attr("y", 541 + 20 * (i % 4))
						   .attr("width",10)
						   .attr("height",10)
						   .style("fill", labels1[i]);
						   })
						   }
						   '
  )
  
  sankey_plot <- htmlwidgets::onRender(sankey_plot, plot_legend_2)
  
  return(sankey_plot)
  
}
