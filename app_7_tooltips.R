
radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){

  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}


tooltip_startup_popup =
  "Welcome to watershedBC, an experimental research tool that is in <u>active development</u>.
          The tool is designed to summarize watershed data and estimate streamflow. Before you proceed,
          please read the following disclaimer:<br><br>
          <ul>
            <li><b><u>Prototype:</u></b> watershedBC is a prototype. There are multiple known issues and innacuracies that are actively being worked on.</li><br>
            <li><b><u>Not validated:</u></b> At this time, do not use any information from watershedBC for decision making.</li><br>
            <li><b><u>Frequent outages:</u></b> This tool is a proof of concept and will periodically be offline, freeze, or crash.</li><br>
            <li><b><u>User responsibility:</u></b> The User is responsible for the safe interpretation of the datasets presented.</li><br>
            <li><b><u>Open source:</u></b> The goal of this project is to be openly transparent via <a href='https://github.com/bcgov/watershedBC/', target='_blank'>https://github.com/bcgov/watershedBC/</a></li><br>
            <li><b><u>Speed:</u></b> This tool slows down with more concurent users. This will be fixed in future versions.</li><br>
            <li><b><u>Feedback:</u></b> Users can provide feedback here: <a href='https://github.com/bcgov/watershedBC/issues/', target='_blank'>https://github.com/bcgov/watershedBC/issues/</a></li><br>
          </ul>"
