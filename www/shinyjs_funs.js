Shiny.addCustomMessageHandler("defaultResolution", function(res) {
  Shiny.setInputValue("select_resolution", res);
})
