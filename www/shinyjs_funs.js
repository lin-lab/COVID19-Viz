Shiny.addCustomMessageHandler("change_click", function(uid) {
  Shiny.setInputValue("map_main_shape_click_manual", {id: uid});
})
