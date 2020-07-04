choose.metrics = function(input = NULL){
  if(is.null(input)) return(list())
  metrics = c("anglex","angley","anglez",
              "bfen","bfx","bfy","bfz",
              "dev_roll_med_acc_x","dev_roll_med_acc_y","dev_roll_med_acc_z",
              "en","enmo","enmoa","hfen","hfenplus","hfx","hfy","hfz",
              "lfen","lfenmo","lfx","lfy","lfz",
              "mad","roll_med_acc_x","roll_med_acc_y","roll_med_acc_z")
  
  choosen = metrics %in% input
  
  return(list(
    anglex = choosen[which(metrics == "anglex")],
    angley = choosen[which(metrics == "angley")],
    anglez = choosen[which(metrics == "anglez")],
    bfen = choosen[which(metrics == "bfen")],
    bfx = choosen[which(metrics == "bfx")],
    bfy = choosen[which(metrics == "bfy")],
    bfz = choosen[which(metrics == "bfz")],
    dev_roll_med_acc_x = choosen[which(metrics == "dev_roll_med_acc_x")],
    dev_roll_med_acc_y = choosen[which(metrics == "dev_roll_med_acc_y")],
    dev_roll_med_acc_z = choosen[which(metrics == "dev_roll_med_acc_z")],
    en = choosen[which(metrics == "en")],
    enmo = choosen[which(metrics == "enmo")],
    enmoa = choosen[which(metrics == "enmoa")],
    hfen = choosen[which(metrics == "hfen")],
    hfenplus = choosen[which(metrics == "hfenplus")],
    hfx = choosen[which(metrics == "hfx")],
    hfy = choosen[which(metrics == "hfy")],
    hfz = choosen[which(metrics == "hfz")],
    lfen = choosen[which(metrics == "lfen")],
    lfenmo = choosen[which(metrics == "lfenmo")],
    lfx = choosen[which(metrics == "lfx")],
    lfy = choosen[which(metrics == "lfy")],
    lfz = choosen[which(metrics == "lfz")],
    mad = choosen[which(metrics == "mad")],
    roll_med_acc_x = choosen[which(metrics == "roll_med_acc_x")],
    roll_med_acc_y = choosen[which(metrics == "roll_med_acc_y")],
    roll_med_acc_z = choosen[which(metrics == "roll_med_acc_z")]
  ))
}
  
