get_label.y = function(input = NULL){
  labels = c("Angle X","Angle Y","Angle Z",
             "BFEN","BF X","BF Y","BF Z",
             "Dev median X","Dev median Y","Dev median Z",
             "EN","ENMO","ENMOa","HFEN","HFEN+","HF X","HF Y","HF Z",
             "LFEN","LFENMO","LF X","LF Y","LF Z",
             "MAD","Median X","Median Y","Median Z")
  metrics = c("anglex","angley","anglez",
              "BFEN","bfx","bfy","bfz",
              "dev_roll_med_acc_x","dev_roll_med_acc_y","dev_roll_med_acc_z",
              "EN","ENMO","ENMOa","HFEN","HFENplus",
              "hfx","hfy","hfz",
              "LFEN","LFENMO","lfx","lfy","lfz",
              "MAD","roll_med_acc_x","roll_med_acc_y","roll_med_acc_z")

  if(input %in% c("anglex","angley","anglez")){
    unit = "(º)"
  } else {
    unit = "(G)"
  }
  label = paste(labels[which(metrics == input)],unit)
  return(label)
}