create_folder <- function(mainDir, subDir) {
  if(!(dir.exists(file.path(subDir)))) {dir.create(file.path(mainDir, subDir))}
  else {print("directory already exists")}
}
