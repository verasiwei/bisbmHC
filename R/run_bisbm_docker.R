#' Fit SBM within docker from pheSBMR



run_bisbm_docker <- function(data,
                             dockerfile_loc = system.file("Dockerfile", package = "bisbmHC"),
                             python_script_loc = system.file("run_bisbm.py", package = "bisbmHC")){
 
   node_a_name <- colnames(data)[1]
   node_b_name <- colnames(data)[2]
  
  # Set the column names to be generic for the model
  colnames(data) <- c('node_a', 'node_b')
  
  #create temporary directory to save the data, dockerfile and run_bisbm.py script
  tmp <- fs::dir_create(fs::file_temp())
  
  # Copy over the data and dockerfile and python script to the temp directory
  fs::file_copy(dockerfile_loc, fs::path(tmp, 'Dockerfile'))
  fs::file_copy(python_script_loc, fs::path(tmp, 'run_bisbm.py'))
  readr::write_csv(data, fs::path(tmp, 'model_data.csv'))
  
  #build docker container and run py script in docker
  build_container <- glue::glue('docker build --build-arg UID=$(id -u) -t graph_tool_custom {tmp}')
  system(build_container)
  run_container <- glue::glue('docker run -t -u analysis -v {tmp}:/home/analysis/data/ -w /home/analysis/ graph_tool_custom bash')
  system(run_container)
  
  # Extract the results
  node_to_clust <- readr::read_csv(fs::path(tmp, 'node_to_clust.csv')) %>%
    mutate(type = ifelse(type == 'node_a', node_a_name, node_b_name))
  
  # remove temp directory
  fs::dir_delete(tmp)
  
  # Give back results as a dataframe
  return(node_to_clust)
}



