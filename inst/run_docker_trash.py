#!/usr/bin/env python3

import os
import numpy as np
import pandas as pd
import tempfile
import shutil

def run_bisbm(data,
  dockerfile_loc = system.file("inst/Dockerfile", package = "bisbmHC"),
  python_script_loc = system.file("inst/run_bisbm_and_save.py", package = "bisbmHC")):
  

#create a temporary folder to save the input data and docker scripts and run_bisbm scripts
tmpdir = tempfile.mkdtemp()
shutil.copy("/home/siwei/bisbmHC/inst/Dockerfile", tmpdir)
shutil.copy("/home/siwei/bisbmHC/inst/run_bisbm.py", tmpdir)
shutil.copy("/home/siwei/bisbmHC/data/JAK2_phenome_long.csv", tmpdir)

#build docker container and run bisbm scripts in docker container
os.system('docker build --build-arg UID=$(id -u) -t graph_tool_custom %s' % (tmpdir))
os.system('docker run -t -u analysis -v %s:/home/analysis/data/ -w /home/analysis/ graph_tool_custom bash' % (tmpdir))

# Extract the results
node_to_cluster = pd.read_csv("%s/node_to_cluster.csv" % (tmpdir), dtype = 'str')
node_to_cluster.to_csv('/home/siwei/bisbmHC/data/node_to_cluster.csv')


shutil.rmtree(tmpdir)



