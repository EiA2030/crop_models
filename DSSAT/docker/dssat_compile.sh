#!/bin/bash
apt-get update && apt-get -y install cmake
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR
rm -r -f dssat-csm-os/ && git clone https://github.com/DSSAT/dssat-csm-os.git && cd dssat-csm-os/ && chmod -R 777 ./
cd dssat-csm-os/ && cmake -P distclean.cmake
make distclean
mkdir build && cd build/
cmake ..
make -j 2
cd $SCRIPT_DIR
rm -r -f dssat-csm-data/ && git clone https://github.com/DSSAT/dssat-csm-data.git
cp -r dssat-csm-os/Data/* dssat-csm-os/build/bin/ && cp -r dssat-csm-os/Data/* /usr/local
cp -r dssat-csm-data/* dssat-csm-os/build/bin/ && cp -r dssat-csm-data/* /usr/local
# Below launches an R script which changes "/usr/local" to ". " (same directory) in the DSSATv47 Profile in Linux (DSSATPRO.L47)
Rscript ${SCRIPT_DIR}/dssat_r.R
exit
