FROM rocker/geospatial:4

WORKDIR /DSSAT47

RUN git clone https://github.com/DSSAT/dssat-csm-os.git && \
    cd dssat-csm-os && \
    mkdir build && \
    cd build && \
    cmake .. && \
    make
    
RUN git clone https://github.com/DSSAT/dssat-csm-data.git
    cp -r dssat-csm-os/Data/* dssat-csm-os/build/bin/ && \
    cp -r dssat-csm-os/Data/* /usr/local && \
    cp -r dssat-csm-data/* dssat-csm-os/build/bin/ && \
    cp -r dssat-csm-data/* /usr/local
    
COPY requirements.R .

RUN Rscript requirements.R
    