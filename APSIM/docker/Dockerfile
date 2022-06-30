FROM rocker/geospatial:4

WORKDIR /APSIMX6868

ARG DEBIAN_FRONTEND=noninteractive

RUN apt update && apt -y upgrade && \
    apt -y install gnupg ca-certificates && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
    echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main" | tee /etc/apt/sources.list.d/mono-official-stable.list && \
    apt update && apt -y install mono-devel

RUN wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb && \
    dpkg -i packages-microsoft-prod.deb && \
    apt-get update && apt-get -y install apt-transport-https && \
    apt-get update && apt-get -y install dotnet-sdk-3.1 && \
    apt-get update && apt-get -y install libsqlite3-dev git libgtksourceview-4-0 gtk-sharp3 zenity

RUN wget http://apsimdev.apsim.info/ApsimXFiles/apsim-6868.deb && \
    dpkg -i apsim-6868.deb

COPY requirements.R .

RUN Rscript requirements.R

