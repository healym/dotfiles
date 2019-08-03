#!/bin/bash

cp fonts/* ~/.local/share/fonts/
cd /tmp

# install unzip just in case the user doesn't already have it.
wget "https://github.com/IBM/type/archive/master.zip"

unzip master.zip
rm master.zip

cd plex-master

mkdir -p ~/.local/share/fonts/opentype/ibm-plex

cp /tmp/plex-master/IBM-Plex-Mono/fonts/complete/otf/*.otf ~/.local/share/fonts/

fc-cache -fv
rm -rf /tmp/type-master
