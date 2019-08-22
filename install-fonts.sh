#!/bin/sh

fonts_dir="${HOME}/.local/share/fonts"
if [ ! -d "${fonts_dir}" ]; then
    echo "mkdir -p $fonts_dir"
    mkdir -p "${fonts_dir}"
else
    echo "Found fonts dir $fonts_dir"
fi

for type in Bold Light Medium Regular Retina; do
    file_path="${HOME}/.local/share/fonts/FiraCode-${type}.ttf"
    file_url="https://github.com/tonsky/FiraCode/blob/master/distr/ttf/FiraCode-${type}.ttf?raw=true"
    if [ ! -e "${file_path}" ]; then
        echo "wget -O $file_path $file_url"
        wget -O "${file_path}" "${file_url}"
    else
	echo "Found existing file $file_path"
    fi;
done

cp fonts/* ~/.local/share/fonts/
cd /tmp

wget "https://github.com/IBM/type/archive/master.zip"

unzip master.zip
rm master.zip

cd plex-master

mkdir -p ~/.local/share/fonts/opentype/ibm-plex

cp /tmp/plex-master/IBM-Plex-Mono/fonts/complete/otf/*.otf ~/.local/share/fonts/opentype/ibm-plex/

fc-cache -fv
rm -rf /tmp/type-master
