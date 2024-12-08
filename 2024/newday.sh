daynum="$(printf "%02d" $1)"
mkdir -p ./data/"$daynum"/
touch ./data/"$daynum"/{example.txt,data.txt}

cp ./src/template.hs ./src/"day${daynum}.hs"

echo "Copy the example to the clipboard, then press ENTER"

read

pbpaste > ./data/"$daynum"/example.txt

echo "Copy the actual data, then press ENTER"

read

pbpaste > ./data/"$daynum"/data.txt
