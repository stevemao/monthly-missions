set -e

mkdir -p ./data/enemy
curl --fail https://raw.githubusercontent.com/ElMustacho/catbot-v1.1/master/enemyunits.tsv --output ./data/enemyunits.tsv
curl --fail https://raw.githubusercontent.com/ElMustacho/catbot-v1.1/master/stages.db --output ./data/stages.db

git submodule init
git submodule update
cd bcu-resources
cd resources/assets
for FILE in *; 
    do unzip -o $FILE; 
done
cd ../../..
mv bcu-resources/resources/assets/org/enemy/**/enemy_icon_*.png ./data/enemy
cd bcu-resources
git clean -fdx
cd ..

stack run migrations
./install.sh
