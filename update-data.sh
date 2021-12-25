mkdir -p ./data/enemy
curl https://raw.githubusercontent.com/ElMustacho/catbot-v1.1/master/enemyunits.tsv --output ./data/enemyunits.tsv
curl https://raw.githubusercontent.com/ElMustacho/catbot-v1.1/master/stages10.2.db --output ./data/stages10.2.db

cd bcu-resources
git pull
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
