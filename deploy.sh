cabal build
cabal run site clean
cabal run site build

cd deploy
git checkout main
git pull

cp -a ../_site/. .

git add -A
git commit -m "Publish."
git push origin main:main

cd ..
