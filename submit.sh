#!/bin/sh

SHA=$(git log -1 --format=%h)
URL=$(grep url .git/config | cut -d' ' -f3 | head -1)
VER=$(awk '/^Version:/ {print $2}' DESCRIPTION)
PKG=$(awk '/^Package:/ {print $2}' DESCRIPTION)

cd ..
R CMD build ${PKG}

git clone -b gh-pages https://github.com/ABbiodiversity/drat.git _tmpdrat
Rscript -e "drat::insertPackage('${PKG}_${VER}.tar.gz', '_tmpdrat')"
cd _tmpdrat
git add --all *
git commit -m "${PKG} ${VER} from ${URL} ${SHA}"
git push -q origin gh-pages

cd ..
rm -rf _tmpdrat
cd ${PKG}
