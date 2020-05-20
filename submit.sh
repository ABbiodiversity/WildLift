#!/bin/sh

cd ..
R CMD build WildLift
PKGNAME=$(ls -1t *.tar.gz | head -n 1)

git clone -b gh-pages https://github.com/ABbiodiversity/drat.git _tmpdrat
Rscript -e "drat::insertPackage('${PKGNAME}', '_tmpdrat')"
cd _tmpdrat
git add --all *
git commit -m "Update ${PKGNAME}"
git push -q origin gh-pages

