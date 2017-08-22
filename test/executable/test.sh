set -e
cd $(dirname $0)
rootdir=../../
exampledir=$rootdir/example
n0un=$rootdir/src/n0un.opt
echo "rootdir=${rootdir}"
echo "exampledir=${exampledir}"
echo "n0un=${n0un}"
for file in $(find $exampledir -maxdepth 1 -name '*.n0un'); do
  filename=$(basename $file)
  echo "filename=${filename}"
  $n0un $file | diff - "${filename%.*}.output"
done
