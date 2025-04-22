CurDir=$PWD

for i in `cat 1`; 
do echo $i;

mkdir $i-bmax
cp exe $i-bmax
mv $i $i-bmax

cd $i-bmax
./exe $i &
cd ..

sleep 2

done
