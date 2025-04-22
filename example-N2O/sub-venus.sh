CurDir=$PWD

for i in `cat 1`; 
do echo $i;

mkdir $i-traj
cp exe $i-traj
mv $i $i-traj

cd $i-traj
./exe $i &
cd ..

sleep 2

done
