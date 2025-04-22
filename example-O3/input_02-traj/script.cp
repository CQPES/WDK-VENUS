cp /home/jwyang/WDK-VENUS/new/venus-E.e ./
cp /home/jwyang/WDK-VENUS/ReadFort-analysis/readfort2628.py ./
 
mkdir -p ./input_02-inpark/0001
cp inp0001 ./input_02-inpark/0001
cp venus-E.e ./input_02-inpark/0001
cp run0001 ./input_02-inpark/0001
 
cp ./sbatch.sc ./input_02-inpark
cp ./input_02 ./input_02-inpark/input
mv ./readfort2628.py ./input_02-inpark
cd input_02-inpark
chmod +x sbatch.sc
./sbatch.sc > scancel.x
sed -i 's/^.\{19\}/scancel /g' scancel.x
chmod +x scancel.x
