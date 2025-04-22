cp /home/jwyang/WDK-VENUS/new/venus-E.e ./
cp /home/jwyang/WDK-VENUS/ReadFort-analysis/readfort2628.py ./
 
mkdir -p ./input_09-inpark/0001
cp inp0001 ./input_09-inpark/0001
cp venus-E.e ./input_09-inpark/0001
cp run0001 ./input_09-inpark/0001
 
cp ./sbatch.sc ./input_09-inpark
cp ./input_09 ./input_09-inpark/input
mv ./readfort2628.py ./input_09-inpark
cd input_09-inpark
chmod +x sbatch.sc
./sbatch.sc > scancel.x
sed -i 's/^.\{19\}/scancel /g' scancel.x
chmod +x scancel.x
