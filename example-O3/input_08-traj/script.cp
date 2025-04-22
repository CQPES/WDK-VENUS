cp /home/jwyang/WDK-VENUS/new/venus-E.e ./
cp /home/jwyang/WDK-VENUS/ReadFort-analysis/readfort2628.py ./
 
mkdir -p ./input_08-inpark/0001
cp inp0001 ./input_08-inpark/0001
cp venus-E.e ./input_08-inpark/0001
cp run0001 ./input_08-inpark/0001
 
cp ./sbatch.sc ./input_08-inpark
cp ./input_08 ./input_08-inpark/input
mv ./readfort2628.py ./input_08-inpark
cd input_08-inpark
chmod +x sbatch.sc
./sbatch.sc > scancel.x
sed -i 's/^.\{19\}/scancel /g' scancel.x
chmod +x scancel.x
