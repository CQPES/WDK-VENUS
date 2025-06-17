cp /home/jwyang/O3-test-QCT/new/venus-E.e ./
cp /home/jwyang/O3-test-QCT/ReadFort-analysis/readfort2628.py ./
 
mkdir -p ./input_10-inpark/0001
cp inp0001 ./input_10-inpark/0001
cp venus-E.e ./input_10-inpark/0001
cp run0001 ./input_10-inpark/0001
 
cp ./sbatch.sc ./input_10-inpark
cp ./input_10 ./input_10-inpark/input
mv ./readfort2628.py ./input_10-inpark
cd input_10-inpark
chmod +x sbatch.sc
./sbatch.sc > scancel.x
sed -i 's/^.\{19\}/scancel /g' scancel.x
chmod +x scancel.x
