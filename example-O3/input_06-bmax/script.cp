cp /home/jwyang/O3-test-QCT/new/venus-E.e ./
cp /home/jwyang/O3-test-QCT/ReadFort-analysis/readfort2628.py ./
 
mkdir -p ./input_06-inpark/0001
cp inp0001 ./input_06-inpark/0001
cp venus-E.e ./input_06-inpark/0001
cp run0001 ./input_06-inpark/0001
 
cp ./sbatch.sc ./input_06-inpark
cp ./input_06 ./input_06-inpark/input
mv ./readfort2628.py ./input_06-inpark
cd input_06-inpark
chmod +x sbatch.sc
./sbatch.sc > scancel.x
sed -i 's/^.\{19\}/scancel /g' scancel.x
chmod +x scancel.x
