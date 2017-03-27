cd ./src
csc -c -d3 *.scm
rm compiler.o
csc compiler.scm *.o
mv compiler ../
rm ./*.o
