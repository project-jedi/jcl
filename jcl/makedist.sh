./clean.sh
cd ..
tar --create --exclude-from=jcl/dist-excludes --file=jcl/dist/JCL$1.tar.gz jcl --gzip
cd jcl