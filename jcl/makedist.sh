./clean.sh
cd ..
tar --create --exclude-from=jcl/dist-excludes --file=jcl/dist/JCL$1.tar.gz jcl --gzip
tar --create --file=jcl/dist/JCLx$1-Help.tar.gz jcl/help --gzip
cd jcl