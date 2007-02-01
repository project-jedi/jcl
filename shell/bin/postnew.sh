# make our paths easy to use 
# export CVSROOT=:pserver:anonymous@cvs1:/cvsroot/jcl
export DAILYDIR=/home/groups/j/jc/jcl/htdocs/daily
export FILEHOME=/home/groups/j/jc/jcl/files
export DATESTRING=`date -I`

# delete old SVN folder
rm -rf $FILEHOME/jcl
cd $FILEHOME
# get the latest sources from SVN
# svn export -rHEAD --native-eol CRLF https://svn.sourceforge.net:443/svnroot/jcl/trunk/jcl jcl
# (outchy) get the revision number
svn export -rHEAD --native-eol CRLF https://jcl.svn.sourceforge.net:443/svnroot/jcl/trunk/jcl jcl | grep "Exported revision" > $FILEHOME/daily_revision.log
mv $FILEHOME/daily_revision.log $FILEHOME/jcl/daily_revision.log
# (outchy) copying template files
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jclc5.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jclc6.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jclkc3.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jclkd3.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld5.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld6.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld7.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jclcs1.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld8.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld9.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld9.net.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld10.inc
cp $FILEHOME/jcl/source/jcl.template.inc $FILEHOME/jcl/source/jcld10.net.inc
# (outchy) display the revision string
cat $FILEHOME/jcl/daily_revision.log
# (outchy) end modif

cd $FILEHOME/jcl
# remove unwanted file(s)
# echo "removing cvsignore"
# find  -type f -name .cvsignore -exec rm -rf {} \;
# convert LF to CRLF for text files

# the list of extensions to convert. 
#extlist=(*.pas *.dfm *.inc *.cpp *.hpp *.h *.dpr *.bpr *.dpk *.bpk *.bpg\
#         *.cfg *.template *.iss *.txt *.bat *.rc *.py *.dof)

# allow null globbing
# shopt -s nullglob

#echo "converting to dos format (CRLF)"
#handled by svn export --native-eol CRLF
# we use find to look for directories AND files because some of
# them may contain spaces which would be detected by for as a
# separator in the list to iterate, thus skipping the file
#find $FILEHOME/jcl -type d -print | while read SRCDIR 
#do
#  echo "Processing in $SRCDIR"
#  cd "$SRCDIR"
#
#  for FILE in ${extlist[@]}
#  do
#    if [[ -a $FILE ]] 
#    then
#      unix2dos -q $FILE
#    fi
#  done
#done

cd $FILEHOME/jcl
# create zip with all files and copy to daily
zip -rq jcl.zip .
#export DATESTRING=`date -I`
cp jcl.zip $DAILYDIR/jcl-$DATESTRING.zip
rm -f jcl.zip

# create a 7zip with all files and copy to daily
/home/groups/j/jc/jcl/bin/7zip a -bd -r jcl.7z . > /dev/null
cp jcl.7z $DAILYDIR/jcl-$DATESTRING.7z
rm -f jcl.7z

cd $DAILYDIR
# delete old zips (we only keep 3 at a time)
find . \( -mtime +3 -type f \) -exec rm -f {} \;
# link to latest full
# (outchy) remove write permissions to "others" and change group
# TODO: change default group for all new files
rm -f jcl-Latest.zip
ln -s jcl-$DATESTRING.zip jcl-Latest.zip
chmod g+w,o-w jcl-$DATESTRING.zip
chmod g+w,o-w jcl-Latest.zip
chgrp jcl jcl-$DATESTRING.zip
chgrp jcl jcl-Latest.zip

# link to latest full
rm -f jcl-Latest.7z
ln -s jcl-$DATESTRING.7z jcl-Latest.7z
chmod g+w,o-w jcl-$DATESTRING.7z
chmod g+w,o-w jcl-Latest.7z
chgrp jcl jcl-$DATESTRING.7z
chgrp jcl jcl-Latest.7z

rm -rf $FILEHOME/jcl
# done!

