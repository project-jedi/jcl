To create the release zips for JCL, proceed as follows:
1. Create a release branch (JCl_X.Y) and switch to it
2. Retrieve the day number from the daily website: https://jcl.sourceforge.net/daily/ (ie, 9330 for 2025/09/15), this will be BuildNumber in the lines below
3. ReleaseNumber is set to 1 for an official release
4. Edit jcl/devtools/pgEdit.xml to change ReleaseNumber and BuildNumber
5. Edit jcl/docs/Readme.html and Readme.txt to change the release Build number and date
6. Edit jcl/source/common/JclBase.pas to set JclVersionRelease to ReleaseNumber and JclVersionBuild to BuildNumber
7. Edit thirdparty/makedist/winscp-jcl-files.txt to use the proper ReleaseNumber and BuildNumber in its URL
8. Regenerate the packages with jcl/devtools/pgedit.exe  (compiled from JVCL devtools)
9. In the jcl/packages folder, run "make -f resources.mak"
10. Git commit
* Follow instructions in thirdparty/makedist/howto-release.txt up to the point where makedist is to be called
** execute the following command:
*** makedist /c=JclFiles.xml /a /s=Jcl-win,Jcl-linux,Jcl-win.sync,Jcl-linux.sync /x
** DMCC and JEDIHELP do not need to be set, if you have access to the Doc-o-Matic command line tool, you may manually create the help files

* Git push
* Create tag JCL-X.Y-BuildZZZZ
* Checkout the "master" branch
* Follow steps 4 to 9 with the following values: Major/Minor = Expected next version number, ReleaseNumber = 0, BuildNumber = release build number + 1
* Git commit
* Git push

