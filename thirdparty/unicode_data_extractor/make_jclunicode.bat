echo off

rem compile UDExtract.dpr
dcc32 UDExtract.dpr -U..\..\jcl\source\windows;..\..\jcl\source\common -I..\..\jcl\source -N0. -E.

rem execute UDExtract.dpr
UDExtract.exe UnicodeData.txt JclUnicode.rc /c=SpecialCasing.txt /f=CaseFolding.txt

rem copying JclUnicode.rc
copy JclUnicode.rc + Composition.rc ..\..\jcl\source\windows\JclUnicode.rc

rem compiling JclUnicode.rc
cd ..\..\jcl\source\windows
brcc32 JclUnicode.rc -foJclUnicode.res
cd ..\..\..\thirdparty\unicode_data_extractor
