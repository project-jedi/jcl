@echo cleaning...
@REM do not delete precompiled installer
@for %%f in (bin\*.exe) do @if not %%f==bin\JediInstaller.exe (del %%f)
@del /f /s *.~* *.bak *.bk bin\*.dll *.a *.bpi *.dcp *.dcu *.hpp *.jdbg *.map *.o
@cd lib
@del /f /s *.obj *.res *.lib *.bpi
@cd ..
@cd examples
@del /f /s *.cfg
@cd ..
@cd experts
@del /f /s *.cfg
@cd ..
@cd packages
@del /f /s *.cfg *.local *.identcache *.rsp
@cd..