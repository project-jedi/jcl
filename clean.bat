@echo cleaning...
@REM do not delete precompiled installer
@ren bin\*Installer.exe *Installer.ex_
@del /f /s *.~* bin\*.exe bin\*.dll *.a *.bpi *.dcp *.dcu *.dpu *.hpp *.jdbg *.map *.o 
@ren bin\*Installer.ex_ *Installer.exe
@cd lib
@del /f /s *.obj *.res
@cd ..
