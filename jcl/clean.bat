@echo cleaning...
@del /f /s *.~* bin\*.exe bin\*.dll *.a *.bpi *.dcp *.dcu *.dpu *.hpp *.jdbg *.map *.o 
@cd lib
@del /f /s *.obj *.res
@cd ..
