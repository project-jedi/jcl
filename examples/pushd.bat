@echo @prompt cd $P > $$$.bat
@command /E:1024 /C $$$.bat > %1\popd.bat
@echo @del %1\popd.bat >> %1\popd.bat
@del $$$.bat
cd %1