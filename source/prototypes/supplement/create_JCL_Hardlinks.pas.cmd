@echo off
echo Continue by pressing ^<ENTER^> to overwrite the old JCL version of
echo this unit with a freshly created one (from prototype).
echo To cancel this operation press ^<CTRL+C^> or close this window!
echo.
echo.
pause
rem Remove the nonJCL parts
simple_pp.pl ..\Hardlinks.pas JCL > "..\..\windows\Hardlinks.pas"

