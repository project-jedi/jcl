@echo off

SET PATH=E:\Borland\Delphi7\Bin

dcc32 -U..\..\JCL\source\Common;..\..\JCL\source\Windows -I..\..\JCL\source\Include -Q -$D- -E.. CompInstall.dpr

pause