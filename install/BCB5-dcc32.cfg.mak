#--------------------------------------------------------------------------------------------------#
#                                                                                                  #
# JCL Install Helper for BCB 5                                                                     #
#                                                                                                  #
# Fixes problem with missing AccCtrl.dcu:                                                          #
#   if Bin\dcc32.cfg does not exist, creates it & adds library paths.                              #
#   if Bin\dcc32.cfg doesn't contain -LU"$(ROOT)\Lib\Obj\vcl50.dcp", inserts it.                   #
#                                                                                                  #
# Robert Rossmair, 2004-06-10                                                                      #
#                                                                                                  #
#--------------------------------------------------------------------------------------------------#

!ifndef ROOT
ROOT = $(MAKEDIR)\..
!endif
#---------------------------------------------------------------------------------------------------
DccCfg = $(MAKEDIR)\dcc32.cfg

$(ROOT)\Lib\Obj\AccCtrl.dcu:
	@if exist $(DccCfg) (if not exist $(DccCfg).bak copy $(DccCfg) $(DccCfg).bak) else echo -u"$(ROOT)\Lib";"$(ROOT)\Lib\Obj" > $(DccCfg)
	@if not exist "$(ROOT)\Lib\Obj\vcl50.dcp" goto Finis
	-@$(MAKEDIR)\grep -i+ vcl50 $(DccCfg)
	@if errorlevel 1 echo -LUvcl50 >> $(DccCfg)
	@:Finis
	
.precious: $(ROOT)\Lib\Obj\AccCtrl.dcu