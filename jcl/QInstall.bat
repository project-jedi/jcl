@PUSHD install
@IF NOT "%1"=="" GOTO SetMake
@MAKE clean qinstall
@GOTO Finis
:SetMake
@%1 clean qinstall
:Finis
@POPD
