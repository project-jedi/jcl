@PUSHD install
@IF NOT "%1"=="" GOTO SetMake
@MAKE
@GOTO Finis
:SetMake
@%1
:Finis
@POPD
