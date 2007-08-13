[Components]
; IDE selection
Name: "IDE"; Description: "Install for IDE"
#ifdef Include_Delphi5
Name: "IDE\Delphi5"; Description: "Install for Delphi 5"; Types: full prefered; Check: IsDelphiInstalled(5)
#endif
#ifdef Include_BCB5
Name: "IDE\BCB5"; Description: "Install for C++Builder 5"; Types: full prefered; Check: IsBCBInstalled(5)
#endif
#ifdef Include_Delphi6
Name: "IDE\Delphi6"; Description: "Install for Delphi 6"; Types: full prefered; Check: IsDelphiInstalled(6)
#endif
#ifdef Include_BCB6
Name: "IDE\BCB6"; Description: "Install for C++Builder 6"; Types: full prefered; Check: IsBCBInstalled(6)
#endif
#ifdef Include_Delphi7
Name: "IDE\Delphi7"; Description: "Install for Delphi 7"; Types: full prefered; Check: IsDelphiInstalled(7)
#endif
#ifdef Include_Delphi9
Name: "IDE\Delphi9"; Description: "Install for Delphi 2005"; Types: full prefered; Check: IsDelphiInstalled(9)
#endif
#ifdef Include_Delphi10
Name: "IDE\Delphi10"; Description: "Install for Delphi/C++Builder 2006"; Types: full prefered; Check: IsDelphiInstalled(10)
#endif
#ifdef Include_Delphi11
Name: "IDE\Delphi11"; Description: "Install for RAD Studio 2007"; Types: full prefered; Check: IsDelphiInstalled(11)
#endif

