    StartPackage('libc');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/libc';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='libc';
    {$ENDIF}
    OS:=[linux];
    T:=Targets.AddUnit('kerneldefs');
    T:=Targets.AddUnit('kernelioctl');
    T:=Targets.AddUnit('libc');
    EndPackage;
