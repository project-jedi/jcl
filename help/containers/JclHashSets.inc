(*$JPPDEFINEMACRO JCLHASHSETINT(SELFCLASSNAME, COLLECTIONINTERFACENAME, MAPINTERFACENAME, TYPENAME)
@@SELFCLASSNAME
<copy TJclHashSet>
\ \

@@!!MEMBEROVERVIEW_SELFCLASSNAME
<copy !!MEMBEROVERVIEW_TJclHashSet>
\ \

@@SELFCLASSNAME.Add@TYPENAME
<copy TJclHashSet.Add@TObject>
\ \

@@SELFCLASSNAME.AddAll@COLLECTIONINTERFACENAME
<copy TJclHashSet.AddAll@IJclCollection>
\ \

@@SELFCLASSNAME.AssignDataTo@TJclAbstractContainerBase
<copy TJclHashSet.AssignDataTo@TJclAbstractContainerBase>
\ \

@@SELFCLASSNAME.Clear
<copy TJclHashSet.Clear>
\ \

@@SELFCLASSNAME.CollectionEquals@COLLECTIONINTERFACENAME
<copy TJclHashSet.CollectionEquals@IJclCollection>
\ \

@@SELFCLASSNAME.Contains@TYPENAME
<copy TJclHashSet.Contains@TObject>
\ \

@@SELFCLASSNAME.ContainsAll@COLLECTIONINTERFACENAME
<copy TJclHashSet.ContainsAll@IJclCollection>
\ \

@@SELFCLASSNAME.Create@MAPINTERFACENAME
<copy TJclHashSet.Create@IJclMap>
\ \

@@SELFCLASSNAME.Create@Integer
<copy TJclHashSet.Create@Integer@Boolean>
\ \

@@SELFCLASSNAME.CreateEmptyContainer
<copy TJclHashSet.CreateEmptyContainer>
\ \

@@SELFCLASSNAME.Destroy
<copy TJclHashSet.Destroy>
\ \

@@SELFCLASSNAME.Extract@TYPENAME
<copy TJclHashSet.Extract@TObject>
\ \

@@SELFCLASSNAME.ExtractAll@COLLECTIONINTERFACENAME
<copy TJclHashSet.ExtractAll@IJclCollection>
\ \

@@SELFCLASSNAME.First
<copy TJclHashSet.First>
\ \

@@SELFCLASSNAME.GetAllowDefaultElements
<copy TJclHashSet.GetAllowDefaultElements>
\ \

@@SELFCLASSNAME.GetAutoPackParameter
<copy TJclHashSet.GetAutoPackParameter>
\ \

@@SELFCLASSNAME.GetAutoPackStrategy
<copy TJclHashSet.GetAutoPackStrategy>
\ \

@@SELFCLASSNAME.GetCapacity
<copy TJclHashSet.GetCapacity>
\ \

@@SELFCLASSNAME.GetDuplicates
<copy TJclHashSet.GetDuplicates>
\ \

@@SELFCLASSNAME.GetEnumerator
<copy TJclHashSet.GetEnumerator>
\ \

@@SELFCLASSNAME.GetReadOnly
<copy TJclHashSet.GetReadOnly>
\ \

@@SELFCLASSNAME.GetRemoveSingleElement
<copy TJclHashSet.GetRemoveSingleElement>
\ \

@@SELFCLASSNAME.GetReturnDefaultElements
<copy TJclHashSet.GetReturnDefaultElements>
\ \

@@SELFCLASSNAME.GetThreadSafe
<copy TJclHashSet.GetThreadSafe>
\ \

@@SELFCLASSNAME.Intersect@COLLECTIONINTERFACENAME
<copy TJclHashSet.Intersect@IJclCollection>
\ \

@@SELFCLASSNAME.IsEmpty
<copy TJclHashSet.IsEmpty>
\ \

@@SELFCLASSNAME.Last
<copy TJclHashSet.Last>
\ \

@@SELFCLASSNAME.Pack
<copy TJclHashSet.Pack>
\ \

@@SELFCLASSNAME.Remove@TYPENAME
<copy TJclHashSet.Remove@TObject>
\ \

@@SELFCLASSNAME.RemoveAll@COLLECTIONINTERFACENAME
<copy TJclHashSet.RemoveAll@IJclCollection>
\ \

@@SELFCLASSNAME.RetainAll@COLLECTIONINTERFACENAME
<copy TJclHashSet.RetainAll@IJclCollection>
\ \

@@SELFCLASSNAME.SetAllowDefaultElements@Boolean
<copy TJclHashSet.SetAllowDefaultElements@Boolean>
\ \

@@SELFCLASSNAME.SetAutoPackParameter@Integer
<copy TJclHashSet.SetAutoPackParameter@Integer>
\ \

@@SELFCLASSNAME.SetAutoPackStrategy@TJclAutoPackStrategy
<copy TJclHashSet.SetAutoPackStrategy@TJclAutoPackStrategy>
\ \

@@SELFCLASSNAME.SetCapacity@Integer
<copy TJclHashSet.SetCapacity@Integer>
\ \

@@SELFCLASSNAME.SetDuplicates@TDuplicates
<copy TJclHashSet.SetDuplicates@TDuplicates>
\ \

@@SELFCLASSNAME.SetReadOnly@Boolean
<copy TJclHashSet.SetReadOnly@Boolean>
\ \

@@SELFCLASSNAME.SetRemoveSingleElement@Boolean
<copy TJclHashSet.SetRemoveSingleElement@Boolean>
\ \

@@SELFCLASSNAME.SetReturnDefaultElements@Boolean
<copy TJclHashSet.SetReturnDefaultElements@Boolean>
\ \

@@SELFCLASSNAME.SetThreadSafe@Boolean
<copy TJclHashSet.SetThreadSafe@Boolean>
\ \

@@SELFCLASSNAME.Size
<copy TJclHashSet.Size>
\ \

@@SELFCLASSNAME.Subtract@COLLECTIONINTERFACENAME
<copy TJclHashSet.Subtract@IJclCollection>
\ \

@@SELFCLASSNAME.Union@COLLECTIONINTERFACENAME
<copy TJclHashSet.Union@IJclCollection>
\ \
*)
{$JPPEXPANDMACRO JCLHASHSETINT(TJclIntfHashSet,IJclIntfCollection,IJclIntfMap,IInterface)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclAnsiStrHashSet,IJclAnsiStrCollection,IJclAnsiStrMap,AnsiString)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclWideStrHashSet,IJclWideStrCollection,IJclWideStrMap,WideString)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclUnicodeStrHashSet,IJclUnicodeStrCollection,IJclUnicodeStrMap,UnicodeString)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclSingleHashSet,IJclSingleCollection,IJclSingleMap,Single)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclDoubleHashSet,IJclDoubleCollection,IJclDoubleMap,Double)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclExtendedHashSet,IJclExtendedCollection,IJclExtendedMap,Extended)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclIntegerHashSet,IJclIntegerCollection,IJclIntegerMap,Integer)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclCardinalHashSet,IJclCardinalCollection,IJclCardinalMap,Cardinal)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclInt64HashSet,IJclInt64Collection,IJclInt64Map,Int64)}
{$JPPEXPANDMACRO JCLHASHSETINT(TJclPtrHashSet,IJclPtrCollection,IJclPtrMap,Pointer)}
