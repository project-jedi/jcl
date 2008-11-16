#pragma once
//-------------------------------------------------------------------------------------------------
// <copyright file="memutil.h" company="Microsoft">
//    Copyright (c) Microsoft Corporation.  All rights reserved.
//    
//    The use and distribution terms for this software are covered by the
//    Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
//    which can be found in the file CPL.TXT at the root of this distribution.
//    By using this software in any fashion, you are agreeing to be bound by
//    the terms of this license.
//    
//    You must not remove this notice, or any other, from this software.
// </copyright>
// 
// <summary>
//    Header for memory helper functions.
// </summary>
//-------------------------------------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif

#define ReleaseMem(p) if (p) { MemFree(p); }
#define ReleaseNullMem(p) if (p) { MemFree(p); p = NULL; }


HRESULT DAPI MemInitialize();
void DAPI MemUninitialize();

LPVOID DAPI MemAlloc(
    __in SIZE_T cbSize,
    __in BOOL fZero
    );
LPVOID DAPI MemReAlloc(
    __in LPVOID pv,
    __in SIZE_T cbSize,
    __in BOOL fZero
    );

HRESULT DAPI MemFree(
    __in LPVOID pv
    );
SIZE_T DAPI MemSize(
    __in LPVOID pv
    );

#ifdef __cplusplus
}
#endif

