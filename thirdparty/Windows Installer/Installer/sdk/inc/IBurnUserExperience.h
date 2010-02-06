//-------------------------------------------------------------------------------------------------
// <copyright file="IBurnUserExperience.h" company="Microsoft">
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
//      IBurnUserExperience, implemented by Burn UX and used by Burn engine/core.
// </summary>
//-------------------------------------------------------------------------------------------------

#pragma once


enum BURN_DISPLAY
{
    BURN_DISPLAY_UNKNOWN,
    BURN_DISPLAY_NONE,
    BURN_DISPLAY_PASSIVE,
    BURN_DISPLAY_FULL,
};


enum BURN_RESTART
{
    BURN_RESTART_UNKNOWN,
    BURN_RESTART_NEVER,
    BURN_RESTART_PROMPT,
    BURN_RESTART_AUTOMATIC,
    BURN_RESTART_ALWAYS,
};


enum BURN_RESUME_TYPE
{
    BURN_RESUME_TYPE_NONE,
    BURN_RESUME_TYPE_INVALID,        // resume information is present but invalid
    BURN_RESUME_TYPE_UNEXPECTED,     // relaunched after an unexpected interruption
    BURN_RESUME_TYPE_REBOOT_PENDING, // reboot has not taken place yet
    BURN_RESUME_TYPE_REBOOT,         // relaunched after reboot
    BURN_RESUME_TYPE_SUSPEND,        // relaunched after suspend
    BURN_RESUME_TYPE_ARP,            // launched from ARP
};


struct BURN_COMMAND
{
    BURN_ACTION action;
    BURN_DISPLAY display;
    BURN_RESTART restart;

    BOOL fResumed;
};


struct __declspec(novtable) IBurnUserExperience
{
public:
    virtual ~IBurnUserExperience() {}

    virtual HRESULT __stdcall Initialize(
        __in IBurnCore* pCore,
        __in int nCmdShow,
        __in BURN_RESUME_TYPE resumeType
        ) = 0;

    virtual HRESULT __stdcall Run() = 0;

    virtual void __stdcall Uninitialize() = 0;

    virtual int __stdcall OnDetectBegin(
        __in DWORD cPackages
        ) = 0;

    virtual int __stdcall OnDetectPackageBegin(
        __in_z LPCWSTR wzPackageId
        ) = 0;

    virtual void __stdcall OnDetectPackageComplete(
        __in LPCWSTR wzPackageId,
        __in HRESULT hrStatus,
        __in PACKAGE_STATE state
        ) = 0;

    virtual void __stdcall OnDetectComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual int __stdcall OnPlanBegin(
        __in DWORD cPackages
        ) = 0;

    virtual int __stdcall OnPlanPackageBegin(
        __in_z LPCWSTR wzPackageId,
        __inout_z REQUEST_STATE* pRequestedState
        ) = 0;

    virtual void __stdcall OnPlanPackageComplete(
        __in LPCWSTR wzPackageId,
        __in HRESULT hrStatus,
        __in PACKAGE_STATE state,
        __in REQUEST_STATE requested,
        __in ACTION_STATE execute,
        __in ACTION_STATE rollback
        ) = 0;

    virtual void __stdcall OnPlanComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual int __stdcall OnApplyBegin() = 0;

    virtual int __stdcall OnRegisterBegin() = 0;

    virtual void __stdcall OnRegisterComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual void __stdcall OnUnregisterBegin() = 0;

    virtual void __stdcall OnUnregisterComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual void __stdcall OnCacheComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual int __stdcall OnExecuteBegin(
        __in DWORD cExecutingPackages
        ) = 0;

    virtual int __stdcall  OnExecutePackageBegin(
        __in LPCWSTR wzPackageId,
        __in BOOL fExecute
        ) = 0;

    virtual int __stdcall OnError(
        __in LPCWSTR wzPackageId,
        __in DWORD dwCode,
        __in_z LPCWSTR wzError,
        __in DWORD dwUIHint
        ) = 0;

    virtual int __stdcall OnProgress(
        __in DWORD dwProgressPercentage,
        __in DWORD dwOverallPercentage
        ) = 0;

    virtual int __stdcall OnExecuteMsiMessage(
        __in_z LPCWSTR wzPackageId,
        __in INSTALLMESSAGE mt,
        __in UINT uiFlags,
        __in_z LPCWSTR wzMessage
        ) = 0;

    virtual int __stdcall OnExecuteMsiFilesInUse(
        __in_z LPCWSTR wzPackageId,
        __in DWORD cFiles,
        __in LPCWSTR* rgwzFiles
        ) = 0;

    virtual void __stdcall OnExecutePackageComplete(
        __in LPCWSTR wzPackageId,
        __in HRESULT hrExitCode
        ) = 0;

    virtual void __stdcall OnExecuteComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual BOOL __stdcall OnRestartRequired() = 0;

    virtual void __stdcall OnApplyComplete(
        __in HRESULT hrStatus
        ) = 0;

    virtual int __stdcall ResolveSource (
        __in    LPCWSTR wzPackageId ,
        __in    LPCWSTR wzPackageOrContainerPath
        ) = 0;

    virtual BOOL __stdcall CanPackagesBeDownloaded(void) = 0;

}; //struct IBurnUserExperience


extern "C" typedef HRESULT (WINAPI *PFN_CREATE_USER_EXPERIENCE)(
    __in BURN_COMMAND* pCommand,
    __out IBurnUserExperience** ppUX
    );
