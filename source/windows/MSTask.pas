{******************************************************************************}
{                                                                              }
{  Microsoft Task Scheduler API definitions interface unit                     }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS" basis,  }
{  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License    }
{  for the specific language governing rights and limitations under the        }
{  License.                                                                    }
{                                                                              }
{  The Original Code is: MSTask.h and MSTask.idl.                              }
{  The Initial Developer of the Original Code is Microsoft. Portions created   }
{  by Microsoft are Copyright (C) 1992 - 1999 Microsoft Corporation. All       }
{  Rights Reserved.                                                            }
{                                                                              }
{  The Original Pascal code is: MSTask.pas.                                    }
{  The Initial Developer of the Original Pascal code is Peter J. Haas.         }
{  Portions created by Peter J. Haas are Copyright (C) 2004 Peter J. Haas.     }
{  All Rights Reserved.                                                        }
{                                                                              }
{  You may retrieve the latest version of this file at the homepage of         }
{  JEDI+ (jediplus att pjh2 dott de), located at http://jediplus.pjh2.de/      }
{                                                                              }
{------------------------------------------------------------------------------}
{                                                                              }
{  NOTE: As of 2004-05-15, Peter J. Haas has stopped maintaining code he       }
{        donated to the JCL. He is not to be held responsible for              }
{        modifications applied after this date.                                }
{        Peter J. Haas no longer wants to be associated with Project JEDI.     }
{                                                                              }
{------------------------------------------------------------------------------}
{                                                                              }
{  Contributor(s):                                                             }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the GNU Lesser General Public License (the  "LGPL License"), in which case  }
{  the provisions of the LGPL License are applicable instead of those above.   }
{  If you wish to allow use of your version of this file only under the terms  }
{  of the LGPL License and not to allow others to use your version of this     }
{  file under the MPL, indicate your decision by deleting the provisions       }
{  above and replace them with the notice and other provisions required by     }
{  the LGPL License. If you do not delete the provisions above, a recipient    }
{  may use your version of this file under either the MPL or the LGPL License. }
{                                                                              }
{  For more information about the LGPL:                                        }
{  http://www.gnu.org/copyleft/lesser.html                                     }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  2004-04-01:                                                                 }
{    - SDK 2003-02, Windows Server 2003                                        }
{                                                                              }
{******************************************************************************}

{$I jcl.inc}
{$I windowsonly.inc}

unit MSTask;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

{$IFDEF SUPPORTS_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF SUPPORTS_DEPRECATED}

interface

uses
  Windows, ActiveX;
  
(*$HPPEMIT '' *)
(*$HPPEMIT '#include <MSTask.h>' *)
(*$HPPEMIT '' *)

//+----------------------------------------------------------------------------
//
//  Task Scheduler
//
//  Microsoft Windows
//  Copyright (C) Microsoft Corporation, 1992 - 1999.
//
//  File:       mstask.idl
//
//  Contents:   ITaskTrigger, ITask, ITaskScheduler, IEnumWorkItems
//              interfaces and related definitions
//
//  History:    06-Sep-95 EricB created
//
//-----------------------------------------------------------------------------

// 148BD520-A2AB-11CE-B11F-00AA00530503 - Task object class ID
// 148BD52A-A2AB-11CE-B11F-00AA00530503 - Task Scheduler class ID
// A6B952F0-A4B1-11D0-997D-00AA006887EC - IScheduledWorkItem interface ID
// 148BD524-A2AB-11CE-B11F-00AA00530503 - ITask interface ID
// 148BD527-A2AB-11CE-B11F-00AA00530503 - ITaskScheduler interface ID
// 148BD528-A2AB-11CE-B11F-00AA00530503 - IEnumWorkItems interface ID
// 148BD52B-A2AB-11CE-B11F-00AA00530503 - ITaskTrigger interface ID

//+----------------------------------------------------------------------------
//
//  Datatypes
//
//-----------------------------------------------------------------------------

const
  {$EXTERNALSYM TASK_SUNDAY}
  TASK_SUNDAY      = $1;
  {$EXTERNALSYM TASK_MONDAY}
  TASK_MONDAY      = $2;
  {$EXTERNALSYM TASK_TUESDAY}
  TASK_TUESDAY     = $4;
  {$EXTERNALSYM TASK_WEDNESDAY}
  TASK_WEDNESDAY   = $8;
  {$EXTERNALSYM TASK_THURSDAY}
  TASK_THURSDAY    = $10;
  {$EXTERNALSYM TASK_FRIDAY}
  TASK_FRIDAY      = $20;
  {$EXTERNALSYM TASK_SATURDAY}
  TASK_SATURDAY    = $40;
  {$EXTERNALSYM TASK_FIRST_WEEK}
  TASK_FIRST_WEEK  = 1;
  {$EXTERNALSYM TASK_SECOND_WEEK}
  TASK_SECOND_WEEK = 2;
  {$EXTERNALSYM TASK_THIRD_WEEK}
  TASK_THIRD_WEEK  = 3;
  {$EXTERNALSYM TASK_FOURTH_WEEK}
  TASK_FOURTH_WEEK = 4;
  {$EXTERNALSYM TASK_LAST_WEEK}
  TASK_LAST_WEEK   = 5;
  {$EXTERNALSYM TASK_JANUARY}
  TASK_JANUARY     = $1;
  {$EXTERNALSYM TASK_FEBRUARY}
  TASK_FEBRUARY    = $2;
  {$EXTERNALSYM TASK_MARCH}
  TASK_MARCH       = $4;
  {$EXTERNALSYM TASK_APRIL}
  TASK_APRIL       = $8;
  {$EXTERNALSYM TASK_MAY}
  TASK_MAY         = $10;
  {$EXTERNALSYM TASK_JUNE}
  TASK_JUNE        = $20;
  {$EXTERNALSYM TASK_JULY}
  TASK_JULY        = $40;
  {$EXTERNALSYM TASK_AUGUST}
  TASK_AUGUST      = $80;
  {$EXTERNALSYM TASK_SEPTEMBER}
  TASK_SEPTEMBER   = $100;
  {$EXTERNALSYM TASK_OCTOBER}
  TASK_OCTOBER     = $200;
  {$EXTERNALSYM TASK_NOVEMBER}
  TASK_NOVEMBER    = $400;
  {$EXTERNALSYM TASK_DECEMBER}
  TASK_DECEMBER    = $800;

  {$EXTERNALSYM TASK_FLAG_INTERACTIVE}
  TASK_FLAG_INTERACTIVE                  = $1;
  {$EXTERNALSYM TASK_FLAG_DELETE_WHEN_DONE}
  TASK_FLAG_DELETE_WHEN_DONE             = $2;
  {$EXTERNALSYM TASK_FLAG_DISABLED}
  TASK_FLAG_DISABLED                     = $4;
  {$EXTERNALSYM TASK_FLAG_START_ONLY_IF_IDLE}
  TASK_FLAG_START_ONLY_IF_IDLE           = $10;
  {$EXTERNALSYM TASK_FLAG_KILL_ON_IDLE_END}
  TASK_FLAG_KILL_ON_IDLE_END             = $20;
  {$EXTERNALSYM TASK_FLAG_DONT_START_IF_ON_BATTERIES}
  TASK_FLAG_DONT_START_IF_ON_BATTERIES   = $40;
  {$EXTERNALSYM TASK_FLAG_KILL_IF_GOING_ON_BATTERIES}
  TASK_FLAG_KILL_IF_GOING_ON_BATTERIES   = $80;
  {$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_DOCKED}
  TASK_FLAG_RUN_ONLY_IF_DOCKED           = $100;
  {$EXTERNALSYM TASK_FLAG_HIDDEN}
  TASK_FLAG_HIDDEN                       = $200;
  {$EXTERNALSYM TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET}
  TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET = $400;
  {$EXTERNALSYM TASK_FLAG_RESTART_ON_IDLE_RESUME}
  TASK_FLAG_RESTART_ON_IDLE_RESUME       = $800;
  {$EXTERNALSYM TASK_FLAG_SYSTEM_REQUIRED}
  TASK_FLAG_SYSTEM_REQUIRED              = $1000;
  {$EXTERNALSYM TASK_FLAG_RUN_ONLY_IF_LOGGED_ON}
  TASK_FLAG_RUN_ONLY_IF_LOGGED_ON        = $2000;
  {$EXTERNALSYM TASK_TRIGGER_FLAG_HAS_END_DATE}
  TASK_TRIGGER_FLAG_HAS_END_DATE         = $1;
  {$EXTERNALSYM TASK_TRIGGER_FLAG_KILL_AT_DURATION_END}
  TASK_TRIGGER_FLAG_KILL_AT_DURATION_END = $2;
  {$EXTERNALSYM TASK_TRIGGER_FLAG_DISABLED}
  TASK_TRIGGER_FLAG_DISABLED             = $4;

//
// 1440 = 60 mins/hour * 24 hrs/day since a trigger/TASK could run all day at
// one minute intervals.
//
const
  {$EXTERNALSYM TASK_MAX_RUN_TIMES}
  TASK_MAX_RUN_TIMES = 1440;

//
// The TASK_TRIGGER_TYPE field of the TASK_TRIGGER structure determines
// which member of the TRIGGER_TYPE_UNION field to use.
//
type
  {$EXTERNALSYM _TASK_TRIGGER_TYPE}
  _TASK_TRIGGER_TYPE = (
    {$EXTERNALSYM TASK_TIME_TRIGGER_ONCE}
    TASK_TIME_TRIGGER_ONCE,             // Ignore the Type field.
    {$EXTERNALSYM TASK_TIME_TRIGGER_DAILY}
    TASK_TIME_TRIGGER_DAILY,            // Use DAILY
    {$EXTERNALSYM TASK_TIME_TRIGGER_WEEKLY}
    TASK_TIME_TRIGGER_WEEKLY,           // Use WEEKLY
    {$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDATE}
    TASK_TIME_TRIGGER_MONTHLYDATE,      // Use MONTHLYDATE
    {$EXTERNALSYM TASK_TIME_TRIGGER_MONTHLYDOW}
    TASK_TIME_TRIGGER_MONTHLYDOW,       // Use MONTHLYDOW
    {$EXTERNALSYM TASK_EVENT_TRIGGER_ON_IDLE}
    TASK_EVENT_TRIGGER_ON_IDLE,         // Ignore the Type field.
    {$EXTERNALSYM TASK_EVENT_TRIGGER_AT_SYSTEMSTART}
    TASK_EVENT_TRIGGER_AT_SYSTEMSTART,  // Ignore the Type field.
    {$EXTERNALSYM TASK_EVENT_TRIGGER_AT_LOGON}
    TASK_EVENT_TRIGGER_AT_LOGON);       // Ignore the Type field.
  {$EXTERNALSYM TASK_TRIGGER_TYPE}
  TASK_TRIGGER_TYPE = _TASK_TRIGGER_TYPE;
  {$EXTERNALSYM PTASK_TRIGGER_TYPE}
  PTASK_TRIGGER_TYPE = ^_TASK_TRIGGER_TYPE;
  TTaskTriggerType = _TASK_TRIGGER_TYPE;
  PTaskTriggerType = PTASK_TRIGGER_TYPE;

  {$EXTERNALSYM _DAILY}
  _DAILY = packed record
    DaysInterval: WORD;
  end;
  {$EXTERNALSYM DAILY}
  DAILY = _DAILY;
  TDaily = _DAILY;

  {$EXTERNALSYM _WEEKLY}
  _WEEKLY = packed record
     WeeksInterval: WORD;
     rgfDaysOfTheWeek: WORD;
  end;
  {$EXTERNALSYM WEEKLY}
  WEEKLY = _WEEKLY;
  TWeekly = _WEEKLY;

  {$EXTERNALSYM _MONTHLYDATE}
  _MONTHLYDATE = packed record
    rgfDays: DWORD;
    rgfMonths: WORD;
  end;
  {$EXTERNALSYM MONTHLYDATE}
  MONTHLYDATE = _MONTHLYDATE;
  TMonthlyDate = _MONTHLYDATE;

  {$EXTERNALSYM _MONTHLYDOW}
  _MONTHLYDOW = packed record
    wWhichWeek: WORD;
    rgfDaysOfTheWeek: WORD;
    rgfMonths: WORD;
  end;
  {$EXTERNALSYM MONTHLYDOW}
  MONTHLYDOW = _MONTHLYDOW;
  TMonthlyDOW = _MONTHLYDOW;

  {$EXTERNALSYM _TRIGGER_TYPE_UNION}
  _TRIGGER_TYPE_UNION = packed record
    case Integer of
      0: (Daily: _DAILY);
      1: (Weekly: _WEEKLY);
      2: (MonthlyDate: _MONTHLYDATE);
      3: (MonthlyDOW: _MONTHLYDOW);
  end;
  {$EXTERNALSYM TRIGGER_TYPE_UNION}
  TRIGGER_TYPE_UNION = _TRIGGER_TYPE_UNION;
  TTriggerTypeUnion = _TRIGGER_TYPE_UNION;

  {$EXTERNALSYM _TASK_TRIGGER}
  _TASK_TRIGGER = packed record
    cbTriggerSize: WORD;             // Structure size.
    Reserved1: WORD;                 // Reserved. Must be zero.
    wBeginYear: WORD;                // Trigger beginning date year.
    wBeginMonth: WORD;               // Trigger beginning date month.
    wBeginDay: WORD;                 // Trigger beginning date day.
    wEndYear: WORD;                  // Optional trigger ending date year.
    wEndMonth: WORD;                 // Optional trigger ending date month.
    wEndDay: WORD;                   // Optional trigger ending date day.
    wStartHour: WORD;                // Run bracket start time hour.
    wStartMinute: WORD;              // Run bracket start time minute.
    MinutesDuration: DWORD;          // Duration of run bracket.
    MinutesInterval: DWORD;          // Run bracket repetition interval.
    rgFlags: DWORD;                  // Trigger flags.
    TriggerType: TASK_TRIGGER_TYPE;  // Trigger type.
    Type_: TRIGGER_TYPE_UNION;       // Trigger data.
    Reserved2: WORD;                 // Reserved. Must be zero.
    wRandomMinutesInterval: WORD;    // Maximum number of random minutes
                                     // after start time.
  end;
  {$EXTERNALSYM TASK_TRIGGER}
  TASK_TRIGGER = _TASK_TRIGGER;
  {$EXTERNALSYM PTASK_TRIGGER}
  PTASK_TRIGGER = ^_TASK_TRIGGER;
  TTaskTrigger = _TASK_TRIGGER;
  PTaskTrigger = PTASK_TRIGGER;

//+----------------------------------------------------------------------------
//
//  Interfaces
//
//-----------------------------------------------------------------------------

//+----------------------------------------------------------------------------
//
//  Interface:  ITaskTrigger
//
//  Synopsis:   Trigger object interface. A Task object may contain several
//              of these.
//
//-----------------------------------------------------------------------------
const
  // {148BD52B-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM IID_ITaskTrigger}
  IID_ITaskTrigger: TIID = (D1: $148BD52B; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

type
  {$EXTERNALSYM ITaskTrigger}
  ITaskTrigger = interface(IUnknown)
    ['{148BD52B-A2AB-11CE-B11F-00AA00530503}']
    function SetTrigger(const pTrigger: TTaskTrigger): HRESULT; stdcall;
    function GetTrigger(out pTrigger: TTaskTrigger): HRESULT; stdcall;
    function GetTriggerString(out ppwszTrigger: LPWSTR): HRESULT; stdcall;
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  IScheduledWorkItem
//
//  Synopsis:   Abstract base class for any runnable work item that can be
//              scheduled by the task scheduler.
//
//-----------------------------------------------------------------------------
const
  // {a6b952f0-a4b1-11d0-997d-00aa006887ec}
  {$EXTERNALSYM IID_IScheduledWorkItem}
  IID_IScheduledWorkItem: TIID = (D1: $a6b952f0; D2: $a4b1; D3: $11d0; D4: ($99, $7d, $00, $aa, $00, $68, $87, $ec));

type
  {$EXTERNALSYM IScheduledWorkItem}
  IScheduledWorkItem = interface(IUnknown)
    ['{a6b952f0-a4b1-11d0-997d-00aa006887ec}']
    // Methods concerning scheduling:
    function CreateTrigger(out piNewTrigger: WORD; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    function DeleteTrigger(iTrigger: WORD): HRESULT; stdcall;
    function GetTriggerCount(out pwCount: WORD): HRESULT; stdcall;
    function GetTrigger(iTrigger: WORD; out ppTrigger: ITaskTrigger): HRESULT; stdcall;
    function GetTriggerString(iTrigger: WORD; out ppwszTrigger: LPWSTR): HRESULT; stdcall;
    function GetRunTimes(pstBegin, pstEnd: PSystemTime; var pCount: Word; out rgstTaskTimes: PSystemTime): HRESULT; stdcall;
    function GetNextRunTime(var pstNextRun: SYSTEMTIME): HRESULT; stdcall;
    function SetIdleWait(wIdleMinutes, wDeadlineMinutes: WORD): HRESULT; stdcall;
    function GetIdleWait(out pwIdleMinutes, pwDeadlineMinutes: WORD): HRESULT; stdcall;
    // Other methods:
    function Run: HRESULT; stdcall;
    function Terminate: HRESULT; stdcall;
    function EditWorkItem(hParent: HWND; dwReserved: DWORD): HRESULT; stdcall;
    function GetMostRecentRunTime(out pstLastRun: SYSTEMTIME): HRESULT; stdcall;
    function GetStatus(out phrStatus: HRESULT): HRESULT; stdcall;
    function GetExitCode(out pdwExitCode: DWORD): HRESULT; stdcall;
    // Properties:
    function SetComment(pwszComment: LPCWSTR): HRESULT; stdcall;
    function GetComment(out ppwszComment: LPWSTR): HRESULT; stdcall;
    function SetCreator(pwszCreator: LPCWSTR): HRESULT; stdcall;
    function GetCreator(out ppwszCreator: LPWSTR): HRESULT; stdcall;
    function SetWorkItemData(cbData: WORD; rgbData: PByte): HRESULT; stdcall;
    function GetWorkItemData(out pcbData: WORD; out prgbData: PByte): HRESULT; stdcall;
    function SetErrorRetryCount(wRetryCount: WORD): HRESULT; stdcall;
    function GetErrorRetryCount(out pwRetryCount: WORD): HRESULT; stdcall;
    function SetErrorRetryInterval(wRetryInterval: WORD): HRESULT; stdcall;
    function GetErrorRetryInterval(out pwRetryInterval: WORD): HRESULT; stdcall;
    function SetFlags(dwFlags: DWORD): HRESULT; stdcall;
    function GetFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    function SetAccountInformation(pwszAccountName, pwszPassword: LPCWSTR): HRESULT; stdcall;
    function GetAccountInformation(out ppwszAccountName: LPWSTR): HRESULT; stdcall;
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  ITask
//
//  Synopsis:   Task object interface. The primary means of task object
//              manipulation.
//
//-----------------------------------------------------------------------------
const
  // {148BD524-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM IID_ITask}
  IID_ITask: TIID = (D1: $148BD524; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

type
  {$EXTERNALSYM ITask}
  ITask = interface(IScheduledWorkItem)
    ['{148BD524-A2AB-11CE-B11F-00AA00530503}']
    // Properties that correspond to parameters of CreateProcess:
    function SetApplicationName(pwszApplicationName: LPCWSTR): HRESULT; stdcall;
    function GetApplicationName(out ppwszApplicationName: LPWSTR): HRESULT; stdcall;
    function SetParameters(pwszParameters: LPCWSTR): HRESULT; stdcall;
    function GetParameters(out ppwszParameters: LPWSTR): HRESULT; stdcall;
    function SetWorkingDirectory(pwszWorkingDirectory: LPCWSTR): HRESULT; stdcall;
    function GetWorkingDirectory(out ppwszWorkingDirectory: LPWSTR): HRESULT; stdcall;
    function SetPriority(dwPriority: DWORD): HRESULT; stdcall;
    function GetPriority(out pdwPriority: DWORD): HRESULT; stdcall;
    // Other properties:
    function SetTaskFlags(dwFlags: DWORD): HRESULT; stdcall;
    function GetTaskFlags(out pdwFlags: DWORD): HRESULT; stdcall;
    function SetMaxRunTime(dwMaxRunTimeMS: DWORD): HRESULT; stdcall;
    function GetMaxRunTime(out pdwMaxRunTimeMS: DWORD): HRESULT; stdcall;
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  IEnumWorkItems
//
//  Synopsis:   Work item object enumerator. Enumerates the work item objects
//              within the Tasks folder.
//
//-----------------------------------------------------------------------------
const
  // {148BD528-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM IID_IEnumWorkItems}
  IID_IEnumWorkItems: TIID = (D1: $148BD528; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

type
  {$EXTERNALSYM IEnumWorkItems}
  IEnumWorkItems = interface(IUnknown)
    ['{148BD528-A2AB-11CE-B11F-00AA00530503}']
    function Next(celt: ULONG; out rgpwszNames: PLPWSTR; out pceltFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
  end;

//+----------------------------------------------------------------------------
//
//  Interface:  ITaskScheduler
//
//  Synopsis:   Task Scheduler interface. Provides location transparent
//              manipulation of task and/or queue objects within the Tasks
//              folder.
//
//-----------------------------------------------------------------------------
const
  // {148BD527-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM IID_ITaskScheduler}
  IID_ITaskScheduler: TIID = (D1: $148BD527; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

type
  {$EXTERNALSYM ITaskScheduler}
  ITaskScheduler = interface(IUnknown)
    ['{148BD527-A2AB-11CE-B11F-00AA00530503}']
    function SetTargetComputer(pwszComputer: LPCWSTR): HRESULT; stdcall;
    function GetTargetComputer(out ppwszComputer: LPWSTR): HRESULT; stdcall;
    function Enum(out ppEnumWorkItems: IEnumWorkItems): HRESULT; stdcall;
    function Activate(pwszName: LPCWSTR; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    function Delete(pwszName: LPCWSTR): HRESULT; stdcall;
    function NewWorkItem(pwszTaskName: LPCWSTR; const rclsid: TCLSID; const riid: TIID; out ppUnk: IUnknown): HRESULT; stdcall;
    function AddWorkItem(pwszTaskName: LPCWSTR; pWorkItem: IScheduledWorkItem): HRESULT; stdcall;
    function IsOfType(pwszName: LPCWSTR; const riid: TIID): HRESULT; stdcall;
  end;

const
  // {148BD520-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM CLSID_CTask}
  CLSID_CTask: TCLSID = (D1: $148BD520; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));
  // {148BD52A-A2AB-11CE-B11F-00AA00530503}
  {$EXTERNALSYM CLSID_CTaskScheduler}
  CLSID_CTaskScheduler: TCLSID = (D1: $148BD52A; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

//
// NOTE: Definition of HPROPSHEETPAGE is from sdk\inc\prsht.h
//       Including this header file causes numerous redefinition errors.
//
type
  {$EXTERNALSYM _PSP}
  _PSP = record end;
  {$EXTERNALSYM HPROPSHEETPAGE}
  HPROPSHEETPAGE = ^_PSP;

  {$EXTERNALSYM _TASKPAGE}
  _TASKPAGE = (
    {$EXTERNALSYM TASKPAGE_TASK}
    TASKPAGE_TASK,
    {$EXTERNALSYM TASKPAGE_SCHEDULE}
    TASKPAGE_SCHEDULE,
    {$EXTERNALSYM TASKPAGE_SETTINGS}
    TASKPAGE_SETTINGS);
  {$EXTERNALSYM TASKPAGE}
  TASKPAGE = _TASKPAGE;

//+----------------------------------------------------------------------------
//
//  Interface:  IProvideTaskPage
//
//  Synopsis:   Task property page retrieval interface. With this interface,
//              it is possible to retrieve one or more property pages
//              associated with a task object. Task objects inherit this
//              interface.
//
//-----------------------------------------------------------------------------
const
  // {4086658a-cbbb-11cf-b604-00c04fd8d565}
  {$EXTERNALSYM IID_IProvideTaskPage}
  IID_IProvideTaskPage: TIID = (D1: $4086658a; D2: $cbbb; D3: $11cf; D4: ($b6, $04, $00, $c0, $4f, $d8, $d5, $65));

type
  {$EXTERNALSYM IProvideTaskPage}
  IProvideTaskPage = interface(IUnknown)
    ['{4086658a-cbbb-11cf-b604-00c04fd8d565}']
    function GetPage(tpType: TASKPAGE; fPersistChanges: BOOL; out phPage: HPROPSHEETPAGE): HRESULT; stdcall;
  end;
  
type
  {$EXTERNALSYM ISchedulingAgent}
  ISchedulingAgent = ITaskScheduler;
  {$EXTERNALSYM IEnumTasks}
  IEnumTasks = IEnumWorkItems;

const
  {$EXTERNALSYM IID_ISchedulingAgent}
  // IID_ISchedulingAgent = IID_ITaskScheduler;
  IID_ISchedulingAgent: TIID = (D1: $148BD527; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));
  {$EXTERNALSYM CLSID_CSchedulingAgent}
  // CLSID_CSchedulingAgent = CLSID_CTaskScheduler;
  CLSID_CSchedulingAgent: TCLSID = (D1: $148BD52A; D2: $A2AB; D3: $11CE; D4: ($B1, $1F, $00, $AA, $00, $53, $05, $03));

implementation

end.
