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
{  The Original Code is part of: WinError.h.                                   }
{  The Initial Developer of the Original Code is Microsoft. Portions created   }
{  by Microsoft are Copyright (C) Microsoft Corporation. All Rights Reserved.  }
{                                                                              }
{  The Original Pascal code is: MSTaskError.pas.                               }
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

unit MSTaskError;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

{$IFDEF SUPPORTS_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF SUPPORTS_DEPRECATED}

interface

(*$HPPEMIT '' *)
(*$HPPEMIT '#include <WinError.h>' *)
(*$HPPEMIT '' *)

(************************************************************************
*                                                                       *
*   winerror.h --  error code definitions for the Win32 API functions   *
*                                                                       *
*   Copyright (c) Microsoft Corp.  All rights reserved.                 *
*                                                                       *
************************************************************************)

const
//
// Task Scheduler errors
//
//
// MessageId: SCHED_S_TASK_READY
//
// MessageText:
//
//  The task is ready to run at its next scheduled time.
//
  {$EXTERNALSYM SCHED_S_TASK_READY}
  SCHED_S_TASK_READY = HRESULT($00041300);

//
// MessageId: SCHED_S_TASK_RUNNING
//
// MessageText:
//
//  The task is currently running.
//
  {$EXTERNALSYM SCHED_S_TASK_RUNNING}
  SCHED_S_TASK_RUNNING = HRESULT($00041301);

//
// MessageId: SCHED_S_TASK_DISABLED
//
// MessageText:
//
//  The task will not run at the scheduled times because it has been disabled.
//
  {$EXTERNALSYM SCHED_S_TASK_DISABLED}
  SCHED_S_TASK_DISABLED = HRESULT($00041302);

//
// MessageId: SCHED_S_TASK_HAS_NOT_RUN
//
// MessageText:
//
//  The task has not yet run.
//
  {$EXTERNALSYM SCHED_S_TASK_HAS_NOT_RUN}
  SCHED_S_TASK_HAS_NOT_RUN = HRESULT($00041303);

//
// MessageId: SCHED_S_TASK_NO_MORE_RUNS
//
// MessageText:
//
//  There are no more runs scheduled for this task.
//
  {$EXTERNALSYM SCHED_S_TASK_NO_MORE_RUNS}
  SCHED_S_TASK_NO_MORE_RUNS = HRESULT($00041304);

//
// MessageId: SCHED_S_TASK_NOT_SCHEDULED
//
// MessageText:
//
//  One or more of the properties that are needed to run this task on a schedule have not been set.
//
  {$EXTERNALSYM SCHED_S_TASK_NOT_SCHEDULED}
  SCHED_S_TASK_NOT_SCHEDULED = HRESULT($00041305);

//
// MessageId: SCHED_S_TASK_TERMINATED
//
// MessageText:
//
//  The last run of the task was terminated by the user.
//
  {$EXTERNALSYM SCHED_S_TASK_TERMINATED}
  SCHED_S_TASK_TERMINATED = HRESULT($00041306);

//
// MessageId: SCHED_S_TASK_NO_VALID_TRIGGERS
//
// MessageText:
//
//  Either the task has no triggers or the existing triggers are disabled or not set.
//
  {$EXTERNALSYM SCHED_S_TASK_NO_VALID_TRIGGERS}
  SCHED_S_TASK_NO_VALID_TRIGGERS = HRESULT($00041307);

//
// MessageId: SCHED_S_EVENT_TRIGGER
//
// MessageText:
//
//  Event triggers don't have set run times.
//
  {$EXTERNALSYM SCHED_S_EVENT_TRIGGER}
  SCHED_S_EVENT_TRIGGER = HRESULT($00041308);

//
// MessageId: SCHED_E_TRIGGER_NOT_FOUND
//
// MessageText:
//
//  Trigger not found.
//
  {$EXTERNALSYM SCHED_E_TRIGGER_NOT_FOUND}
  SCHED_E_TRIGGER_NOT_FOUND = HRESULT($80041309);

//
// MessageId: SCHED_E_TASK_NOT_READY
//
// MessageText:
//
//  One or more of the properties that are needed to run this task have not been set.
//
  {$EXTERNALSYM SCHED_E_TASK_NOT_READY}
  SCHED_E_TASK_NOT_READY = HRESULT($8004130A);

//
// MessageId: SCHED_E_TASK_NOT_RUNNING
//
// MessageText:
//
//  There is no running instance of the task to terminate.
//
  {$EXTERNALSYM SCHED_E_TASK_NOT_RUNNING}
  SCHED_E_TASK_NOT_RUNNING = HRESULT($8004130B);

//
// MessageId: SCHED_E_SERVICE_NOT_INSTALLED
//
// MessageText:
//
//  The Task Scheduler Service is not installed on this computer.
//
  {$EXTERNALSYM SCHED_E_SERVICE_NOT_INSTALLED}
  SCHED_E_SERVICE_NOT_INSTALLED = HRESULT($8004130C);

//
// MessageId: SCHED_E_CANNOT_OPEN_TASK
//
// MessageText:
//
//  The task object could not be opened.
//
  {$EXTERNALSYM SCHED_E_CANNOT_OPEN_TASK}
  SCHED_E_CANNOT_OPEN_TASK = HRESULT($8004130D);

//
// MessageId: SCHED_E_INVALID_TASK
//
// MessageText:
//
//  The object is either an invalid task object or is not a task object.
//
  {$EXTERNALSYM SCHED_E_INVALID_TASK}
  SCHED_E_INVALID_TASK = HRESULT($8004130E);

//
// MessageId: SCHED_E_ACCOUNT_INFORMATION_NOT_SET
//
// MessageText:
//
//  No account information could be found in the Task Scheduler security database for the task indicated.
//
  {$EXTERNALSYM SCHED_E_ACCOUNT_INFORMATION_NOT_SET}
  SCHED_E_ACCOUNT_INFORMATION_NOT_SET = HRESULT($8004130F);

//
// MessageId: SCHED_E_ACCOUNT_NAME_NOT_FOUND
//
// MessageText:
//
//  Unable to establish existence of the account specified.
//
  {$EXTERNALSYM SCHED_E_ACCOUNT_NAME_NOT_FOUND}
  SCHED_E_ACCOUNT_NAME_NOT_FOUND = HRESULT($80041310);

//
// MessageId: SCHED_E_ACCOUNT_DBASE_CORRUPT
//
// MessageText:
//
//  Corruption was detected in the Task Scheduler security database; the database has been reset.
//
  {$EXTERNALSYM SCHED_E_ACCOUNT_DBASE_CORRUPT}
  SCHED_E_ACCOUNT_DBASE_CORRUPT = HRESULT($80041311);

//
// MessageId: SCHED_E_NO_SECURITY_SERVICES
//
// MessageText:
//
//  Task Scheduler security services are available only on Windows NT.
//
  {$EXTERNALSYM SCHED_E_NO_SECURITY_SERVICES}
  SCHED_E_NO_SECURITY_SERVICES = HRESULT($80041312);

//
// MessageId: SCHED_E_UNKNOWN_OBJECT_VERSION
//
// MessageText:
//
//  The task object version is either unsupported or invalid.
//
  {$EXTERNALSYM SCHED_E_UNKNOWN_OBJECT_VERSION}
  SCHED_E_UNKNOWN_OBJECT_VERSION = HRESULT($80041313);

//
// MessageId: SCHED_E_UNSUPPORTED_ACCOUNT_OPTION
//
// MessageText:
//
//  The task has been configured with an unsupported combination of account settings and run time options.
//
  {$EXTERNALSYM SCHED_E_UNSUPPORTED_ACCOUNT_OPTION}
  SCHED_E_UNSUPPORTED_ACCOUNT_OPTION = HRESULT($80041314);

//
// MessageId: SCHED_E_SERVICE_NOT_RUNNING
//
// MessageText:
//
//  The Task Scheduler Service is not running.
//
  {$EXTERNALSYM SCHED_E_SERVICE_NOT_RUNNING}
  SCHED_E_SERVICE_NOT_RUNNING = HRESULT($80041315);

implementation

end.
