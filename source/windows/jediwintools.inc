{******************************************************************************}
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
{  The Original Code is: jediwintools.inc.                                     }
{  The Initial Developer of the Original Code is Peter J. Haas. Portions       }
{  created by Peter J. Haas are Copyright (C) 2003 Peter J. Haas. All Rights   }
{  Reserved.                                                                   }
{                                                                              }
{  You may retrieve the latest version of this file at the homepage of         }
{  JEDI+ (jediplus@pjh2.de), located at http://jediplus.pjh2.de/               }
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

{$I jedi.inc}

{$IFNDEF MSWINDOWS}
MSWindows only
{$ENDIF}

{$IFDEF SUPPORTS_PLATFORM}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF SUPPORTS_DEPRECATED}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
