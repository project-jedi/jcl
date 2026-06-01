{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test Unit                                                                                  }
{                                                                                                  }
{ Covers:      JclExprEval                                                                                  }
{ Last Update: 14-Feb-2022                                                                         }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclExprEval;

interface
uses
  TestFramework,
  JclExprEval;

 { TJclExprEvalTest}

type
  TJclExprEvalTest = class (TTestCase)
  private
    FEvaluator : TEvaluator;
    FCompiledEvaluator : TCompiledEvaluator;
    TestsToCopyToExcel : string;

    procedure CheckEvaluators(expected: extended; const aExpression: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure SimpleIntegerCalculations;
    procedure SimpleFloatCalculations;
    procedure EvaluationOrderCalculations;
  end;

implementation

uses
  Clipbrd,
  SysUtils;


//==================================================================================================
// TJclExprEvalTest
//==================================================================================================


{ TJclExprEvalTest }


procedure TJclExprEvalTest.SetUp;
begin
  inherited;
  FEvaluator := TEvaluator.Create;
  FCompiledEvaluator := TCompiledEvaluator.Create;
end;


procedure TJclExprEvalTest.TearDown;
begin
  FEvaluator.Free;
  FCompiledEvaluator.Free;

//  To test the tests in Excel uncomment next line, run the tests, open excel and paste, compare the values in the collumns.
//  Clipboard.AsText := TestsToCopyToExcel;

  inherited;
end;


procedure TJclExprEvalTest.CheckEvaluators(expected: extended; const aExpression: string);
begin
  TestsToCopyToExcel := TestsToCopyToExcel + '=' + aExpression + #9 + expected.ToString() + #13#10;
  CheckEquals(expected, FEvaluator.Evaluate(aExpression), 0.000001, 'Eval:' + aExpression);
  FCompiledEvaluator.Compile(aExpression);
  CheckEquals(expected, FCompiledEvaluator.Evaluate, 0.000001, 'Compiled:' + aExpression);
end;


procedure TJclExprEvalTest.SimpleIntegerCalculations;
begin
  CheckEvaluators(12, '10+2');
  CheckEvaluators(13, ' 10 + 3 ');
  CheckEvaluators(6, '3*2');
  CheckEvaluators(-6, '3*-2');
  CheckEvaluators(-9, '18/-2');
  CheckEvaluators(-9, '-18/2');
  CheckEvaluators(81, '3^4');
  CheckEvaluators(99, '990*10%');
  CheckEvaluators(88, '10%*880');
end;


procedure TJclExprEvalTest.SimpleFloatCalculations;
begin
  FormatSettings.DecimalSeparator := '.';

  CheckEvaluators(2223.61, '1.61+2222');
  CheckEvaluators(-2220.39, '1.61-2222');
  CheckEvaluators(6.66, '3*2.22');
  CheckEvaluators(-6.90, ' 2 * -3.45');
  CheckEvaluators(36, '90/2.5');
  CheckEvaluators(99.9*1.5/100, '99.9*1.5%');
end;


procedure TJclExprEvalTest.EvaluationOrderCalculations;
begin
  CheckEvaluators(7, '1+2*3');
  CheckEvaluators(9, '(1+2)*3');
  CheckEvaluators(3, '1+4/2');
  CheckEvaluators(2.5, '(1+4)/2');
  CheckEvaluators(2, '1+10*10%');
  CheckEvaluators(2, '1+10%*10');
  CheckEvaluators(10.1, '10%+10*1');

  CheckEvaluators(101, '1+10^2');
  CheckEvaluators(101, '10^2+1');
  CheckEvaluators(-99, '1-10^2');
  CheckEvaluators(99, '10^2-1');
  CheckEvaluators(10, '10^(2-1)');
  CheckEvaluators(81, '(1-10)^2');
  CheckEvaluators(121, '(1--10)^2');
  CheckEvaluators(200, '10^2*2');
  CheckEvaluators(200, '2*10^2');

  CheckEvaluators(30000, '100^2*3');
  CheckEvaluators(90000, '9*100^2');
  CheckEvaluators(60000, '3*100^2*2');
  CheckEvaluators(80000, '4*(10*10)^2*2');
  CheckEvaluators(40000,'2*(10*10)^(1+1)*2');
  CheckEvaluators(314, '314/4*(2^2)');
  CheckEvaluators(314/2, '314/8*2^2');
  CheckEvaluators(78.5, '314/4*(1^2)');
  CheckEvaluators(78.5, '314/4*1^2');

  CheckEvaluators(314, '314/4*(2^200%)');
  CheckEvaluators(314, '314/4*(2^20000%%)');
  CheckEvaluators(314, '314/4*2^200%');
  CheckEvaluators(313, '314/4*2^20000%%-1');
  CheckEvaluators(25, '100*2^-2');
end;



initialization
  RegisterTest('JclExprEval', TJclExprEvalTest.Suite);

end.


