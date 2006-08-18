unit JediInstallIntf;

interface

uses
  ComCtrls, DelphiInstall;

const
  // Feature masks
  FID_Product              = $7F000000;
  FID_IsProduct            = $00FFFFFF;
  FID_Category             = $00FF0000;
  FID_IsCategory           = $0000FFFF;
  FID_Level2               = $0000FF00;
  FID_IsLevel2             = $000000FF;
  FID_Level3               = $000000FF;
  FID_Checked              = $80000000;
  FID_NumberMask           = $7FFFFFFF;

  // Icon indexes
  IcoProduct               = 0;
  IcoLevel1                = 1;
  IcoChecked               = 2;
  IcoUnchecked             = 3;

type
  IJediInstallTool = interface
    ['{30E41236-3DCD-4F84-9ADF-4489CA1E4EC7}']
    function ActiveVersionNumberPage: Integer;
    function BPLPath(VersionNumber: Integer): string;
    function DCPPath(VersionNumber: Integer): string;
    function FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
    function GetDelphiInstallations: TJclDelphiInstallations;
    function MessageBox(const Text: string; Flags: Integer): Integer;
    procedure UpdateInfo(VersionNumber: Integer; const InfoText: string);
    procedure UpdateStatus(const Text: string);
    property DelphiInstallations: TJclDelphiInstallations read GetDelphiInstallations;
  end;

  IJediInstall = interface
    ['{BE0A7968-9003-40DD-99F0-250CAC8B2D85}']
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function PopulateTreeView(Nodes: TTreeNodes; VersionNumber: Integer; Page: TTabSheet): Boolean;
    function SelectedNodeCollapsing(Node: TTreeNode): Boolean;
    procedure SelectedNodeChanged(Node: TTreeNode);
    procedure SetTool(const Value: IJediInstallTool);
  end;

implementation

end.
