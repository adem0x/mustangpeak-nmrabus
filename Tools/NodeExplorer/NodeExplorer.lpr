program NodeExplorer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, runtimetypeinfocontrols, UnitMainForm, unitlogwindow, olcb_utilities,
  olcb_stringconstants, unitolcb_defines, unitsettings, serialport_thread,
  olcb_testmatrix, nodeexplorer_settings, unitDebugLogger, unitLinuxFTDI;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormLog, FormLog);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormDebugLogger, FormDebugLogger);
  Application.Run;
end.
