#sub AddIconFiles
#expr GetFileVars
Source: {#Copy (IconsMask, 0, RPos ("\", IconsMask)) + CurrentFileName}; DestDir: {app}; DestName: {#IconName}; Components: icons\{#CurrentName}; Flags: ignoreversion
#endsub

#sub GetFileVars
	#define i
	#define CurrentNameDropSym " !#$%&'()+,-.;=@[]^`{}~"
	#expr CurrentFileName		= FindGetFileName	(FindHandle)
	#expr CurrentDesc		= Copy			(CurrentFileName, 0, RPos (".", CurrentFileName) - 1)
	#expr CurrentName		= Lowercase		(CurrentDesc)
	#for {i=1; i <= Len (CurrentNameDropSym); i++} CurrentName = StringChange (CurrentName, Copy (CurrentNameDropSym, i, 1), "_")
	#expr CurrentName = StringChange (CurrentName, "__", "_")
#endsub

[files]
Source: "{app}\Plugins\wlx\SynWrite\HL\C#.acp"; DestDir: "{app}\Plugins\wlx\SynWrite\HL"; MinVersion: 0.0,5.0; Flags: restartreplace overwritereadonly uninsrestartdelete ignoreversion uninsremovereadonly
Source: "{app}\Plugins\wlx\SynWrite\HL\C++.acp"; DestDir: "{app}\Plugins\wlx\SynWrite\HL"; MinVersion: 0.0,5.0; Flags: restartreplace overwritereadonly uninsrestartdelete ignoreversion uninsremovereadonly
Source: "{app}\Plugins\wlx\SynWrite\HL\C.acp"; DestDir: "{app}\Plugins\wlx\SynWrite\HL"; MinVersion: 0.0,5.0; Flags: restartreplace overwritereadonly uninsrestartdelete ignoreversion uninsremovereadonly

[Code]

// + ����������� ���������� �����
#ifdef IsSVPInt

	procedure CurPageChanged (CurPageID: Integer);
	begin
		if not WizardSilent and (CurPageID = wpSelectTasks) then
		begin
			if IsComponentSelected ('svp\raw_filter') then
			begin
				WizardForm.TasksList.Checked [WizardForm.TasksList.Items.IndexOf (ExpandConstant ('{cm:DisableVideoProc}'))]		:= true;

				if GetWindowsVersion shr 24 >= 6 then
				begin
					WizardForm.TasksList.Checked [WizardForm.TasksList.Items.IndexOf (ExpandConstant ('{cm:UseDXVARenderless}'))]	:= true;
					WizardForm.TasksList.Checked [WizardForm.TasksList.Items.IndexOf (ExpandConstant ('{cm:UseSystemVC1}'))]	:= false;
					WizardForm.TasksList.Checked [WizardForm.TasksList.Items.IndexOf (ExpandConstant ('{cm:UseSystemWMV}'))]	:= false;
				end;
			end;
		end;
	end;

#endif


[CustomMessages]
ru.FixErrorGroup	=�������������� ������:
ru.ResetSettigs		=�������� ��� ��������� ���������
ru.DisableAudioProc	=��������� ����-��������� �����
ru.DisableVideoProc	=��������� ����-��������� �����
ru.UseDXVARenderless	=����� DXVA � ������������ ����-���������
ru.UseSystemVC1		=������������ ��������� ������� VC1
ru.UseSystemWMV		=������������ ��������� ������� WMV
ru.UseD3DFullScreen	=������������ D3D FullScreen (VMR/EVR)
ru.UseLevels16_235Shader=������������ ������ "{#Copy(LevelsShaderName, 0, RPos (".", LevelsShaderName) - 1)}" (VMR/EVR)
