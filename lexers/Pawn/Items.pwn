/*
Items-System by Games, 2013.
Programming language: Pawn


���������� ����� ���������:

� ������ OnFilterScriptInit() ��������� ����� �-�� AddItem("�������� ��������")

��������, ������ ����� ����� ����� ���:

public OnFilterScriptInit()
{
	AddItem("������");
	AddItem("�����");
	AddItem("�������");
	AddItem("��������");
}

��� ���������� ������ DEBUG � ������� ��� ���������� ������� ����������� �� ��.

�������� ���-�� ��������� ������ ����� ����� 2 �������:
stock SetItemTheName(playerid, string[], Val); //�������� ���-�� ��������� ������ �� �������� ��������
stock SetItemTheID(playerid, oID, Val); //�������� ���-�� ��������� ������ �� �� ��������

�������� ��� ������������� ����������� �����:
stock UseItem(playerid, itID); //������������� ���������
*/

#include <a_samp>
#include <mxINI>

#define MAX_ITEMS 100
#define DEBUG 1
#define D_Items 500 //������ �� ������� �������

//��������� �������:
stock AddItem(string[]); //�������� ������� � ������ ���������
stock SetItemTheName(playerid, string[], Val); //�������� ���-�� ��������� ������ �� �������� ��������
stock SetItemTheID(playerid, oID, Val); //�������� ���-�� ��������� ������ �� �� ��������
stock ResetItems(playerid); //�������� �������� ������
stock LoadItems(playerid); //�������� ���������
stock UseItem(playerid, itID); //������������� ���������

//--------------
//���������� �-��:
stock ShiftData(playerid, ItS); //�������� ������ � ������ ��� ������ �����

enum I_Info
{
	Name[30]
}

new Items[MAX_ITEMS][I_Info];
new PlayerItems[MAX_PLAYERS][MAX_ITEMS][2]; // 0 - ID | 1 - ���-��
new UseSlotItem[MAX_PLAYERS]; //���� ��� ��������
new GiveInfo[MAX_PLAYERS][4];

new ItemID = 0x0;

public OnFilterScriptInit()
{
	AddItem("������");
	AddItem("�����");
	AddItem("�������");
	AddItem("��������");
}

public OnPlayerConnect(playerid)
{
	ResetItems(playerid);
	LoadItems(playerid);
}

public OnPlayerDisconnect(playerid, reason)
{
    SaveItems(playerid);
}

public OnPlayerCommandText(playerid, cmdtext[])
{
	if(!strcmp("/inv", cmdtext, true))
	{
		new string[1000], find = 0; //�������� ��� �������������
		for(new f; f != ItemID; f++)
		{
  			if(PlayerItems[playerid][f][0] != 0)
	 		{
	 		    find = 1;
	 		    format(string, sizeof(string), "%s%s [%d]\n", string, Items[PlayerItems[playerid][f][0] - 1][Name], PlayerItems[playerid][f][1]);
	 		}
	 		else break;
		}
		if(find == 1) ShowPlayerDialog(playerid, D_Items, DIALOG_STYLE_LIST, "���������", string, "��", "������");
		else SendClientMessage(playerid, -1, "� ��� ��� ���������.");
		return 1;
	}
	if(!strcmp("/giveinv", cmdtext, true))
	{
	    SetItemTheID(playerid, 1, 20);
	    SetItemTheID(playerid, 2, 20);
	    SetItemTheID(playerid, 3, 20);
	    SetItemTheID(playerid, 4, 20);
	    return true;
	}
	return 0;
}

public OnDialogResponse(playerid, dialogid, response, listitem, inputtext[])
{
	if(dialogid == D_Items && response)
	{
        UseSlotItem[playerid] = listitem;
        ShowPlayerDialog(playerid, D_Items + 1, DIALOG_STYLE_LIST, "���������", "������������\n��������\n��������", "��", "������");
	}
	else if(dialogid == D_Items + 1 && response)
	{
	    switch(listitem)
	    {
	        case 0: //������������
	        {
	            //��� �������������
	            UseItem(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0]);
	            SetItemTheID(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0], -1);
	            SendClientMessage(playerid, -1, "�� ������� ������������ �������");
	        }
			case 1: //�������� �������
			{
				ShowPlayerDialog(playerid, D_Items + 3, DIALOG_STYLE_INPUT, "�������� �������", "������� ���-�� ���������, ������� �� ������ ��������:", "��", "������");
			}
			case 2: //�������� �������
			{
			    ShowPlayerDialog(playerid, D_Items + 2, DIALOG_STYLE_INPUT, "�������� �������", "������� �� ������:", "��", "������");
			}
	    }
	}
	else if(dialogid == D_Items + 2 && response)
	{
	    new id = strval(inputtext);
		if(!IsPlayerConnected(id)) return SendClientMessage(playerid, -1, "������ ����� �� ���������.");
		new Float: Pos[3];
		GetPlayerPos(id, Pos[0], Pos[1], Pos[2]);
		if(!IsPlayerInRangeOfPoint(playerid, 5.00, Pos[0], Pos[1], Pos[2])) return SendClientMessage(playerid, -1, "�� ������ ���������� ����� ������.");
		GiveInfo[playerid][0] = id;
	    ShowPlayerDialog(playerid, D_Items + 4, DIALOG_STYLE_INPUT, "�������� �������", "������� ���-�� ���������:", "��", "������");
	}
	else if(dialogid == D_Items + 3 && response)
	{
	    if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "�� ������ �� �����");
	    if(strval(inputtext) > PlayerItems[playerid][UseSlotItem[playerid]][1]) return SendClientMessage(playerid, -1, "� ��� ��� ������� ���������.");
	    SetItemTheID(playerid, PlayerItems[playerid][UseSlotItem[playerid]][0], -strval(inputtext));
		SendClientMessage(playerid, -1, "�� ������� �������� �������(�)");
	}
	else if(dialogid == D_Items + 4 && response)
	{
		if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "�� ������ �� �����");
	    if(strval(inputtext) > PlayerItems[playerid][UseSlotItem[playerid]][1]) return SendClientMessage(playerid, -1, "� ��� ��� ������� ���������.");
		GiveInfo[playerid][1] = strval(inputtext);
		ShowPlayerDialog(playerid, D_Items + 5, DIALOG_STYLE_INPUT, "�������� �������", "������� c����:", "��", "������");
	}
	else if(dialogid == D_Items + 5 && response)
	{
		if(!strlen(inputtext)) return SendClientMessage(playerid, -1, "�� ������ �� �����");
		if(strval(inputtext) < 1) return SendClientMessage(playerid, -1, "������ ������� ������������� �����.");
		if(!IsPlayerConnected(GiveInfo[playerid][0])) return SendClientMessage(playerid, -1, "������ ����� �� ���������.");


		new SendStr[128], Names[MAX_PLAYER_NAME];
		GetPlayerName(playerid, Names, MAX_PLAYER_NAME);
		//printf("�������: %d | ��������: %d", GiveInfo[playerid][0], playerid);
		format(SendStr, sizeof(SendStr), "%s [%d] ���������� ������ %s [%d] �� $%d.\n�� ������ ������ ���?", Names, playerid, Items[PlayerItems[playerid][UseSlotItem[playerid]][0]][Name], GiveInfo[playerid][1], strval(inputtext));
		ShowPlayerDialog(GiveInfo[playerid][0], D_Items + 6, DIALOG_STYLE_MSGBOX, "������� ��������", SendStr, "��", "���");

		GiveInfo[GiveInfo[playerid][0]][0] = playerid; //�� ������
		GiveInfo[GiveInfo[playerid][0]][1] = GiveInfo[playerid][1]; //���-��
		GiveInfo[GiveInfo[playerid][0]][2] = strval(inputtext);//����
		GiveInfo[GiveInfo[playerid][0]][3] = UseSlotItem[playerid]; //���� � ������
	}
	else if(dialogid == D_Items + 6 && response)
	{
		if(!IsPlayerConnected(GiveInfo[playerid][0])) return SendClientMessage(playerid, -1, "������ ����� �� ���������.");
		//printf("�������: %d | ��������: %d", GiveInfo[playerid][0], playerid);
        SetItemTheID(playerid, PlayerItems[GiveInfo[playerid][0]][GiveInfo[playerid][3]][0], GiveInfo[playerid][1]);
		SetItemTheID(GiveInfo[playerid][0], PlayerItems[GiveInfo[playerid][0]][GiveInfo[playerid][3]][0], -GiveInfo[playerid][1]);


		new SendStr[128], Names[2][MAX_PLAYER_NAME];
		GetPlayerName(playerid, Names[0], MAX_PLAYER_NAME); //������� �����
		GetPlayerName(GiveInfo[playerid][0], Names[1], MAX_PLAYER_NAME); //������� ������

		format(SendStr, sizeof(SendStr), "�� ������� �������(�) ������ %s [%d] �� $%d.", Names[0], playerid, GiveInfo[playerid][2]);
		SendClientMessage(GiveInfo[playerid][0], -1, SendStr);
		format(SendStr, sizeof(SendStr), "��� ������� �������(�) ����� %s [%d] �� $%d.", Names[1], GiveInfo[playerid][0], GiveInfo[playerid][2]);
		SendClientMessage(playerid, -1, SendStr);
	}
	return 0;
}

stock AddItem(string[]) //�������� ������� � ������ ���������
{
	if(ItemID >= MAX_ITEMS) return printf("ERROR: �������� ����� ���������.");
	#if DEBUG
	printf("�������: %s | ID: %d", string, ItemID + 1);
	#endif
	format(Items[ItemID][Name], 30, "%s", string);
	ItemID++;
	return 1;
}

stock SetItemTheName(playerid, string[], Val) //�������� ���-�� ��������� ������ �� �������� ��������
{
	for(new i; i != ItemID; i++)
	{
		if(!strcmp(Items[i][Name], string, true))
		{
   			for(new f; f != ItemID; f++)
			{
			    if(PlayerItems[playerid][f][0] == i + 1 || PlayerItems[playerid][f][0] == 0)
			    {
			        PlayerItems[playerid][f][0] = i + 1;
			        PlayerItems[playerid][f][1] += Val;
					ShiftData(playerid, f);
			        return 1;
			    }
			}
		}
	}
	printf("������� %s - �� ����������!", string);
	return 1;
}

stock SetItemTheID(playerid, oID, Val) //�������� ���-�� ��������� ������ �� �� ��������
{
    if(oID < 1 || oID > ItemID) return printf("ERROR: ������ ������� �� ����������.");

	
	for(new f; f != ItemID; f++)
	{
 		if(PlayerItems[playerid][f][0] == oID || PlayerItems[playerid][f][0] == 0)
 		{
 			PlayerItems[playerid][f][0] = oID;
 			PlayerItems[playerid][f][1] += Val;
 			ShiftData(playerid, f);
 			break;
 		}
	}
	return 1;
}

stock ShiftData(playerid, ItS) //�������� ������ � ������ ��� ������ �����
{
	if(PlayerItems[playerid][ItS][1] != 0) return 1; //�������� ����, ����� �� �����

	PlayerItems[playerid][ItS][0] = 0;

	if(ItS == ItemID) return 1; //��������� �������, ����� �� �����
	for(new i = ItS; i != ItemID; i++)
	{
		PlayerItems[playerid][i][0] = PlayerItems[playerid][i + 1][0];
 		PlayerItems[playerid][i][1] = PlayerItems[playerid][i + 1][1];
	}
	return 1;
}

stock ResetItems(playerid) //�������� �������� ������
{
	for(new i = 0; i != ItemID; i++)
	{
		PlayerItems[playerid][i][0] = 0;
 		PlayerItems[playerid][i][1] = 0;
	}
}

stock LoadItems(playerid) //�������� ���������
{
	new PLName[MAX_PLAYER_NAME], FileAddr[MAX_PLAYER_NAME + 20], temp[2][30];
	GetPlayerName(playerid, PLName, MAX_PLAYER_NAME);
	format(FileAddr, sizeof(FileAddr), "Items/%s.ini", PLName);
	if(fexist(FileAddr))
 	{
		new file = ini_openFile(FileAddr);
		new Slot, val, UseSlot;
		for(new f; f != ItemID; f++)
		{
            format(temp[0], 30,"Item%d", f);
            ini_getString(file, temp[0], temp[1]);
            for(new j, l = strlen(temp[1]), pos, temps[5]; j != l; j++)
            {
                if(temp[1][j] == '|')
                {
                    pos = 0;
                    Slot = strval(temps);
                    for(new m; m != sizeof(temps); m++) temps[m] = '\0';
                    continue;
                }
                temps[pos] = temp[1][j];
                pos++;
                if(j == l - 1) val = strval(temps);
            }

            if(val != 0)
   			{
   			    printf("%i %i | %i", f, Slot, val);
				PlayerItems[playerid][UseSlot][0] = Slot;
    			PlayerItems[playerid][UseSlot][1] = val;
	      		UseSlot++;
   			}
   			val = 0;
		}
		ini_closeFile(file);
	}
	else
	{
		new file = ini_createFile(FileAddr);
		ini_setString(file, "Item0", "0|0");
		ini_closeFile(file);
	}
}

stock SaveItems(playerid) //���������� ���������
{
	new PLName[MAX_PLAYER_NAME], FileAddr[MAX_PLAYER_NAME + 20], temp[2][30];
	GetPlayerName(playerid, PLName, MAX_PLAYER_NAME);
	format(FileAddr, sizeof(FileAddr), "Items/%s.ini", PLName);
	new file = ini_openFile(FileAddr);
	for(new f; f != ItemID; f++)
	{
		format(temp[0], 30,"Item%d", f);
		format(temp[1], 30,"%d|%d", PlayerItems[playerid][f][0], PlayerItems[playerid][f][1]);
		ini_setString(file, temp[0], temp[1]);
	}
	ini_closeFile(file);
}

stock UseItem(playerid, itID) //������������� ���������
{
	switch(itID)
	{
	    case 1: //�������� ��� �������� 1
	    {
	        new Float:PH;
		    GetPlayerHealth(playerid, PH);
		    SetPlayerHealth(playerid, PH +10);
	    }
	    case 2: //�������� ��� �������� 2
	    {

	    }
	    //� ��� �����
	}
}
